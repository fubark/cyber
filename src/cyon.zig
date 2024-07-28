const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;

const cy = @import("cyber.zig");
const ast = cy.ast;
const Parser = cy.Parser;
const log = cy.log.scoped(.cdata);

pub const EncodeListContext = struct {
    writer: std.ArrayListUnmanaged(u8).Writer,
    tmp_buf: *std.ArrayList(u8),
    cur_indent: u32,
    user_ctx: ?*anyopaque,

    pub fn indent(self: *EncodeListContext) !void {
        try self.writer.writeByteNTimes(' ', self.cur_indent * 4);
    }

    pub fn encodeList(self: *EncodeListContext, val: anytype, cb: fn (*EncodeListContext, @TypeOf(val)) anyerror!void) !void {
        _ = try self.writer.write("{\n");

        var list_ctx = EncodeListContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try cb(&list_ctx, val);

        try self.indent();
        _ = try self.writer.write("}");
    }
    
    pub fn encodeTable(self: *EncodeListContext, val: anytype, encode_table: fn (*EncodeTableContext, @TypeOf(val)) anyerror!void) !void {
        _ = try self.writer.write("{\n");

        var map_ctx = EncodeTableContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_table(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("}");
    }

    pub fn encodeBool(self: *EncodeListContext, b: bool) !void {
        try Common.encodeBool(self.writer, b);
    }

    pub fn encodeFloat(self: *EncodeListContext, f: f64) !void {
        try Common.encodeFloat(self.writer, f);
    }

    pub fn encodeInt(self: *EncodeListContext, i: i64) !void {
        try Common.encodeInt(self.writer, i);
    }

    pub fn encodeString(self: *EncodeListContext, str: []const u8) !void {
        try Common.encodeString(self.tmp_buf, self.writer, str);
    }
};

/// TODO: Rename to EncodeRootContext
pub const EncodeValueContext = struct {
    writer: std.ArrayListUnmanaged(u8).Writer,
    tmp_buf: *std.ArrayList(u8),
    cur_indent: u32,
    user_ctx: ?*anyopaque,

    fn indent(self: *EncodeValueContext) !void {
        try self.writer.writeByteNTimes(' ', self.cur_indent * 4);
    }

    pub fn encodeList(self: *EncodeValueContext, val: anytype, encode_list: fn (*EncodeListContext, @TypeOf(val)) anyerror!void) !void {
        _ = try self.writer.print("{{\n", .{});

        var list_ctx = EncodeListContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_list(&list_ctx, val);

        try self.indent();
        _ = try self.writer.write("}");
    }

    pub fn encodeTable(self: *EncodeValueContext, val: anytype, encode_table: fn (*EncodeTableContext, @TypeOf(val)) anyerror!void) !void {
        _ = try self.writer.write("{\n");

        var table_ctx = EncodeTableContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_table(&table_ctx, val);

        try self.indent();
        _ = try self.writer.write("}");
    }

    pub fn encodeBool(self: *EncodeValueContext, b: bool) !void {
        try Common.encodeBool(self.writer, b);
    }

    pub fn encodeFloat(self: *EncodeValueContext, f: f64) !void {
        try Common.encodeFloat(self.writer, f);
    }

    pub fn encodeInt(self: *EncodeValueContext, i: i64) !void {
        try Common.encodeInt(self.writer, i);
    }

    pub fn encodeString(self: *EncodeValueContext, str: []const u8) !void {
        try Common.encodeString(self.tmp_buf, self.writer, str);
    }
};

pub const EncodeTableContext = struct {
    writer: std.ArrayListUnmanaged(u8).Writer,
    tmp_buf: *std.ArrayList(u8),
    cur_indent: u32,
    user_ctx: ?*anyopaque,

    fn indent(self: *EncodeTableContext) !void {
        try self.writer.writeByteNTimes(' ', self.cur_indent * 4);
    }

    pub fn encodeSlice(self: *EncodeTableContext, key: []const u8, slice: anytype, encode_value: fn (*EncodeValueContext, anytype) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s} = {{\n", .{key});

        var val_ctx = EncodeValueContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        self.cur_indent += 1;
        for (slice) |it| {
            try self.indent();
            try encode_value(&val_ctx, it);
            _ = try self.writer.write(",\n");
        }
        self.cur_indent -= 1;

        try self.indent();
        _ = try self.writer.write("},\n");
    }

    pub fn encodeList(self: *EncodeTableContext, key: []const u8, val: anytype, encode_list: fn (*EncodeListContext, @TypeOf(val)) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s} = {{\n", .{key});

        var list_ctx = EncodeListContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_list(&list_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }

    pub fn encodeTable(self: *EncodeTableContext, key: []const u8, val: anytype, encode_table: fn (*EncodeTableContext, @TypeOf(val)) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s} = {{\n", .{key});

        var map_ctx = EncodeTableContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_table(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("},\n");
    }

    pub fn encodeTable2(self: *EncodeTableContext, key: []const u8, val: anytype, encode_table: fn (*EncodeTableContext, anytype) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s} = ", .{ key });

        _ = try self.writer.write("{\n");

        var table_ctx = EncodeTableContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_table(&table_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }

    pub fn encode(self: *EncodeTableContext, key: []const u8, val: anytype) !void {
        try self.indent();
        _ = try self.writer.print("{s} = ", .{key});
        try self.encodeValue(val);
        _ = try self.writer.write(",\n");
    }

    pub fn encodeString(self: *EncodeTableContext, key: []const u8, val: []const u8) !void {
        try self.indent();
        _ = try self.writer.print("{s} = ", .{key});
        try Common.encodeString(self.tmp_buf, self.writer, val);
        _ = try self.writer.write(",\n");
    }

    pub fn encodeInt(self: *EncodeTableContext, key: []const u8, i: i64) !void {
        try self.indent();
        _ = try self.writer.print("{s} = ", .{key});
        try Common.encodeInt(self.writer, i);
        _ = try self.writer.write(",\n");
    }

    pub fn encodeFloat(self: *EncodeTableContext, key: []const u8, f: f64) !void {
        try self.indent();
        _ = try self.writer.print("{s} = ", .{key});
        try Common.encodeFloat(self.writer, f);
        _ = try self.writer.write(",\n");
    }

    pub fn encodeBool(self: *EncodeTableContext, key: []const u8, b: bool) !void {
        try self.indent();
        _ = try self.writer.print("{s} = ", .{key});
        try Common.encodeBool(self.writer, b);
        _ = try self.writer.write(",\n");
    }

    pub fn encodeAnyToTable(self: *EncodeTableContext, key: anytype, val: anytype, encode_table: fn (*EncodeTableContext, @TypeOf(val)) anyerror!void) !void {
        try self.encodeAnyKey_(key);
        _ = try self.writer.write("{\n");

        var table_ctx = EncodeTableContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_table(&table_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }

    fn encodeAnyKey_(self: *EncodeTableContext, key: anytype) !void {
        try self.indent();
        const T = @TypeOf(key);
        switch (T) {
            // Don't support string types since there can be many variations. Use `encode` instead.
            u32 => {
                _ = try self.writer.print("{} = ", .{key});
            },
            else => {
                log.tracev("unsupported: {s}", .{@typeName(T)});
                return error.Unsupported;
            },
        }
    }

    pub fn encodeAnyToString(self: *EncodeTableContext, key: anytype, val: []const u8) !void {
        try self.encodeAnyKey_(key);
        try Common.encodeString(self.tmp_buf, self.writer, val);
        _ = try self.writer.write(",\n");
    }

    pub fn encodeAnyToValue(self: *EncodeTableContext, key: anytype, val: anytype) !void {
        try self.encodeAnyKey_(key);
        try self.encodeValue(val);
        _ = try self.writer.write(",\n");
    }

    fn encodeValue(self: *EncodeTableContext, val: anytype) !void {
        const T = @TypeOf(val);
        switch (T) {
            bool,
            u32 => {
                _ = try self.writer.print("{}", .{val});
            },
            else => {
                @compileError("unsupported: " ++ @typeName(T));
            },
        }
    }
};

const Common = struct {
    fn encodeString(tmpBuf: *std.ArrayList(u8), writer: anytype, str: []const u8) !void {
        tmpBuf.clearRetainingCapacity();
        if (std.mem.indexOfScalar(u8, str, '\n') == null) {
            _ = replaceIntoList(u8, str, "'", "\\'", tmpBuf);
            _ = try writer.print("'{s}'", .{tmpBuf.items});
        } else {
            _ = replaceIntoList(u8, str, "`", "\\`", tmpBuf);
            _ = try writer.print("`{s}`", .{tmpBuf.items});
        }
    }

    fn encodeBool(writer: anytype, b: bool) !void {
        _ = try writer.print("{}", .{b});
    }

    fn encodeInt(writer: anytype, i: i64) !void {
        try writer.print("{}", .{i});
    }

    fn encodeFloat(writer: anytype, f: f64) !void {
        if (cy.Value.floatCanBeInteger(f)) {
            try writer.print("{d:.0}.0", .{f});
        } else {
            try writer.print("{d}", .{f});
        }
    }
};

pub fn encode(alloc: std.mem.Allocator, user_ctx: ?*anyopaque, val: anytype, encode_value: fn (*EncodeValueContext, anytype) anyerror!void) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    var tmp_buf = std.ArrayList(u8).init(alloc);
    defer tmp_buf.deinit();
    var val_ctx = EncodeValueContext{
        .writer = buf.writer(alloc),
        .cur_indent = 0,
        .user_ctx = user_ctx,
        .tmp_buf = &tmp_buf,
    };
    try encode_value(&val_ctx, val);
    return buf.toOwnedSlice(alloc);
}

pub const DecodeListIR = struct {
    alloc: std.mem.Allocator,
    ast: cy.ast.AstView,
    arr: []const *ast.Node,

    fn init(alloc: std.mem.Allocator, view: cy.ast.AstView, list_: *ast.Node) !DecodeListIR {
        if (list_.type() != .init_lit) {
            return error.NotAList;
        }

        const list = list_.cast(.init_lit);

        var new = DecodeListIR{
            .alloc = alloc,
            .ast = view,
            .arr = &.{},
        };

        // Construct list.
        var buf: std.ArrayListUnmanaged(*ast.Node) = .{};
        if (list.args.len > 0) {
            for (list.args) |arg| {
                try buf.append(alloc, arg);
            }
        }
        new.arr = try buf.toOwnedSlice(alloc);
        return new;
    }

    pub fn deinit(self: *DecodeListIR) void {
        self.alloc.free(self.arr);
    }

    pub fn getIndex(self: DecodeListIR, idx: usize) DecodeValueIR {
        return DecodeValueIR{
            .alloc = self.alloc,
            .ast = self.ast,
            .expr = self.arr[idx],
        };
    }

    pub fn decodeTable(self: DecodeListIR, idx: u32) !DecodeTableIR {
        if (idx < self.arr.len) {
            return try DecodeTableIR.init(self.alloc, self.ast, self.arr[idx]);
        } else return error.NoSuchEntry;
    }
};

pub const DecodeTableIR = struct {
    alloc: std.mem.Allocator,
    ast: cy.ast.AstView,

    /// Preserve order of entries.
    map: std.StringArrayHashMapUnmanaged(*ast.Node),

    fn init(alloc: std.mem.Allocator, view: cy.ast.AstView, map: *ast.Node) !DecodeTableIR {
        if (map.type() != .init_lit) {
            return error.NotAMap;
        }

        var new = DecodeTableIR{
            .alloc = alloc,
            .ast = view,
            .map = .{},
        };

        // Parse literal into map.
        for (map.cast(.init_lit).args) |entry| {
            const pair = entry.cast(.keyValue);
            switch (pair.key.type()) {
                .binLit,
                .octLit,
                .hexLit,
                .decLit,
                .floatLit,
                .ident => {
                    const str = view.nodeString(pair.key);
                    try new.map.put(alloc, str, pair.value);
                },
                else => return error.Unsupported,
            }
        }
        return new;
    }

    pub fn deinit(self: *DecodeTableIR) void {
        self.map.deinit(self.alloc);
    }

    pub fn iterator(self: DecodeTableIR) std.StringArrayHashMapUnmanaged(*ast.Node).Iterator {
        return self.map.iterator();
    }

    pub fn getValue(self: DecodeTableIR, key: []const u8) DecodeValueIR {
        return DecodeValueIR{
            .alloc = self.alloc,
            .ast = self.ast,
            .expr = self.map.get(key).?,
        };
    }
    
    pub fn allocString(self: DecodeTableIR, key: []const u8) ![]const u8 {
        if (self.map.get(key)) |val| {
            if (val.type() == .raw_string_lit) {
                const token_s = self.ast.nodeString(val);
                return try self.alloc.dupe(u8, token_s);
            } else if (val.type() == .stringLit) {
                const token_s = self.ast.nodeString(val);
                var buf = std.ArrayList(u8).init(self.alloc);
                defer buf.deinit();

                try buf.resize(token_s.len);
                const str = try cy.unescapeString(buf.items, token_s, true);
                buf.items.len = str.len;
                return buf.toOwnedSlice();
            } else if (val.type() == .stringTemplate) {
                const parts = val.cast(.stringTemplate).parts;
                if (parts.len == 1) {
                    const token_s = self.ast.nodeString(parts[0]);
                    var buf = std.ArrayList(u8).init(self.alloc);
                    defer buf.deinit();
                    _ = replaceIntoList(u8, token_s, "\\`", "`", &buf);
                    return buf.toOwnedSlice();
                }
            }
            return error.NotAString;
        } else return error.NoSuchEntry;
    }

    pub fn getU32(self: DecodeTableIR, key: []const u8) !u32 {
        if (self.map.get(key)) |val| {
            switch (val.type()) {
                .binLit => {
                    const str = self.ast.nodeString(val)[2..];
                    return try std.fmt.parseInt(u32, str, 2);
                },
                .octLit => {
                    const str = self.ast.nodeString(val)[2..];
                    return try std.fmt.parseInt(u32, str, 8);
                },
                .hexLit => {
                    const str = self.ast.nodeString(val)[2..];
                    return try std.fmt.parseInt(u32, str, 16);
                },
                .decLit => {
                    const str = self.ast.nodeString(val);
                    return try std.fmt.parseInt(u32, str, 10);
                },
                else => return error.NotANumber,
            }
        } else return error.NoSuchEntry;
    }

    pub fn getBool(self: DecodeTableIR, key: []const u8) !bool {
        return self.getBoolOpt(key) orelse return error.NoSuchEntry;
    }

    pub fn getBoolOpt(self: DecodeTableIR, key: []const u8) !?bool {
        if (self.map.get(key)) |val_id| {
            const val_n = self.ast.node(val_id);
            if (val_n.type() == .trueLit) {
                return true;
            } else if (val_n.type() == .falseLit) {
                return false;
            } else return error.NotABool;
        } else return null;
    }

    pub fn decodeList(self: DecodeTableIR, key: []const u8) !DecodeListIR {
        if (self.map.get(key)) |val_id| {
            return DecodeListIR.init(self.alloc, self.ast, val_id);
        } else return error.NoSuchEntry;
    }

    pub fn decodeTable(self: DecodeTableIR, key: []const u8) !DecodeTableIR {
        if (self.map.get(key)) |val_id| {
            return try DecodeTableIR.init(self.alloc, self.ast, val_id);
        } else return error.NoSuchEntry;
    }
};

// Currently uses Cyber parser.
pub fn decodeTable(alloc: std.mem.Allocator, parser: *Parser, ctx: anytype, out: anytype, decode_map: fn (DecodeTableIR, @TypeOf(ctx), @TypeOf(out)) anyerror!void, cdata: []const u8) !void {
    const res = try parser.parse(cdata, .{});
    if (res.has_error) {
        return error.ParseError;
    }

    const root = res.root orelse {
        return error.NotAMap;
    };
    const first_stmt = root.stmts[0];
    if (first_stmt.type() != .exprStmt) {
        return error.NotAMap;
    }

    var map = try DecodeTableIR.init(alloc, res.ast, first_stmt.cast(.exprStmt).child);
    defer map.deinit();
    try decode_map(map, ctx, out);
}

pub fn decode(alloc: std.mem.Allocator, parser: *Parser, cyon: []const u8) !DecodeValueIR {
    const res = try parser.parse(cyon, .{});
    if (res.has_error) {
        return error.ParseError;
    }

    const root = res.root orelse {
        return error.NotAValue;
    };
    const first_stmt = root.stmts[0];
    if (first_stmt.type() != .exprStmt) {
        return error.NotAValue;
    }

    return DecodeValueIR{
        .alloc = alloc, 
        .ast = res.ast,
        .expr = first_stmt.cast(.exprStmt).child,
    };
}

const ValueType = enum {
    list,
    map,
    string,
    integer,
    float,
    bool,
};

pub const DecodeValueIR = struct {
    alloc: std.mem.Allocator,
    ast: cy.ast.AstView,
    expr: *ast.Node,

    pub fn getValueType(self: DecodeValueIR) ValueType {
        switch (self.expr.type()) {
            .init_lit => {
                if (self.expr.cast(.init_lit).array_like) {
                    return .list;
                } else {
                    return .map;
                }
            },
            .raw_string_lit,
            .stringLit => return .string,
            .hexLit,
            .binLit,
            .octLit,
            .decLit => return .integer,
            .floatLit => return .float,
            .trueLit => return .bool,
            .falseLit => return .bool,
            else => cy.panicFmt("unsupported {}", .{self.expr.type()}),
        }
    }

    pub fn getList(self: DecodeValueIR) !DecodeListIR {
        return DecodeListIR.init(self.alloc, self.ast, self.expr);
    }

    pub fn getMap(self: DecodeValueIR) !DecodeTableIR {
        return DecodeTableIR.init(self.alloc, self.ast, self.expr);
    }

    pub fn allocString(self: DecodeValueIR) ![]u8 {
        const token_s = self.ast.nodeString(self.expr);
        var buf = std.ArrayList(u8).init(self.alloc);
        defer buf.deinit();
        try buf.resize(token_s.len);
        const str = try cy.unescapeString(buf.items, token_s, true);
        buf.items.len = str.len;
        return try buf.toOwnedSlice();
    }

    pub fn getF64(self: DecodeValueIR) !f64 {
        const token_s = self.ast.nodeString(self.expr);
        return try std.fmt.parseFloat(f64, token_s);
    }

    pub fn getInt(self: DecodeValueIR) !i64 {
        const token_s = self.ast.nodeString(self.expr);
        return try std.fmt.parseInt(i64, token_s, 10);
    }

    pub fn getBool(self: DecodeValueIR) bool {
        if (self.expr.type() == .trueLit) {
            return true;
        } else if (self.expr.type() == .falseLit) {
            return false;
        } else {
            cy.panicFmt("Unsupported type: {}", .{self.expr.type()});
        }
    }
};

const TestRoot = struct {
    name: []const u8,
    list: []const TestListItem,
    table: []const TestTableItem,
};

const TestListItem = struct {
    field: u32,
};

const TestTableItem = struct {
    id: u32,
    val: []const u8,
};

test "encode" {
    const root = TestRoot{
        .name = "project",
        .list = &.{
            .{ .field = 1 },
            .{ .field = 2 },
        },
        .table = &.{
            .{ .id = 1, .val = "foo" },
            .{ .id = 2, .val = "bar" },
            .{ .id = 3, .val = "ba'r" },
            .{ .id = 4, .val = "bar\nbar" },
            .{ .id = 5, .val = "bar `bar`\nbar" },
        },
    };

    const S = struct {
        fn encodeRoot(ctx: *EncodeTableContext, val: TestRoot) anyerror!void {
            try ctx.encodeString("name", val.name);
            try ctx.encodeSlice("list", val.list, encodeValue);
            try ctx.encodeTable("table", val.table, encodeTable);
        }
        fn encodeTable(ctx: *EncodeTableContext, val: []const TestTableItem) anyerror!void {
            for (val) |it| {
                try ctx.encodeAnyToString(it.id, it.val);
            }
        }
        fn encodeItem(ctx: *EncodeTableContext, val: TestListItem) anyerror!void {
            try ctx.encode("field", val.field);
        }
        fn encodeValue(ctx: *EncodeValueContext, val: anytype) !void {
            const T = @TypeOf(val);
            if (T == TestRoot) {
                try ctx.encodeTable(val, encodeRoot);
            } else if (T == TestListItem) {
                try ctx.encodeTable(val, encodeItem);
            } else {
                cy.panicFmt("unsupported: {s}", .{@typeName(T)});
            }
        }
    };

    const res = try encode(t.alloc, null, root, S.encodeValue);
    defer t.alloc.free(res);
    try t.eqStr(res,
        \\{
        \\    name = 'project',
        \\    list = {
        \\        {
        \\            field = 1,
        \\        },
        \\        {
        \\            field = 2,
        \\        },
        \\    },
        \\    table = {
        \\        1 = 'foo',
        \\        2 = 'bar',
        \\        3 = 'ba\'r',
        \\        4 = `bar
        \\bar`,
        \\        5 = `bar \`bar\`
        \\bar`,
        \\    },
        \\}
    );
}

test "decodeTable" {
    const S = struct {
        fn decodeRoot(map: DecodeTableIR, _: void, root: *TestRoot) anyerror!void {
            root.name = try map.allocString("name");

            var list: std.ArrayListUnmanaged(TestListItem) = .{};
            var list_ir = try map.decodeList("list");
            defer list_ir.deinit();
            var i: u32 = 0;
            while (i < list_ir.arr.len) : (i += 1) {
                var item: TestListItem = undefined;
                var item_map = try list_ir.decodeTable(i);
                defer item_map.deinit();
                item.field = try item_map.getU32("field");
                try list.append(t.alloc, item);
            }
            root.list = try list.toOwnedSlice(t.alloc);

            var table_items: std.ArrayListUnmanaged(TestTableItem) = .{};
            var table_ = try map.decodeTable("table");
            defer table_.deinit();
            var iter = table_.iterator();
            while (iter.next()) |entry| {
                const key = try std.fmt.parseInt(u32, entry.key_ptr.*, 10);
                const value = try table_.allocString(entry.key_ptr.*);
                try table_items.append(t.alloc, .{ .id = key, .val = value });
            }
            root.table = try table_items.toOwnedSlice(t.alloc);
        }
    };

    var parser: Parser = undefined;
    try parser.init(t.alloc);
    defer parser.deinit();

    var root: TestRoot = undefined;
    try decodeTable(t.alloc, &parser, {}, &root, S.decodeRoot, 
        \\{
        \\    name = 'project',
        \\    list = {
        \\        {field=1},
        \\        {field=2},
        \\    },
        \\    table = {
        \\        1 = 'foo',
        \\        2 = 'bar',
        \\        3 = "ba\"r",
        \\        4 = """ba"r""",
        \\        5 = """bar `bar`
        \\bar"""
        \\    }
        \\}
    );
    defer {
        t.alloc.free(root.list);
        t.alloc.free(root.name);
        for (root.table) |it| {
            t.alloc.free(it.val);
        }
        t.alloc.free(root.table);
    }

    try t.eqStr(root.name, "project");
    try t.eq(root.list[0].field, 1);
    try t.eq(root.list[1].field, 2);
    try t.eq(root.table.len, 5);
    try t.eq(root.table[0].id, 1);
    try t.eqStr(root.table[0].val, "foo");
    try t.eq(root.table[1].id, 2);
    try t.eqStr(root.table[1].val, "bar");
    try t.eq(root.table[2].id, 3);
    try t.eqStr(root.table[2].val, "ba\"r");
    try t.eq(root.table[3].id, 4);
    try t.eqStr(root.table[3].val, "ba\"r");
    try t.eq(root.table[4].id, 5);
    try t.eqStr(root.table[4].val, "bar `bar`\nbar");
}

// Same as std.mem.replace except we write to an ArrayList.
pub fn replaceIntoList(comptime T: type, input: []const T, needle: []const T, replacement: []const T, output: *std.ArrayList(T)) usize {
    // Clear the array list.
    output.clearRetainingCapacity();
    var i: usize = 0;
    var slide: usize = 0;
    var replacements: usize = 0;
    while (slide < input.len) {
        if (std.mem.indexOf(T, input[slide..], needle) == @as(usize, 0)) {
            output.appendSlice(replacement) catch unreachable;
            i += replacement.len;
            slide += needle.len;
            replacements += 1;
        } else {
            output.append(input[slide]) catch unreachable;
            i += 1;
            slide += 1;
        }
    }
    return replacements;
}