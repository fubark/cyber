const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;

const cy = @import("cyber.zig");
const NodeId = cy.NodeId;
const Parser = cy.Parser;
const log = stdx.log.scoped(.cdata);

pub const EncodeListContext = struct {
    writer: std.ArrayListUnmanaged(u8).Writer,
    tmp_buf: *std.ArrayList(u8),
    cur_indent: u32,
    user_ctx: ?*anyopaque,

    fn indent(self: *EncodeListContext) !void {
        try self.writer.writeByteNTimes(' ', self.cur_indent * 4);
    }
    
    pub fn encodeMap(self: *EncodeListContext, val: anytype, encode_map: fn (*EncodeMapContext, @TypeOf(val)) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.write("{\n");

        var map_ctx = EncodeMapContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_map(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }
};

pub const EncodeValueContext = struct {
    writer: std.ArrayListUnmanaged(u8).Writer,
    tmp_buf: *std.ArrayList(u8),
    cur_indent: u32,
    user_ctx: ?*anyopaque,

    fn indent(self: *EncodeValueContext) !void {
        try self.writer.writeByteNTimes(' ', self.cur_indent * 4);
    }

    pub fn encodeAsMap(self: *EncodeValueContext, val: anytype, encode_map: fn (*EncodeMapContext, @TypeOf(val)) anyerror!void) !void {
        _ = try self.writer.write("{\n");

        var map_ctx = EncodeMapContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_map(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("}");
    }
};

pub const EncodeMapContext = struct {
    writer: std.ArrayListUnmanaged(u8).Writer,
    tmp_buf: *std.ArrayList(u8),
    cur_indent: u32,
    user_ctx: ?*anyopaque,

    fn indent(self: *EncodeMapContext) !void {
        try self.writer.writeByteNTimes(' ', self.cur_indent * 4);
    }

    pub fn encodeSlice(self: *EncodeMapContext, key: []const u8, slice: anytype, encode_value: fn (*EncodeValueContext, anytype) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s}: [\n", .{key});

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
            _ = try self.writer.write("\n");
        }
        self.cur_indent -= 1;

        try self.indent();
        _ = try self.writer.write("]\n");
    }

    pub fn encodeAsList(self: *EncodeMapContext, key: []const u8, val: anytype, encode_list: fn (*EncodeListContext, @TypeOf(val)) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s}: [\n", .{key});

        var list_ctx = EncodeListContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_list(&list_ctx, val);

        try self.indent();
        _ = try self.writer.write("]\n");
    }

    pub fn encodeAsMap(self: *EncodeMapContext, key: []const u8, val: anytype, encode_map: fn (*EncodeMapContext, @TypeOf(val)) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s}: {{\n", .{key});

        var map_ctx = EncodeMapContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_map(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }

    pub fn encodeAsMap2(self: *EncodeMapContext, key: []const u8, val: anytype, encode_map: fn (*EncodeMapContext, anytype) anyerror!void) !void {
        try self.indent();
        _ = try self.writer.print("{s}: ", .{ key });

        _ = try self.writer.write("{\n");

        var map_ctx = EncodeMapContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_map(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }

    pub fn encode(self: *EncodeMapContext, key: []const u8, val: anytype) !void {
        try self.indent();
        _ = try self.writer.print("{s}: ", .{key});
        try self.encodeValue(val);
    }

    pub fn encodeString(self: *EncodeMapContext, key: []const u8, val: []const u8) !void {
        try self.indent();
        _ = try self.writer.print("{s}: ", .{key});
        try self.encodeString_(val);
    }

    pub fn encodeAnyToMap(self: *EncodeMapContext, key: anytype, val: anytype, encode_map: fn (*EncodeMapContext, @TypeOf(val)) anyerror!void) !void {
        try self.encodeAnyKey_(key);
        _ = try self.writer.write("{\n");

        var map_ctx = EncodeMapContext{
            .writer = self.writer,
            .tmp_buf = self.tmp_buf,
            .cur_indent = self.cur_indent + 1,
            .user_ctx = self.user_ctx,
        };
        try encode_map(&map_ctx, val);

        try self.indent();
        _ = try self.writer.write("}\n");
    }

    fn encodeAnyKey_(self: *EncodeMapContext, key: anytype) !void {
        try self.indent();
        const T = @TypeOf(key);
        switch (T) {
            // Don't support string types since there can be many variations. Use `encode` instead.
            u32 => {
                _ = try self.writer.print("{}: ", .{key});
            },
            else => {
                log.debug("unsupported: {s}", .{@typeName(T)});
                return error.Unsupported;
            },
        }
    }

    pub fn encodeAnyToString(self: *EncodeMapContext, key: anytype, val: []const u8) !void {
        try self.encodeAnyKey_(key);
        try self.encodeString_(val);
    }

    fn encodeString_(self:* EncodeMapContext, str: []const u8) !void {
        self.tmp_buf.clearRetainingCapacity();
        if (std.mem.indexOfScalar(u8, str, '\n') == null) {
            _ = replaceIntoList(u8, str, "'", "\\'", self.tmp_buf);
            _ = try self.writer.print("'{s}'\n", .{self.tmp_buf.items});
        } else {
            _ = replaceIntoList(u8, str, "`", "\\`", self.tmp_buf);
            _ = try self.writer.print("`{s}`\n", .{self.tmp_buf.items});
        }
    }

    pub fn encodeAnyToValue(self: *EncodeMapContext, key: anytype, val: anytype) !void {
        try self.encodeAnyKey_(key);
        try self.encodeValue(val);
    }

    fn encodeValue(self: *EncodeMapContext, val: anytype) !void {
        const T = @TypeOf(val);
        switch (T) {
            bool,
            u32 => {
                _ = try self.writer.print("{}\n", .{val});
            },
            else => {
                @compileError("unsupported: " ++ @typeName(T));
            },
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
    res: cy.ParseResultView,
    arr: []const NodeId,

    fn init(alloc: std.mem.Allocator, res: cy.ParseResultView, list_id: NodeId) !DecodeListIR {
        const list = res.nodes.items[list_id];
        if (list.node_t != .arr_literal) {
            return error.NotAList;
        }

        var new = DecodeListIR{
            .alloc = alloc,
            .res = res,
            .arr = &.{},
        };

        // Construct list.
        var buf: std.ArrayListUnmanaged(NodeId) = .{};
        var item_id = list.head.child_head;
        while (item_id != NullId) {
            const item = res.nodes.items[item_id];
            try buf.append(alloc, item_id);
            item_id = item.next;
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
            .res = self.res,
            .exprId = self.arr[idx],
        };
    }

    pub fn decodeMap(self: DecodeListIR, idx: u32) !DecodeMapIR {
        if (idx < self.arr.len) {
            return try DecodeMapIR.init(self.alloc, self.res, self.arr[idx]);
        } else return error.NoSuchEntry;
    }
};

pub const DecodeMapIR = struct {
    alloc: std.mem.Allocator,
    res: cy.ParseResultView,

    /// Preserve order of entries.
    map: std.StringArrayHashMapUnmanaged(NodeId),

    fn init(alloc: std.mem.Allocator, res: cy.ParseResultView, map_id: NodeId) !DecodeMapIR {
        const map = res.nodes.items[map_id];
        if (map.node_t != .map_literal) {
            return error.NotAMap;
        }

        var new = DecodeMapIR{
            .alloc = alloc,
            .res = res,
            .map = .{},
        };

        // Parse literal into map.
        var entry_id = map.head.child_head;
        while (entry_id != NullId) {
            const entry = res.nodes.items[entry_id];
            const key = res.nodes.items[entry.head.left_right.left];
            switch (key.node_t) {
                .number,
                .ident => {
                    const str = res.getTokenString(key.start_token);
                    try new.map.put(alloc, str, entry.head.left_right.right);
                },
                else => return error.Unsupported,
            }
            entry_id = entry.next;
        }
        return new;
    }

    pub fn deinit(self: *DecodeMapIR) void {
        self.map.deinit(self.alloc);
    }

    pub fn iterator(self: DecodeMapIR) std.StringArrayHashMapUnmanaged(NodeId).Iterator {
        return self.map.iterator();
    }

    pub fn getValue(self: DecodeMapIR, key: []const u8) DecodeValueIR {
        return DecodeValueIR{
            .alloc = self.alloc,
            .res = self.res,
            .exprId = self.map.get(key).?,
        };
    }
    
    pub fn allocString(self: DecodeMapIR, key: []const u8) ![]const u8 {
        if (self.map.get(key)) |val_id| {
            const val_n = self.res.nodes.items[val_id];
            if (val_n.node_t == .string) {
                const token_s = self.res.getTokenString(val_n.start_token);
                var buf = std.ArrayList(u8).init(self.alloc);
                defer buf.deinit();

                try buf.resize(token_s.len);
                const str = cy.unescapeString(buf.items, token_s);
                buf.items.len = str.len;
                return buf.toOwnedSlice();
            } else if (val_n.node_t == .stringTemplate) {
                const str = self.res.nodes.items[val_n.head.stringTemplate.partsHead];
                if (str.next == NullId) {
                    const token_s = self.res.getTokenString(str.start_token);
                    var buf = std.ArrayList(u8).init(self.alloc);
                    defer buf.deinit();
                    _ = replaceIntoList(u8, token_s, "\\`", "`", &buf);
                    return buf.toOwnedSlice();
                }
            }
            return error.NotAString;
        } else return error.NoSuchEntry;
    }

    pub fn getU32(self: DecodeMapIR, key: []const u8) !u32 {
        if (self.map.get(key)) |val_id| {
            const val_n = self.res.nodes.items[val_id];
            if (val_n.node_t == .number) {
                const token_s = self.res.getTokenString(val_n.start_token);
                return try std.fmt.parseInt(u32, token_s, 10);
            } else return error.NotANumber;
        } else return error.NoSuchEntry;
    }

    pub fn getBool(self: DecodeMapIR, key: []const u8) !bool {
        return self.getBoolOpt(key) orelse return error.NoSuchEntry;
    }

    pub fn getBoolOpt(self: DecodeMapIR, key: []const u8) !?bool {
        if (self.map.get(key)) |val_id| {
            const val_n = self.res.nodes.items[val_id];
            if (val_n.node_t == .true_literal) {
                return true;
            } else if (val_n.node_t == .false_literal) {
                return false;
            } else return error.NotABool;
        } else return null;
    }

    pub fn decodeList(self: DecodeMapIR, key: []const u8) !DecodeListIR {
        if (self.map.get(key)) |val_id| {
            return DecodeListIR.init(self.alloc, self.res, val_id);
        } else return error.NoSuchEntry;
    }

    pub fn decodeMap(self: DecodeMapIR, key: []const u8) !DecodeMapIR {
        if (self.map.get(key)) |val_id| {
            return try DecodeMapIR.init(self.alloc, self.res, val_id);
        } else return error.NoSuchEntry;
    }
};

const NullId = std.math.maxInt(u32);

// Currently uses Cyber parser.
pub fn decodeMap(alloc: std.mem.Allocator, parser: *Parser, ctx: anytype, out: anytype, decode_map: fn (DecodeMapIR, @TypeOf(ctx), @TypeOf(out)) anyerror!void, cdata: []const u8) !void {
    const res = try parser.parse(cdata);
    if (res.has_error) {
        log.debug("Parse Error: {s}", .{res.err_msg});
        return error.ParseError;
    }

    const root = res.nodes.items[res.root_id];
    if (root.head.child_head == NullId) {
        return error.NotAMap;
    }
    const first_stmt = res.nodes.items[root.head.child_head];
    if (first_stmt.node_t != .expr_stmt) {
        return error.NotAMap;
    }

    var map = try DecodeMapIR.init(alloc, res, first_stmt.head.child_head);
    defer map.deinit();
    try decode_map(map, ctx, out);
}

pub fn decode(alloc: std.mem.Allocator, parser: *Parser, cyon: []const u8) !DecodeValueIR {
    const res = try parser.parse(cyon);
    if (res.has_error) {
        log.debug("Parse Error: {s}", .{res.err_msg});
        return error.ParseError;
    }

    const root = res.nodes.items[res.root_id];
    if (root.head.child_head == NullId) {
        return error.NotAValue;
    }
    const first_stmt = res.nodes.items[root.head.child_head];
    if (first_stmt.node_t != .expr_stmt) {
        return error.NotAValue;
    }

    return DecodeValueIR{
        .alloc = alloc, 
        .res = res,
        .exprId = first_stmt.head.child_head,
    };
}

const ValueType = enum {
    list,
    map,
    string,
    number,
};

pub const DecodeValueIR = struct {
    alloc: std.mem.Allocator,
    res: cy.ParseResultView,
    exprId: NodeId,

    pub fn getValueType(self: DecodeValueIR) ValueType {
        const node = self.res.nodes.items[self.exprId];
        switch (node.node_t) {
            .arr_literal => return .list,
            .map_literal => return .map,
            .string => return .string,
            .number => return .number,
            else => stdx.panicFmt("unsupported {}", .{node.node_t}),
        }
    }

    pub fn asList(self: DecodeValueIR) !DecodeListIR {
        return DecodeListIR.init(self.alloc, self.res, self.exprId);
    }

    pub fn asMap(self: DecodeValueIR) !DecodeMapIR {
        return DecodeMapIR.init(self.alloc, self.res, self.exprId);
    }

    pub fn allocString(self: DecodeValueIR) ![]u8 {
        const node = self.res.nodes.items[self.exprId];
        const token_s = self.res.getTokenString(node.start_token);
        var buf = std.ArrayList(u8).init(self.alloc);
        defer buf.deinit();
        try buf.resize(token_s.len);
        const str = cy.unescapeString(buf.items, token_s);
        buf.items.len = str.len;
        return try buf.toOwnedSlice();
    }

    pub fn asF64(self: DecodeValueIR) !f64 {
        const node = self.res.nodes.items[self.exprId];
        const token_s = self.res.getTokenString(node.start_token);
        return try std.fmt.parseFloat(f64, token_s);
    }
};

const TestRoot = struct {
    name: []const u8,
    list: []const TestListItem,
    map: []const TestMapItem,
};

const TestListItem = struct {
    field: u32,
};

const TestMapItem = struct {
    id: u32,
    val: []const u8,
};

test "encode" {
    var root = TestRoot{
        .name = "project",
        .list = &.{
            .{ .field = 1 },
            .{ .field = 2 },
        },
        .map = &.{
            .{ .id = 1, .val = "foo" },
            .{ .id = 2, .val = "bar" },
            .{ .id = 3, .val = "ba'r" },
            .{ .id = 4, .val = "bar\nbar" },
            .{ .id = 5, .val = "bar `bar`\nbar" },
        },
    };

    const S = struct {
        fn encodeRoot(ctx: *EncodeMapContext, val: TestRoot) !void {
            try ctx.encodeString("name", val.name);
            try ctx.encodeSlice("list", val.list, encodeValue);
            try ctx.encodeAsMap("map", val.map, encodeMap);
        }
        fn encodeMap(ctx: *EncodeMapContext, val: []const TestMapItem) !void {
            for (val) |it| {
                try ctx.encodeAnyToString(it.id, it.val);
            }
        }
        fn encodeItem(ctx: *EncodeMapContext, val: TestListItem) !void {
            try ctx.encode("field", val.field);
        }
        fn encodeValue(ctx: *EncodeValueContext, val: anytype) !void {
            const T = @TypeOf(val);
            if (T == TestRoot) {
                try ctx.encodeAsMap(val, encodeRoot);
            } else if (T == TestListItem) {
                try ctx.encodeAsMap(val, encodeItem);
            } else {
                stdx.panicFmt("unsupported: {s}", .{@typeName(T)});
            }
        }
    };

    const res = try encode(t.alloc, null, root, S.encodeValue);
    defer t.alloc.free(res);
    try t.eqStr(res,
        \\{
        \\    name: 'project'
        \\    list: [
        \\        {
        \\            field: 1
        \\        }
        \\        {
        \\            field: 2
        \\        }
        \\    ]
        \\    map: {
        \\        1: 'foo'
        \\        2: 'bar'
        \\        3: 'ba\'r'
        \\        4: `bar
        \\bar`
        \\        5: `bar \`bar\`
        \\bar`
        \\    }
        \\}
    );
}

test "decodeMap" {
    const S = struct {
        fn decodeRoot(map: DecodeMapIR, _: void, root: *TestRoot) !void {
            root.name = try map.allocString("name");

            var list: std.ArrayListUnmanaged(TestListItem) = .{};
            var list_ir = try map.decodeList("list");
            defer list_ir.deinit();
            var i: u32 = 0;
            while (i < list_ir.arr.len) : (i += 1) {
                var item: TestListItem = undefined;
                var item_map = try list_ir.decodeMap(i);
                defer item_map.deinit();
                item.field = try item_map.getU32("field");
                try list.append(t.alloc, item);
            }
            root.list = try list.toOwnedSlice(t.alloc);

            var map_items: std.ArrayListUnmanaged(TestMapItem) = .{};
            var map_ = try map.decodeMap("map");
            defer map_.deinit();
            var iter = map_.iterator();
            while (iter.next()) |entry| {
                const key = try std.fmt.parseInt(u32, entry.key_ptr.*, 10);
                const value = try map_.allocString(entry.key_ptr.*);
                try map_items.append(t.alloc, .{ .id = key, .val = value });
            }
            root.map = try map_items.toOwnedSlice(t.alloc);
        }
    };

    var parser = Parser.init(t.alloc);
    defer parser.deinit();

    var root: TestRoot = undefined;
    try decodeMap(t.alloc, &parser, {}, &root, S.decodeRoot, 
        \\{
        \\    name: 'project'
        \\    list: [
        \\        {
        \\            field: 1
        \\        }
        \\        {
        \\            field: 2
        \\        }
        \\    ]
        \\    map: {
        \\        1: 'foo'
        \\        2: 'bar'
        \\        3: 'ba\'r'
        \\        4: "bar
        \\bar"
        \\        5: "bar `bar`
        \\bar"
        \\    }
        \\}
    );
    defer {
        t.alloc.free(root.list);
        t.alloc.free(root.name);
        for (root.map) |it| {
            t.alloc.free(it.val);
        }
        t.alloc.free(root.map);
    }

    try t.eqStr(root.name, "project");
    try t.eq(root.list[0].field, 1);
    try t.eq(root.list[1].field, 2);
    try t.eq(root.map.len, 5);
    try t.eq(root.map[0].id, 1);
    try t.eqStr(root.map[0].val, "foo");
    try t.eq(root.map[1].id, 2);
    try t.eqStr(root.map[1].val, "bar");
    try t.eq(root.map[2].id, 3);
    try t.eqStr(root.map[2].val, "ba'r");
    try t.eq(root.map[3].id, 4);
    try t.eqStr(root.map[3].val, "bar\nbar");
    try t.eq(root.map[4].id, 5);
    try t.eqStr(root.map[4].val, "bar `bar`\nbar");
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