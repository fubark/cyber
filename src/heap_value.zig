const std = @import("std");
const build_config = @import("build_config");
const vmc = @import("vmc");
const cy = @import("cyber.zig");
const C = @import("capi.zig");
const tcc = @import("tcc");
const Value = cy.Value;
const bt = cy.types.BuiltinTypes;

const log = cy.log.scoped(.heap_value);

const log_mem = build_config.log_mem;
const log_free = log_mem and true;

pub fn moveValueTo(self: *cy.Heap, val_t: *cy.Type, dst: [*]u8, val: Value) void {
    if (val_t.is_cte_boxed()) {
        const src = val.asPtr([*]const u8);
        @memcpy(dst[0..val_t.size()], src[0..val_t.size()]);
        self.freeOnly(val.asPtr(*cy.HeapObject));
    } else {
        const src: [*]const u8 = @ptrCast(&val);
        @memcpy(dst[0..val_t.size()], src[0..val_t.size()]);
    }
}

pub fn copyValueTo(self: *cy.Heap, val_t: *cy.Type, dst: [*]u8, src: [*]const u8) !void {
    switch (val_t.id()) {
        bt.Void => {},
        bt.R64,
        bt.F64,
        bt.I64,
        bt.Type => {
            valueCast(dst)[0] = valueConstCast(src)[0];
        },
        bt.R32,
        bt.I32,
        bt.F32 => {
            @as(*u32, @ptrCast(@alignCast(dst))).* = @as(*const u32, @ptrCast(@alignCast(src))).*;
        },
        bt.R8,
        bt.I8,
        bt.Bool => {
            dst[0] = src[0];
        },
        bt.StrLit => {
            const src_str: *const cy.heap.StrLit = @ptrCast(@alignCast(src));
            const new_buf = try self.alloc.dupe(u8, src_str.slice());
            const dst_str: *cy.heap.StrLit = @ptrCast(@alignCast(dst));
            dst_str.* = .{
                .ptr = @intFromPtr(new_buf.ptr),
                .header = src_str.header,
            };
        },
        else => {
            switch (val_t.kind()) {
                .vector => {
                    const array_t = val_t.cast(.vector);
                    for (0..array_t.n) |i| {
                        const offset = i * array_t.elem_t.size();
                        try self.copyValueTo(array_t.elem_t, dst + offset, src + offset);
                    }
                },
                .option => {
                    const option_t = val_t.cast(.option);
                    try copyOptionTo(self, option_t, dst, src);
                },
                .struct_t => {
                    const struct_t = val_t.cast(.struct_t);
                    try copyStructTo(self, struct_t, dst, src);
                },
                .func_ptr => {
                    valueCast(dst)[0] = valueConstCast(src)[0];
                },
                .enum_t => {
                    valueCast(dst)[0] = valueConstCast(src)[0];
                },
                .pointer => {
                    const ptr_t = val_t.cast(.pointer);
                    if (ptr_t.ref) {
                        self.retain(valueConstCast(src)[0]);
                    }
                    valueCast(dst)[0] = valueConstCast(src)[0];
                },
                else => {
                    std.debug.panic("Unsupported: {s} {}", .{val_t.name(), val_t.kind()});
                },
            }
        },
    }
}

pub fn copyValueTo2(self: *cy.Heap, val_t: *cy.Type, dst: [*]u8, val: Value) !void {
    if (val_t.info.no_copy) {
        return error.NoCopy;
    }
    if (val_t.is_cte_boxed()) {
        try self.copyValueTo(val_t, dst, val.asPtr([*]const u8));
    } else {
        try self.copyValueTo(val_t, dst, @ptrCast(&val));
    }
}

pub fn copyValue(self: *cy.Heap, val: cy.TypeValue) !cy.TypeValue {
    const new = try self.copyValue2(val.type, val.value);
    return .{ .type = val.type, .value = new };
}

pub fn copyValue2(self: *cy.Heap, val_t: *cy.Type, val: Value) !Value {
    if (val_t.is_cte_boxed()) {
        const src = val.asPtr([*]const u8);
        return copyValue3(self, val_t, src);
    } else {
        return copyValue3(self, val_t, @ptrCast(&val));
    }
}

pub fn copyValue3(self: *cy.Heap, val_t: *cy.Type, src: [*]const u8) !Value {
    switch (val_t.id()) {
        bt.Void => {
            return Value.Void;
        },
        bt.R8,
        bt.I8,
        bt.Bool => {
            return Value.initRaw(@intCast(src[0]));
        },
        bt.R16,
        bt.I16 => {
            const ptr: *const i16 = @ptrCast(@alignCast(src));
            return Value.initInt16(@bitCast(ptr.*));
        },
        bt.R32,
        bt.F32,
        bt.I32 => {
            const ptr: *const i32 = @ptrCast(@alignCast(src));
            return Value.initInt32(ptr.*);
        },
        bt.PartialStructLayout,
        bt.FuncSig,
        bt.IntLit,
        bt.R64,
        bt.F64,
        bt.I64,
        bt.Type => {
            return valueConstCast(src)[0];
        },
        bt.StrLit => {
            const src_str: *const cy.heap.StrLit = @ptrCast(@alignCast(src));
            const new_buf = try self.alloc.dupe(u8, src_str.slice());
            const new = try self.new_object_undef(bt.StrLit, 16);
            new.str_lit = .{
                .ptr = @intFromPtr(new_buf.ptr),
                .header = src_str.header,
            };
            return Value.initPtr(new);
        },
        else => {
            switch (val_t.kind()) {
                .vector => {
                    const array_t = val_t.cast(.vector);
                    const array = try self.new_object_undef(array_t.base.id(), array_t.size);
                    try self.copyValueTo(&array_t.base, @ptrCast(array), src);
                    return Value.initPtr(array);
                },
                .option => {
                    const option_t = val_t.cast(.option);
                    return copyOption(self, option_t, src);
                },
                .choice => {
                    const choice_t = val_t.cast(.choice);
                    return copyChoice(self, choice_t, src);
                },
                .struct_t => {
                    const struct_t = val_t.cast(.struct_t);
                    return copyStruct(self, struct_t, src);
                },
                .pointer => {
                    const ptr_t = val_t.cast(.pointer);
                    if (ptr_t.ref) {
                        self.retain(valueConstCast(src)[0]);
                    }
                    return valueConstCast(src)[0];
                },
                .func_sym => {
                    return valueConstCast(src)[0];
                },
                .borrow,
                .func_ptr,
                .enum_t => {
                    return valueConstCast(src)[0];
                },
                else => {},
            }
            std.debug.panic("Unsupported: {s} {} {}", .{val_t.name(), val_t.kind(), val_t.id()});
        },
    }
}

pub fn copy_str(self: *cy.Heap, val: cy.heap.Str) !cy.heap.Str {
    if (val.buf()) |buf| {
        self.retainObject(@ptrCast(buf));
    }
    return val;
}

pub fn copyStruct(self: *cy.Heap, struct_t: *cy.types.Struct, src: [*]const u8) !Value {
    const new = try self.new_object_undef(struct_t.base.id(), struct_t.size);
    const dst = new.object.getBytePtr();
    try self.copyStructTo(struct_t, dst, src);
    return Value.initPtr(new);
}

pub fn copyStructTo(self: *cy.Heap, struct_t: *cy.types.Struct, dst: [*]u8, src: [*]const u8) anyerror!void {
    if (!struct_t.base.info.copy_user) {
        for (struct_t.fields()) |field| {
            try self.copyValueTo(field.type, dst + field.offset, src + field.offset);
        }
    } else {
        std.debug.panic("Unsupported: {s}", .{struct_t.base.name()});
    }
}

pub fn copyChoice(self: *cy.Heap, choice_t: *cy.types.Choice, src: [*]const u8) !Value {
    const new = try self.new_object_undef(choice_t.base.id(), choice_t.size);
    const dst = new.object.getBytePtr();
    try copyChoiceTo(self, choice_t, dst, src);
    return Value.initPtr(new);
}

fn copyChoiceTo(self: *cy.Heap, choice_t: *cy.types.Choice, dst: [*]u8, src: [*]const u8) !void {
    const tag = valueConstCast(src)[0];
    valueCast(dst)[0] = tag;
    for (choice_t.cases()) |case| {
        if (tag.asInt() == @as(i64, @intCast(case.val))) {
            try self.copyValueTo(case.payload_t, dst + 8, src + 8);
            return;
        }
    }
    @panic("unreachable");
}

pub fn unwrap_option_or(self: *cy.Heap, option_t: *cy.types.Option, src: Value) !?Value {
    if (option_t.zero_union) {
        const tag = src.asInt();
        if (tag == 0) {
            return null;
        }
        return try copyValue3(self, option_t.child_t, @ptrCast(&src));
    } else {
        const tag = src.asPtr(*i64).*;
        if (tag == 0) {
            return null;
        }
        return try copyValue3(self, option_t.child_t, src.asBytes() + 8);
    }
}

pub fn copyOption(self: *cy.Heap, option_t: *cy.types.Option, src: [*]const u8) !Value {
    if (option_t.zero_union) {
        var new: cy.Value = undefined;
        try copyOptionTo(self, option_t, @ptrCast(&new), src);
        return new;
    } else {
        const new = try self.new_object_undef(option_t.base.id(), option_t.size);
        try copyOptionTo(self, option_t, new.object.getBytePtr(), src);
        return Value.initPtr(new);
    }
}

fn copyOptionTo(self: *cy.Heap, option_t: *cy.types.Option, dst: [*]u8, src: [*]const u8) anyerror!void {
    const tag = valueConstCast(src)[0];
    if (option_t.zero_union) {
        if (tag.asInt() != 0) {
            try self.copyValueTo(option_t.child_t, dst, src);
        } else {
            valueCast(dst)[0] = Value.initInt(0);
        }
    } else {
        valueCast(dst)[0] = tag;
        if (tag.asInt() == 1) {
            try self.copyValueTo(option_t.child_t, dst + 8, src + 8);
        }
    }
}

pub fn destructValue(self: *cy.Heap, val: cy.TypeValue) void {
    destructValue2(self, val.type, val.value);
}

pub fn destructValue2(self: *cy.Heap, val_t: *cy.Type, val: Value) void {
    switch (val_t.id()) {
        bt.Void,
        bt.F32,
        bt.F64,
        bt.R8,
        bt.R16,
        bt.R32,
        bt.R64,
        bt.I8,
        bt.I16,
        bt.I32,
        bt.I64,
        bt.Any,
        bt.IntLit,
        bt.Symbol,
        bt.FuncSig,
        bt.Type,
        bt.PartialStructLayout,
        bt.Code,
        bt.Bool => {},
        bt.StrBuffer => @panic("Unexpected."),
        bt.Object => {
            self.release(val);
        },
        else => {
            switch (val_t.kind()) {
                .choice => {
                    self.release(val);
                },
                .option => {
                    const option_t = val_t.cast(.option);
                    if (option_t.zero_union) {
                        if (val.val != 0) {
                            self.release(val);
                        }
                    } else {
                        self.release(val);
                    }
                    // const ptr = val.asHeapObject().object.getValuesPtr();
                    // if (ptr[0].asInt() == 1) {
                    //     destructValueAt(vm, option_t.child_t, ptr + 1);
                    // }
                    // freeObject(vm, @ptrCast(ptr), false, {}); 
                },
                .struct_t => {
                    // const struct_t = val_t.cast(.struct_t);
                    // if (!struct_t.cstruct) {
                    //     const ptr = val.asHeapObject().object.getValuesPtr();
                    //     for (struct_t.fields()) |field| {
                    //         destructValueAt(vm, field.type, ptr + field.offset);
                    //     }
                    //     freeObject(vm, @ptrCast(ptr), false, {}); 
                    // } else {
                    //     std.debug.panic("Unsupported: {s}", .{val_t.name()});
                    // }
                    self.release(val);
                },
                .partial_vector => {
                    self.release(val);
                },
                .vector => {
                    self.release(val);
                },
                .borrow_trait,
                .ref_trait => {
                    self.release(val);
                },
                .pointer => {
                    const ptr_t = val_t.cast(.pointer);
                    if (ptr_t.ref) {
                        self.release(val);
                    }
                },
                .borrow,
                .func_sym,
                .int,
                .func_ptr,
                .enum_t => {
                    // Nop.
                },
                else => {
                    std.debug.panic("Unsupported: {} {s}", .{val_t.kind(), val_t.name()});
                },
            }
        }
    }
}

pub fn destructStr(self: *cy.Heap, str: *const cy.heap.Str) void {
    if (str.buf()) |buf| { 
        self.releaseObject(@ptrCast(buf));
    }
}

pub fn destructValueAt(self: *cy.Heap, val_t: *cy.Type, ptr: [*]u8) void {
    switch (val_t.id()) {
        bt.Void,
        bt.R8,
        bt.R16,
        bt.R32,
        bt.R64,
        bt.I8,
        bt.I16,
        bt.I32,
        bt.I64,
        bt.F32,
        bt.F64,
        bt.Symbol,
        bt.Type,
        bt.Bool => {},
        bt.Object => {
            self.release(valueCast(ptr)[0]);
        },
        bt.StrLit => {
            var s: *cy.heap.StrLit = @ptrCast(@alignCast(ptr));
            self.alloc.free(s.slice());
        },
        bt.Str => {
            self.destructStr(@ptrCast(@alignCast(ptr)));
        },
        bt.StrBuffer => @panic("Unexpected."),
        else => {
            switch (val_t.kind()) {
                .option => {
                    const option_t = val_t.cast(.option);
                    if (option_t.zero_union) {
                        if (valueCast(ptr)[0].asInt() != 0) {
                            destructValueAt(self, option_t.child_t, ptr);
                        }
                    } else {
                        if (valueCast(ptr)[0].asInt() == 1) {
                            destructValueAt(self, option_t.child_t, ptr + 8);
                        }
                    }
                },
                .struct_t => {
                    const struct_t = val_t.cast(.struct_t);
                    if (!struct_t.cstruct) {
                        for (struct_t.fields()) |field| {
                            destructValueAt(self, field.type, ptr + field.offset);
                        }
                    }
                },
                .vector => {
                    const vector_t = val_t.cast(.vector);
                    for (0..vector_t.n) |i| {
                        const offset = i * vector_t.elem_t.size();
                        destructValueAt(self, vector_t.elem_t, ptr + offset);
                    }
                },
                .ref_trait => {
                    self.release(valueCast(ptr)[0]);
                },
                .pointer => {
                    const ptr_t = val_t.cast(.pointer);
                    if (ptr_t.ref) {
                        self.release(valueCast(ptr)[0]);
                    }
                },
                .borrow,
                .enum_t => {
                    // Nop.
                },
                else => {
                    std.debug.panic("Unsupported: {s}", .{val_t.name()});
                },
            }
        }
    }
}

pub inline fn retain(self: *cy.Heap, val: cy.Value) void {
    retainObject(self, val.asHeapObject());
}

pub fn retainObjectOpt(self: *cy.Heap, opt: ?*cy.HeapObject) void {
    if (opt) |obj| {
        self.retainObject(obj);
    }
}

pub inline fn retainObject(self: *cy.Heap, obj: *cy.HeapObject) void {
    if (cy.Trace) {
        if (self.checkRetainDanglingPointer(obj)) {
            std.debug.panic("retaining dangling pointer: {*}", .{obj});
        }
        log.tracevIf(log_mem, "{} +1, {s}, {*}", .{obj.rc(), self.getType(obj.getTypeId()).name(), obj});
    }
    obj.retain();
    if (cy.Trace) {
        self.c.refCounts += 1;
        self.c.numRetains += 1;
    }
}

pub inline fn retainInc(self: *cy.Heap, val: cy.Value, inc: u32) void {
    const obj = val.asBoxObject();
    if (cy.Trace) {
        cy.heap.checkRetainDanglingPointer(self, obj);
        log.tracevIf(log_mem, "{} +{}, {s}, {*}", .{obj.head.rc, inc, self.getTypeName(obj.getTypeId()), obj});
    }
    obj.head.rc += inc;
    if (cy.Trace) {
        self.c.refCounts += inc;
        self.c.trace.numRetains += inc;
    }
}

pub fn releaseOpaque(self: *cy.Heap, obj: *anyopaque) void {
    releaseObject2(self, @ptrCast(@alignCast(obj)), false, {});
}

pub fn releaseObject(self: *cy.Heap, obj: *cy.HeapObject) void {
    releaseObject2(self, obj, false);
}

pub fn releaseObject2(self: *cy.Heap, obj: *cy.HeapObject, comptime fatal: bool) void {
    if (releaseOnly(self, obj)) {
        @call(.never_inline, destroyObject, .{self, obj, fatal});
    }
}

pub fn release(self: *cy.Heap, val: cy.Value) void {
    releaseObject(self, val.asHeapObject());
}

/// Returns whether the reference count reached zero.
pub fn releaseOnly(self: *cy.Heap, obj: *cy.HeapObject) bool {
    if (cy.Trace) {
        if (self.checkDoubleFree(obj)) {
            std.debug.panic("Double free. {*}", .{obj});
        }
        if (self.c.refCounts == 0) {
            std.debug.print("Double free. {}\n", .{obj.getTypeId()});
            cy.fatal();
        }
    }
    if (cy.TraceRC) {
        log.tracevIf(log_mem, "{} -1, {s}, {*}", .{obj.rc(), self.getType(obj.getTypeId()).name(), obj});
    }
    obj.release();
    if (cy.Trace) {
        self.c.refCounts -= 1;
        self.c.numReleases += 1;
    }
    return obj.rc() == 0;
}

pub fn freeOnly(self: *cy.Heap, obj: *cy.HeapObject) void {
    const zero_rc = self.releaseOnly(obj);
    if (cy.Trace) {
        if (!zero_rc) {
            std.debug.panic("Expected 0 rc.", .{});
        }
    }
    const obj_t = self.getType(obj.getTypeId());
    self.freeObject(obj, obj_t.size());
}

pub fn unbox2(self: *cy.Heap, val: Value, type_id: cy.TypeId) !Value {
    switch (type_id) {
        bt.Void => {
            return Value.initRaw(0);
        },
        bt.Bool => {
            return Value.initBool(val.asPtr(*bool).*);
        },
        bt.F64 => {
            return Value.initFloat64(val.asRefF64());
        },
        bt.I8 => {
            return @bitCast(@as(u64, @intCast(val.asRefByte())));
        },
        bt.I32 => {
            return @bitCast(val.asRefInt());
        },
        bt.I64 => {
            return @bitCast(val.asRefInt());
        },
        bt.Symbol => {
            return Value.initSymbol(val.asRefSymbol());
        },
        bt.Error => {
            return Value.initError(val.asRefSymbol());
        },
        else => {
            const type_ = self.getType(type_id);
            switch (type_.kind()) {
                .struct_t => {
                    const src = val.asPtr([*]const u8);
                    return copyStruct(self, type_.cast(.struct_t), src);
                },
                .option => {
                    const src = val.asPtr([*]const u8);
                    return copyOption(self, type_.cast(.option), src);
                },
                .func_sym => {
                    return val;
                },
                .pointer => {
                    return @bitCast(val.asRefInt());
                },
                .enum_t => {
                    return @bitCast(val.asRefInt());
                },
                else => {
                    std.debug.panic("TODO: {} {s}", .{type_.kind(), type_.name()});
                },
            }
        },
    }
}

pub fn unbox(self: *cy.Heap, val: Value) !cy.Value {
    const type_id = val.getRefeeType();
    return unbox2(self, val, type_id);
}

pub fn destroyObject(self: *cy.Heap, obj: *cy.heap.HeapObject, comptime fatal: bool) void {
    if (cy.Trace) {
        if (obj.isFreed()) {
            cy.panicFmt("Double free object: {*} Should have been discovered in release op.", .{obj});
        } else {
            if (log_free) {
                if (fatal) {
                    // Since heap objects in an arbitrary order during fatal cleanup, only print pointer and type name.
                    log.tracev("free type={}({s}) {*}", .{
                        obj.getTypeId(), self.getType(obj.getTypeId()).name(), obj,
                    });
                } else {
                    // Avoid printing too much details about the value during types teardown
                    // since it can be dependent on something already freed. (e.g. `type` that already has freed template args)
                    const desc = self.bufPrintBoxValueShortStr(&cy.tempBuf, Value.initPtr(obj), false) catch cy.fatal();
                    log.tracev("free type={}({s}) {*}: `{s}`", .{
                        obj.getTypeId(), self.getType(obj.getTypeId()).name(), obj, desc,
                    });
                }
            }
        }
    }

    const typeId = obj.getTypeId();
    switch (typeId) {
        bt.Range => {
            self.freePoolObject(obj);
        },
        bt.FuncSig => {
            self.freePoolObject(obj);
        },
        bt.I64 => {
            self.freePoolObject(obj);
        },
        bt.StrLit => {
            self.alloc.free(obj.str_lit.slice());
            self.freePoolObject(obj);
        },
        bt.StrBuffer => {
            const len = obj.raw_buffer.len;
            if (8 + len <= 32) {
                self.freePoolObject(obj);
            } else {
                self.freeBigObject(obj, 8 + len);
            }
        },
        bt.TccState => {
            if (cy.hasFFI) {
                tcc.tcc_delete(obj.tccState.state);
                if (obj.tccState.hasDynLib) {
                    obj.tccState.lib.close();
                    self.alloc.destroy(obj.tccState.lib);
                }
                self.freePoolObject(obj);
            } else {
                unreachable;
            }
        },
        bt.Type => {
            self.freePoolObject(obj);
        },
        else => {
            if (cy.Trace) {
                // Check range.
                if (typeId >= self.sema.types.items.len) {
                    log.tracev("unsupported type {}", .{typeId});
                    cy.fatal();
                }
            }
            // TODO: Determine isHostObject from object to avoid extra read from `rt.Type`
            // TODO: Use a dispatch table for host objects only.
            const obj_t = self.getType(typeId);
            switch (obj_t.kind()) {
                .result => {
                    if (!fatal) {
                        self.destructValueAt(obj_t, @ptrCast(obj));
                    }
                    const result_t = obj_t.cast(.result);
                    self.freeObject(obj, result_t.size);
                },
                .option => {
                    if (!fatal) {
                        self.destructValueAt(obj_t, @ptrCast(obj));
                    }
                    const option_t = obj_t.cast(.option);
                    self.freeObject(obj, option_t.size);
                },
                .struct_t => {
                    const struct_t = obj_t.cast(.struct_t);
                    if (!struct_t.cstruct) {
                        if (obj_t.sym().instance) |variant| {
                            if (variant.data.sym.template == self.sema.raw_buffer_tmpl) {
                                // Emulate destructor.
                                const elem_t = variant.params[0].asPtr(*cy.Type);
                                const elem_size = elem_t.size();
                                if (!fatal) {
                                    const elems: [*]u8 = @ptrCast(&obj.raw_buffer.data);
                                    const nelems: usize = @intCast(obj.raw_buffer.len);
                                    for (0..nelems) |i| {
                                        self.destructValueAt(elem_t, @ptrCast(@alignCast(elems + i*elem_size)));
                                    }
                                }
                                const size = 8 + obj.raw_buffer.len * elem_size;
                                self.freeObject(obj, size);
                                return;
                            } else if (variant.data.sym.template == self.sema.buffer_tmpl) {
                                const elem_t = variant.params[0].asPtr(*cy.Type);
                                const elem_size = elem_t.size();
                                if (obj.buffer.base != 0) {
                                    const elems: [*]u8 = @ptrFromInt(obj.buffer.base);
                                    if (!fatal) {
                                        const nelems: usize = @intCast(obj.buffer.len);
                                        for (0..nelems) |i| {
                                            self.destructValueAt(elem_t, elems + i*elem_size);
                                        }
                                        self.free_byte_buffer(@ptrCast(@alignCast(elems)));
                                    }
                                }
                                return;
                            }
                        }
                        if (!fatal) {
                            self.destructValueAt(obj_t, @ptrCast(obj));
                        }
                    }
                    self.freeObject(obj, struct_t.size);
                },
                .func_ptr => {},
                .func => {
                    switch (obj.func.kind) {
                        .host => {
                            self.freePoolObject(obj);
                        },
                        .closure => {
                            const num_captured = obj.func.data.closure.numCaptured;
                            if (!fatal) {
                                const src = obj.func.getCapturedValuesPtr()[0..num_captured];
                                for (src) |captured| {
                                    release(self, captured);
                                }
                            }
                            if (num_captured <= 1) {
                                self.freePoolObject(obj);
                            } else {
                                self.freeBigObject(obj, (3 + num_captured) * @sizeOf(Value));
                            }
                        },
                        .pinned_closure => {
                            const num_captured = obj.func.data.closure.numCaptured;
                            if (num_captured <= 1) {
                                self.freePoolObject(obj);
                            } else {
                                self.freeBigObject(obj, (3 + num_captured) * @sizeOf(Value));
                            }
                        },
                        .bc => {
                            self.freePoolObject(obj);
                        },
                    }
                },
                .func_sym => {
                    self.freePoolObject(obj);
                },
                .vector => {
                    const array_t = obj_t.cast(.vector);
                    if (!fatal) {
                        const child_t = array_t.elem_t;
                        const elem_size = child_t.size();
                        const vals = obj.object.getBytePtr();
                        for (0..array_t.n) |i| {
                            self.destructValueAt(child_t, vals + i * elem_size);
                        }
                    }
                    self.freeObject(obj, array_t.size);
                },
                .partial_vector => {
                    const array_t = obj_t.cast(.partial_vector);
                    if (!fatal) {
                        const child_t = array_t.elem_t;
                        const elem_size = child_t.size();
                        const vals = obj.object.getBytePtr() + 8;
                        const len = obj.object.firstValue.val;
                        for (0..len) |i| {
                            self.destructValueAt(child_t, vals + i * elem_size);
                        }
                    }
                    self.freeObject(obj, array_t.size);
                },
                .ref_trait => {
                    const vals = obj.object.getValuesPtr();
                    if (!fatal) {
                        self.release(vals[0]);
                    }
                    self.freePoolObject(obj);
                },
                .c_union => {
                    const c_union = obj_t.cast(.c_union);
                    self.freeObject(obj, c_union.size);
                },
                .choice => {
                    const choice_t = obj_t.cast(.choice);
                    if (!fatal) {
                        const vals = obj.object.getValuesPtr();
                        const tag: usize = @intCast(vals[0].asInt());
                        const payload_t = choice_t.getCaseByTag(@intCast(tag)).payload_t;
                        self.destructValueAt(payload_t, @ptrCast(vals + 1));
                    }
                    self.freeObject(obj, choice_t.size);
                },
                .pointer => {
                    if (!fatal) {
                        if (obj_t.cast(.pointer).ref) {
                            self.release(obj.object.firstValue);
                        }
                    }
                    self.freePoolObject(obj);
                },
                .borrow_trait,
                .void,
                .bool,
                .raw,
                .int,
                .float,
                .enum_t => {
                    self.freePoolObject(obj);
                },
                .dyn_trait,
                .c_variadic,
                .never,
                .generic_vector,
                .generic_trait,
                .int_lit,
                .generic,
                .null,
                .borrow,
                .ex_borrow,
                .bare => {
                    std.debug.panic("Unexpected type {}.", .{obj_t.kind()});
                },
            }
        },
    }
    if (cy.Trace) {
        if (self.countFrees) {
            self.numFreed += 1;
        }
    }
}

pub fn allocValueStr(self: *cy.Heap, val: Value) ![]const u8 {
    const str = try self.get_or_buf_print_object(&cy.tempBuf, val);
    return try self.alloc.dupe(u8, str);
}

pub fn bufPrintBoxValueShortStr(self: *cy.Heap, buf: []u8, val: Value, skip_full_type_names: bool) ![]const u8 {
    var final_val = val;
    const val_t = final_val.getRefeeType();
    if (self.getType(val_t).isPrimitive()) {
        final_val = try unbox2(self, final_val, val_t);
    }
    return bufPrintValueShortStr(self, buf, val_t, final_val, skip_full_type_names);
}

pub fn bufPrintValueShortStr(self: *cy.Heap, buf: []u8, val_t: cy.TypeId, val: Value, skip_full_type_names: bool) ![]const u8 {
    var w = std.Io.Writer.fixed(buf);

    if (val_t == bt.Str) {
        const str = val.asString();
        if (str.len > 20) {
            try w.print("String({}) {s}...", .{str.len, str[0..20]});
        } else {
            try w.print("String({}) {s}", .{str.len, str});
        }
    } else if (val_t == cy.heap.BigObjectPtrType & vmc.TYPE_MASK) {
        try w.print("bigObjectPtr", .{});
    } else {
        _ = try self.writeValue(&w, val_t, val, skip_full_type_names);
    }
    return w.buffered();
}

/// String is guaranteed to be valid UTF-8.
pub fn get_or_buf_print_object(self: *cy.Heap, buf: []u8, val: Value) ![]const u8 {
    var w = std.Io.Writer.fixed(buf);

    if (val.isRefString()) {
        return val.asString();
    } else {
        const val_t = val.getRefeeType();
        _ = try writeValue(self, &w, val_t, try self.unbox(val), false);
    }
    return w.buffered();
}

pub fn get_or_buf_print_object2(self: *cy.Heap, buf: []u8, val: Value, out_ascii: *bool) ![]const u8 {
    var fbuf = std.io.fixedBufferStream(buf);
    const w = fbuf.writer();

    if (val.isRefString()) {
        out_ascii.* = val.asHeapObject().string.ascii();
        return val.asString();
    } else {
        const val_t = val.getRefeeType();
        if (try self.writeValue(w, val_t, try self.unbox(val), false)) {
            out_ascii.* = true;
            return fbuf.getWritten();
        } else {
            const res = fbuf.getWritten();
            out_ascii.* = cy.string.isAstring(res);
            return res;
        }
    }
}

pub fn writeValue2(self: *const cy.Heap, w: anytype, val_static_t: cy.TypeId, val: Value, skip_full_type_names: bool) !bool {
    if (val_static_t == bt.Object or val_static_t == bt.Dyn) {
        const val_t = val.getTypeId();
        return writeValue(self, w, val_t, try self.unbox(val), skip_full_type_names);
    } else {
        return writeValue(self, w, val_static_t, val, skip_full_type_names);
    }
}

/// Assumes `val` is unboxed.
/// Returns whether the string written can be assumed to be ASCII.
/// `skip_full_type_names` can be useful when printing a value during teardown.
pub fn writeValue(self: *const cy.Heap, w: *std.Io.Writer, val_t: cy.TypeId, val: Value, skip_full_type_names: bool) !bool {
    switch (val_t) {
        bt.F64 => {
            const f = val.asF64();
            if (Value.floatIsSpecial(f)) {
                try w.print("{}", .{f});
            } else {
                if (Value.floatCanBeInteger(f)) {
                    try w.print("{d:.1}", .{f});
                } else {
                    try w.print("{d}", .{f});
                }
            }
            return true;
        },
        bt.Bool => {
            if (val.asBool()) {
                try w.writeAll("true");
            } else {
                try w.writeAll("false");
            }
            return true;
        },
        bt.Error => {
            const symId = val.asError();
            try w.print("error.{s}", .{self.getSymbolName(symId)});
            return true;
        },
        bt.Symbol => {
            const litId = val.asSymbol();
            try w.print("${s}", .{self.getSymbolName(litId)});
            return true;
        },
        bt.I64 => {
            try w.print("{}", .{val.asInt()});
            return true;
        },
        bt.I32 => {
            try w.print("{}", .{val.asI32()});
            return true;
        },
        bt.I8 => {
            try w.print("{}", .{val.asByte()});
            return true;
        },
        bt.IntLit => {
            try w.print("{}", .{val.as_int_lit()});
            return true;
        },
        bt.Str => {
            const str = val.asPtr(*cy.heap.Str);
            try w.writeAll(str.slice());
            return str.ascii();
        },
        bt.Type => {
            try w.writeAll("type: ");
            const type__ = val.asPtr(*cy.Type);
            if (skip_full_type_names) {
                try w.writeAll(type__.name());
            } else {
                try self.sema.writeTypeName(w, type__, null);
            }
            return false;
        },
        else => {
            const type_ = self.getType(val_t);
            if (val_t == cy.NullId & vmc.TYPE_MASK) {
                try w.writeAll("danglingObject");
                return true;
            }
            if (val_t == cy.heap.BigObjectPtrType & vmc.TYPE_MASK) {
                try w.writeAll("BigObjectPtr");
                return true;
            }

            if (type_.kind() == .enum_t) {
                const enumv = val.asInt();
                const name = type_.cast(.enum_t).getCaseByTag(@intCast(enumv)).head.name();
                try w.print("{s}.{s}", .{type_.name(), name});
                return false;
            } else if (type_.kind() == .pointer) {
                const pointer_t = type_.cast(.pointer);
                if (pointer_t.ref) {
                    try w.writeAll("^");
                    if (skip_full_type_names) {
                        try w.writeAll(type_.name());
                    } else {
                        try self.sema.writeTypeName(w, type_, null);
                    }
                    return false;
                }
            }

            if (skip_full_type_names) {
                try w.writeAll(type_.name());
            } else {
                try self.sema.writeTypeName(w, type_, null);
            }
            return false;
        },
    }
}

pub fn unwrapChoice(self: *cy.Heap, choice_t: *cy.Type, choice: cy.Value, tag_name: []const u8) !cy.Value {
    if (choice_t.kind() != .choice) {
        return error.InvalidArgument;
    }

    const sym = choice_t.sym().getMod().getSym(tag_name) orelse {
        return error.InvalidArgument;
    };
    if (sym.type != .choice_case) {
        return error.InvalidArgument;
    }
    const case = sym.cast(.choice_case);

    const active_tag = choice.asHeapObject().object.getValue(0).asInt();
    if (active_tag != case.val) {
        return error.TagMismatch;
    }

    const payload = choice.asHeapObject().object.getValuePtr(1);
    if (case.payload_t.is_cte_boxed()) {
        const obj = try self.new_object_undef(case.payload_t.id(), case.payload_t.size());
        try self.copyValueTo(case.payload_t, @ptrCast(obj), @ptrCast(payload));
        return Value.initPtr(obj);
    } else {
        if (case.payload_t.isObject()) {
            self.retain(payload.*);
        }
        return payload.*;
    }
}

pub fn box(self: *cy.Heap, val: Value, type_id: cy.TypeId) !cy.Value {
    switch (type_id) {
        bt.Object => {
            self.retain(val);
            return val;
        },
        bt.Void => {
            return cy.Value.Void;
        },
        bt.Bool => {
            return self.allocBool(val.asBool());
        },
        bt.MetaType => {
            return self.allocBoxValue(type_id, val);
        },
        bt.Symbol => {
            return self.allocBoxValue(type_id, val);
        },
        bt.I64 => {
            return self.allocBoxValue(type_id, val);
        },
        bt.Mask8 => {
            return self.allocBoxValue(type_id, val);
        },
        bt.Float => {
            return self.allocBoxValue(type_id, val);
        },
        bt.Error => {
            return self.allocBoxValue(type_id, val);
        },
        else => {
            const type_ = self.getType(type_id);
            switch (type_.kind()) {
                .struct_t => {
                    const src = val.asPtr([*]const u8);
                    return self.copyStruct(type_.cast(.struct_t), src);
                },
                .hostobj => {
                    return val;
                },
                .enum_t => {
                    return self.allocBoxValue(type_id, val);
                },
                .pointer => {
                    if (type_.cast(.pointer).ref) {
                        return val;
                    } else {
                        return self.allocBoxValue(type_id, val);
                    }
                },
                .func_sym => {
                    return val;
                },
                .func_ptr => {
                    return self.allocBoxValue(type_id, val);
                },
                else => {
                    std.debug.panic("TODO: {} {s}", .{type_.kind(), type_.name()});
                },
            }
        },
    }
}

pub inline fn valueCast(ptr: [*]u8) [*]Value {
    return @ptrCast(@alignCast(ptr));
}

pub inline fn valueConstCast(ptr: [*]const u8) [*]const Value {
    return @ptrCast(@alignCast(ptr));
}
