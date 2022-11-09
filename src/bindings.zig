const std = @import("std");
const stdx = @import("stdx");
const builtin = @import("builtin");

const cy = @import("cyber.zig");
const Value = cy.Value;
const vm_ = @import("vm.zig");
const gvm = &vm_.gvm;

const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.bindings);

pub fn bindCore(self: *cy.VM) !void {
    // Init compile time builtins.
    const resize = try self.ensureStructSym("resize");
    var id = try self.addStruct("List");
    std.debug.assert(id == cy.ListS);
    try self.addStructSym(cy.ListS, resize, cy.SymbolEntry.initNativeFunc1(listResize));
    self.iteratorObjSym = try self.ensureStructSym("iterator");
    try self.addStructSym(cy.ListS, self.iteratorObjSym, cy.SymbolEntry.initNativeFunc1(listIterator));
    self.nextObjSym = try self.ensureStructSym("next");
    try self.addStructSym(cy.ListS, self.nextObjSym, cy.SymbolEntry.initNativeFunc1(listNext));
    const add = try self.ensureStructSym("add");
    try self.addStructSym(cy.ListS, add, cy.SymbolEntry.initNativeFunc1(listAdd));
    const sort = try self.ensureStructSym("sort");
    try self.addStructSym(cy.ListS, sort, cy.SymbolEntry.initNativeFunc1(listSort));

    id = try self.addStruct("Map");
    std.debug.assert(id == cy.MapS);
    const remove = try self.ensureStructSym("remove");
    try self.addStructSym(cy.MapS, remove, cy.SymbolEntry.initNativeFunc1(mapRemove));

    id = try self.addStruct("Closure");
    std.debug.assert(id == cy.ClosureS);

    id = try self.addStruct("Lambda");
    std.debug.assert(id == cy.LambdaS);

    id = try self.addStruct("String");
    std.debug.assert(id == cy.StringS);

    id = try self.ensureFuncSym("std.readInput");
    try self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdReadInput));
    id = try self.ensureFuncSym("std.parseCyon");
    try self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdParseCyon));
    id = try self.ensureFuncSym("std.print");
    try self.setFuncSym(id, cy.FuncSymbolEntry.initNativeFunc1(stdPrint));

    try self.ensureGlobalFuncSym("readInput", "std.readInput");
    try self.ensureGlobalFuncSym("parseCyon", "std.parseCyon");
    try self.ensureGlobalFuncSym("print", "std.print");
}

fn stdPrint(self: *cy.VM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const str = self.valueToTempString(args[0]);
    std.io.getStdOut().writer().print("{s}\n", .{str}) catch stdx.fatal();
    gvm.release(args[0], false);
    return Value.initNone();
}

fn stdReadInput(_: *cy.VM, _: [*]const Value, _: u8) Value {
    const input = std.io.getStdIn().readToEndAlloc(gvm.alloc, 10e8) catch stdx.fatal();
    return gvm.allocOwnedString(input) catch stdx.fatal();
}

fn stdParseCyon(_: *cy.VM, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    const str = gvm.valueAsString(args[0]);
    defer gvm.release(args[0], false);

    var parser = cy.Parser.init(gvm.alloc);
    defer parser.deinit();
    const val = cy.decodeCyon(gvm.alloc, &parser, str) catch stdx.fatal();
    return fromCyonValue(gvm, val) catch stdx.fatal();
}

fn fromCyonValue(self: *cy.VM, val: cy.DecodeValueIR) !Value {
    switch (val.getValueType()) {
        .list => {
            var dlist = val.asList() catch stdx.fatal();
            defer dlist.deinit();
            const elems = try self.alloc.alloc(Value, dlist.arr.len);
            for (elems) |*elem, i| {
                elem.* = try fromCyonValue(self, dlist.getIndex(i));
            }
            return try self.allocOwnedList(elems);
        },
        .map => {
            var dmap = val.asMap() catch stdx.fatal();
            defer dmap.deinit();
            var iter = dmap.iterator();

            const mapVal = try self.allocEmptyMap();
            const map = stdx.ptrCastAlign(*cy.HeapObject, mapVal.asPointer().?);
            while (iter.next()) |entry| {
                const child = try fromCyonValue(self, dmap.getValue(entry.key_ptr.*));
                const key = try self.allocString(entry.key_ptr.*);
                stdMapPut(self, map, key, child);
            }
            return mapVal;
        },
        .string => {
            const str = val.allocString();
            log.debug("cyon string {s}", .{str});
            return try self.allocOwnedString(str);
        },
        .number => {
            return Value.initF64(try val.asF64());
        },
    }
}

fn stdMapPut(self: *cy.VM, obj: *cy.HeapObject, key: Value, value: Value) void {
    const map = stdx.ptrCastAlign(*cy.MapInner, &obj.map.inner); 
    map.put(self.alloc, self, key, value) catch stdx.fatal();
}

fn listSort(_: *cy.VM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }

    const vm = gvm;
    const obj = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const list = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &obj.retainedList.list);
    const Context = struct {
        lessFn: Value,
        vm: *cy.VM,
    };
    var ctx = Context{
        .lessFn = args[0],
        .vm = vm,
    };
    const S = struct {
        fn less(ctx_: *Context, a: Value, b: Value) bool {
            ctx_.vm.ensureUnusedStackSpace(3) catch stdx.fatal();
            ctx_.vm.stack.top += 3;
            ctx_.vm.retain(a);
            ctx_.vm.retain(b);
            ctx_.vm.stack.buf[ctx_.vm.stack.top-3] = a;
            ctx_.vm.stack.buf[ctx_.vm.stack.top-2] = b;
            ctx_.vm.stack.buf[ctx_.vm.stack.top-1] = ctx_.lessFn;
            const retInfo = ctx_.vm.buildReturnInfo(1, false);
            ctx_.vm.call(ctx_.lessFn, 3, retInfo) catch stdx.fatal();
            @call(.{ .modifier = .never_inline }, vm_.evalLoopGrowStack, .{false}) catch unreachable;
            const res = ctx_.vm.popRegister();
            return res.toBool();
        }
    };
    std.sort.sort(Value, list.items, &ctx, S.less);
    vm.release(ctx.lessFn, false);
    return Value.initNone();
}

fn listAdd(self: *cy.VM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &list.retainedList.list);
    inner.append(self.alloc, args[0]) catch stdx.fatal();
    return Value.initNone();
}

fn listNext(self: *cy.VM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    _ = self;
    _ = args;
    _ = nargs;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    if (list.retainedList.nextIterIdx < list.retainedList.list.len) {
        defer list.retainedList.nextIterIdx += 1;
        return list.retainedList.list.ptr[list.retainedList.nextIterIdx];
    } else return Value.initNone();
}

fn listIterator(self: *cy.VM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = self;
    _ = args;
    _ = nargs;
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    list.retainedList.nextIterIdx = 0;
    return Value.initPtr(ptr);
}

fn listResize(self: *cy.VM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*std.ArrayListUnmanaged(Value), &list.retainedList.list);
    const size = @floatToInt(u32, args[0].toF64());
    inner.resize(self.alloc, size) catch stdx.fatal();
    return Value.initNone();
}

fn mapRemove(self: *cy.VM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    @setRuntimeSafety(debug);
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const obj = stdx.ptrCastAlign(*cy.HeapObject, ptr);
    const inner = stdx.ptrCastAlign(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(self, args[0]);
    return Value.initNone();
}