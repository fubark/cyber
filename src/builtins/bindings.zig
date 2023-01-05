const std = @import("std");
const stdx = @import("stdx");
const builtin = @import("builtin");

const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const gvm = &vm_.gvm;
const fmt = @import("../fmt.zig");

const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.bindings);

const NullId = std.math.maxInt(u32);

pub const TagLit = enum {
    int,
    bool,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    f32,
    f64,
    float,
    double,
    charPtrZ,
    ptr,
    void,

    little,
    big,

    AssertError,
    NotFound,
    MissingSymbol,
    EndOfStream,
    OutOfBounds,

    running,
    paused,
    done,

    err,
    number,
    object,
    map,
};

const StdSection = cy.StdSection;
const Section = cy.Section;

// This keeps .eval section first in order.
pub export fn forceSectionDep() linksection(cy.HotSection) callconv(.C) void {}

pub fn bindCore(self: *cy.VM) !void {
    @setCold(true);
    forceSectionDep();

    const resize = try self.ensureMethodSymKey("resize", 1);
    self.iteratorObjSym = try self.ensureMethodSymKey("iterator", 0);
    self.nextObjSym = try self.ensureMethodSymKey("next", 0);
    self.pairIteratorObjSym = try self.ensureMethodSymKey("pairIterator", 0);
    self.nextPairObjSym = try self.ensureMethodSymKey("nextPair", 0);
    const add = try self.ensureMethodSymKey("add", 1);
    const insert = try self.ensureMethodSymKey("insert", 2);
    const remove = try self.ensureMethodSymKey("remove", 1);
    const sort = try self.ensureMethodSymKey("sort", 1);
    const size = try self.ensureMethodSymKey("size", 0);
    const len = try self.ensureMethodSymKey("len", 0);
    const charAt = try self.ensureMethodSymKey("charAt", 1);
    const status = try self.ensureMethodSymKey("status", 0);
    const streamLines = try self.ensureMethodSymKey("streamLines", 0);
    const streamLines1 = try self.ensureMethodSymKey("streamLines", 1);

    // Init compile time builtins.

    // Primitive types.
    var id = try self.addStruct("none");
    std.debug.assert(id == cy.NoneT);
    id = try self.addStruct("boolean");
    std.debug.assert(id == cy.BooleanT);
    id = try self.addStruct("error");
    std.debug.assert(id == cy.ErrorT);
    id = try self.addStruct("conststring");
    std.debug.assert(id == cy.ConstStringT);
    try self.addMethodSym(cy.ConstStringT, len, cy.SymbolEntry.initNativeFunc1(constStringLen));
    try self.addMethodSym(cy.ConstStringT, charAt, cy.SymbolEntry.initNativeFunc1(constStringCharAt));
    id = try self.addStruct("tag");
    std.debug.assert(id == cy.UserTagT);
    id = try self.addStruct("tagliteral");
    std.debug.assert(id == cy.UserTagLiteralT);
    id = try self.addStruct("integer");
    std.debug.assert(id == cy.IntegerT);
    id = try self.addStruct("number");
    std.debug.assert(id == cy.NumberT);

    id = try self.addStruct("List");
    std.debug.assert(id == cy.ListS);
    try self.addMethodSym(cy.ListS, resize, cy.SymbolEntry.initNativeFunc1(listResize));
    try self.addMethodSym(cy.ListS, self.iteratorObjSym, cy.SymbolEntry.initNativeFunc1(listIterator));
    try self.addMethodSym(cy.ListS, self.nextObjSym, cy.SymbolEntry.initNativeFunc1(listNext));
    try self.addMethodSym(cy.ListS, self.pairIteratorObjSym, cy.SymbolEntry.initNativeFunc1(listIterator));
    try self.addMethodSym(cy.ListS, self.nextPairObjSym, cy.SymbolEntry.initNativeFunc2(listNextPair));
    try self.addMethodSym(cy.ListS, add, cy.SymbolEntry.initNativeFunc1(listAdd));
    try self.addMethodSym(cy.ListS, insert, cy.SymbolEntry.initNativeFunc1(listInsert));
    try self.addMethodSym(cy.ListS, remove, cy.SymbolEntry.initNativeFunc1(listRemove));
    try self.addMethodSym(cy.ListS, sort, cy.SymbolEntry.initNativeFunc1(listSort));
    try self.addMethodSym(cy.ListS, len, cy.SymbolEntry.initNativeFunc1(listLen));

    id = try self.addStruct("Map");
    std.debug.assert(id == cy.MapS);
    try self.addMethodSym(cy.MapS, remove, cy.SymbolEntry.initNativeFunc1(mapRemove));
    try self.addMethodSym(cy.MapS, size, cy.SymbolEntry.initNativeFunc1(mapSize));
    try self.addMethodSym(cy.MapS, self.iteratorObjSym, cy.SymbolEntry.initNativeFunc1(mapIterator));
    try self.addMethodSym(cy.MapS, self.pairIteratorObjSym, cy.SymbolEntry.initNativeFunc1(mapIterator));
    try self.addMethodSym(cy.MapS, self.nextObjSym, cy.SymbolEntry.initNativeFunc1(mapNext));
    try self.addMethodSym(cy.MapS, self.nextPairObjSym, cy.SymbolEntry.initNativeFunc2(mapNextPair));

    id = try self.addStruct("Closure");
    std.debug.assert(id == cy.ClosureS);

    id = try self.addStruct("Lambda");
    std.debug.assert(id == cy.LambdaS);

    id = try self.addStruct("String");
    std.debug.assert(id == cy.StringS);
    try self.addMethodSym(cy.StringS, len, cy.SymbolEntry.initNativeFunc1(stringLen));
    try self.addMethodSym(cy.StringS, charAt, cy.SymbolEntry.initNativeFunc1(stringCharAt));

    id = try self.addStruct("Fiber");
    std.debug.assert(id == cy.FiberS);
    try self.addMethodSym(cy.FiberS, status, cy.SymbolEntry.initNativeFunc1(fiberStatus));

    id = try self.addStruct("Box");
    std.debug.assert(id == cy.BoxS);

    id = try self.addStruct("NativeFunc1");
    std.debug.assert(id == cy.NativeFunc1S);

    id = try self.addStruct("TccState");
    std.debug.assert(id == cy.TccStateS);

    id = try self.addStruct("OpaquePtr");
    std.debug.assert(id == cy.OpaquePtrS);

    id = try self.addStruct("File");
    std.debug.assert(id == cy.FileT);
    try self.addMethodSym(cy.FileT, streamLines, cy.SymbolEntry.initNativeFunc1(fileStreamLines));
    try self.addMethodSym(cy.FileT, streamLines1, cy.SymbolEntry.initNativeFunc1(fileStreamLines1));
    try self.addMethodSym(cy.FileT, self.iteratorObjSym, cy.SymbolEntry.initNativeFunc1(fileIterator));
    try self.addMethodSym(cy.FileT, self.nextObjSym, cy.SymbolEntry.initNativeFunc1(fileNext));

    const sid = try self.ensureStruct("CFunc");
    self.structs.buf[sid].numFields = 3;
    id = try self.ensureFieldSym("sym");
    try self.addFieldSym(sid, id, 0);
    id = try self.ensureFieldSym("args");
    try self.addFieldSym(sid, id, 1);
    id = try self.ensureFieldSym("ret");
    try self.addFieldSym(sid, id, 2);

    try ensureTagLitSym(self, "int", .int);
    try ensureTagLitSym(self, "bool", .bool);
    try ensureTagLitSym(self, "i8", .i8);
    try ensureTagLitSym(self, "u8", .u8);
    try ensureTagLitSym(self, "i16", .i16);
    try ensureTagLitSym(self, "u16", .u16);
    try ensureTagLitSym(self, "i32", .i32);
    try ensureTagLitSym(self, "u32", .u32);
    try ensureTagLitSym(self, "f32", .f32);
    try ensureTagLitSym(self, "f64", .f64);
    try ensureTagLitSym(self, "float", .float);
    try ensureTagLitSym(self, "double", .double);
    try ensureTagLitSym(self, "charPtrZ", .charPtrZ);
    try ensureTagLitSym(self, "ptr", .ptr);
    try ensureTagLitSym(self, "void", .void);

    try ensureTagLitSym(self, "little", .little);
    try ensureTagLitSym(self, "big", .big);

    try ensureTagLitSym(self, "AssertError", .AssertError);
    try ensureTagLitSym(self, "NotFound", .NotFound);
    try ensureTagLitSym(self, "MissingSymbol", .MissingSymbol);
    try ensureTagLitSym(self, "EndOfStream", .EndOfStream);
    try ensureTagLitSym(self, "OutOfBounds", .OutOfBounds);

    try ensureTagLitSym(self, "running", .running);
    try ensureTagLitSym(self, "paused", .paused);
    try ensureTagLitSym(self, "done", .done);

    try ensureTagLitSym(self, "error", .err);
    try ensureTagLitSym(self, "number", .number);
    try ensureTagLitSym(self, "object", .object);
    try ensureTagLitSym(self, "map", .map);
}

fn ensureTagLitSym(vm: *cy.VM, name: []const u8, tag: TagLit) !void {
    const id = try vm.ensureTagLitSym(name);
    std.debug.assert(id == @enumToInt(tag));
}

fn listSort(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }

    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
    const LessContext = struct {
        lessFn: Value,
        vm: *cy.UserVM,
        newFramePtr: u32,
    };
    var lessCtx = LessContext{
        .lessFn = args[0],
        .vm = vm,
        .newFramePtr = vm.getNewFramePtrOffset(args),
    };

    const S = struct {
        fn less(ctx_: *LessContext, a: Value, b: Value) bool {
            const res = ctx_.vm.callFunc(ctx_.newFramePtr, ctx_.lessFn, &.{a, b}) catch stdx.fatal();
            return res.toBool();
        }
    };
    std.sort.sort(Value, list.items(), &lessCtx, S.less);
    vm.releaseObject(obj);
    return Value.None;
}

fn listRemove(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(cy.Section) Value {
    _ = nargs;
    const index = @floatToInt(i64, args[0].toF64());
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(list);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index >= inner.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    vm.release(inner.buf[@intCast(usize, index)]);
    inner.remove(@intCast(usize, index));
    return Value.None;
}

fn listInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(cy.Section) Value {
    _ = nargs;
    const index = @floatToInt(i64, args[0].toF64());
    const value = args[1];
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(list);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    if (inner.len == inner.buf.len) {
        inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch stdx.fatal();
    }
    inner.insertAssumeCapacity(@intCast(usize, index), value);
    return Value.None;
}

fn listAdd(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (inner.len == inner.buf.len) {
        // After reaching a certain size, use power of two ceil.
        // This reduces allocations for big lists while not over allocating for smaller lists.
        if (inner.len > 512) {
            const newCap = std.math.ceilPowerOfTwo(u32, @intCast(u32, inner.len) + 1) catch stdx.fatal();
            inner.growTotalCapacityPrecise(gvm.alloc, newCap) catch stdx.fatal();
        } else {
            inner.growTotalCapacity(gvm.alloc, inner.len + 1) catch stdx.fatal();
        }
    }
    inner.appendAssumeCapacity(args[0]);
    vm.releaseObject(list);
    return Value.None;
}

fn listNextPair(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) cy.ValuePair {
    _ = args;
    _ = nargs;
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(list);
    if (list.list.nextIterIdx < list.list.list.len) {
        defer list.list.nextIterIdx += 1;
        const val = list.list.list.ptr[list.list.nextIterIdx];
        vm.retain(val);
        return .{
            .left = Value.initF64(@intToFloat(f64, list.list.nextIterIdx)),
            .right = val,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn listNext(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = args;
    _ = nargs;
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(list);
    if (list.list.nextIterIdx < list.list.list.len) {
        defer list.list.nextIterIdx += 1;
        const val = list.list.list.ptr[list.list.nextIterIdx];
        vm.retain(val);
        return val;
    } else return Value.None;
}

fn listIterator(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = args;
    _ = nargs;
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    list.list.nextIterIdx = 0;
    return Value.initPtr(ptr);
}

fn listResize(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    const size = @floatToInt(u32, args[0].toF64());
    inner.resize(gvm.alloc, size) catch stdx.fatal();
    vm_.releaseObject(gvm, list);
    return Value.None;
}

fn mapIterator(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(Section) Value {
    _ = nargs;
    _ = args;
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    obj.map.inner.extra = 0;
    return Value.initPtr(ptr);
}

fn mapNextPair(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(Section) cy.ValuePair {
    _ = args;
    _ = nargs;
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const map = @ptrCast(*cy.ValueMap, &obj.map.inner);
    if (map.next()) |entry| {
        vm.retain(entry.key);
        vm.retain(entry.value);
        return .{
            .left = entry.key,
            .right = entry.value,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn mapNext(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(Section) Value {
    _ = args;
    _ = nargs;
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const map = @ptrCast(*cy.ValueMap, &obj.map.inner);
    if (map.next()) |entry| {
        vm.retain(entry.value);
        return entry.value;
    } else return Value.None;
}

fn mapSize(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    _ = nargs;
    _ = args;
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    vm_.releaseObject(gvm, obj);
    return Value.initF64(@intToFloat(f64, inner.size));
}

fn mapRemove(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) Value {
    if (nargs == 0) {
        stdx.panic("Args mismatch");
    }
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(@ptrCast(*cy.VM, vm), args[0]);
    vm.releaseObject(obj);
    return Value.None;
}

fn listLen(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(Section) Value {
    _ = nargs;
    _ = args;
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    vm_.releaseObject(gvm, list);
    return Value.initF64(@intToFloat(f64, inner.len));
}


// Keep as reference in case resume should be a function call.
// Although it works, it requires native func calls to perform additional copies of pc and framePtr back to the eval loop,
// which is a bad tradeoff for every other function call that doesn't need to.
// One solution is to add another bytecode to call nativeFunc1 with control over execution context.
// fn fiberResume(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.HotSection) Value {
//     const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
//     if (&obj.fiber != @ptrCast(*cy.VM, vm).curFiber) {
//         // Only resume fiber if it's not done.
//         if (obj.fiber.pc != NullId) {
//             // Obtain the startLocal from looking at previous inst operand.
//             const startLocal = (@ptrCast(*cy.VM, vm).pc - 14 + 1)[0].arg;
//             // Obtain previous framePtr by subtracting from args pointer.
//             const prevFramePtr = @intToPtr([*]Value, @ptrToInt(args - startLocal - 4));

//             const pcOffset = @intCast(u32, @ptrToInt(@ptrCast(*cy.VM, vm).pc) - @ptrToInt(@ptrCast(*cy.VM, vm).ops.ptr));
//             const res = cy.pushFiber(@ptrCast(*cy.VM, vm), pcOffset, prevFramePtr, &obj.fiber, startLocal);
//             @ptrCast(*cy.VM, vm).pc = res.pc;
//             @ptrCast(*cy.VM, vm).framePtr = res.framePtr;
//             return Value.None;
//         }
//     }
//     vm.releaseObject(obj);
//     return Value.None;
// }

fn fiberStatus(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    if (gvm.curFiber == @ptrCast(*cy.Fiber, obj)) {
        return Value.initTagLiteral(@enumToInt(TagLit.running));
    } else {
        // Check if done.
        if (obj.fiber.pc == NullId) {
            return Value.initTagLiteral(@enumToInt(TagLit.done));
        } else {
            return Value.initTagLiteral(@enumToInt(TagLit.paused));
        }
    }
}

fn stringLen(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.initF64(@intToFloat(f64, obj.string.len));
}

fn stringCharAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.initF64(@intToFloat(f64, obj.string.ptr[@floatToInt(u32, args[0].toF64())]));
}

fn constStringLen(_: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const str = val.asConstStr();
    return Value.initF64(@intToFloat(f64, str.len()));
}

fn constStringCharAt(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const str = val.asConstStr();
    const idx = @floatToInt(u32, args[0].toF64());
    return Value.initF64(@intToFloat(f64, gvm.strBuf[str.start + idx]));
}

pub fn fileStreamLines(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, nargs: u8) linksection(StdSection) Value {
    return fileStreamLines1(vm, ptr, &[_]Value{ Value.initF64(@intToFloat(f64, 4096)) }, nargs);
}

pub fn fileStreamLines1(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const bufSize = @floatToInt(u32, args[0].toF64());
    var createReadBuf = true;
    if (obj.file.hasReadBuf) {
        if (bufSize != obj.file.readBufCap) {
            // Cleanup previous buffer.
            vm.allocator().free(obj.file.readBuf[0..obj.file.readBufCap]);
        } else {
            createReadBuf = false;
        }
    }
    // Allocate read buffer.
    obj.file.iterLines = true;
    if (createReadBuf) {
        const readBuf = vm.allocator().alloc(u8, bufSize) catch stdx.fatal();
        obj.file.readBuf = readBuf.ptr;
        obj.file.readBufCap = @intCast(u32, readBuf.len);
        obj.file.hasReadBuf = true;
    }
    return Value.initPtr(ptr);
}

pub fn fileIterator(_: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    obj.file.curPos = 0;
    obj.file.readBufEnd = 0;
    return Value.initPtr(ptr);
}

fn getLineEndCpu(buf: []const u8) ?usize {
    for (buf) |ch, i| {
        if (ch == '\n') {
            return i + 1;
        } else if (ch == '\r') {
            if (i + 1 < buf.len) {
                if (buf[i+1] == '\n') {
                    return i + 2;
                }
            }
            return i + 1;
        }
    }
    return null;
}

fn getLineEnd(buf: []const u8) ?usize {
    if (comptime std.simd.suggestVectorSize(u8)) |VecSize| {
        const MaskInt = std.meta.Int(.unsigned, VecSize);
        var vbuf: @Vector(VecSize, u8) = undefined;
        const lfNeedle: @Vector(VecSize, u8) = @splat(VecSize, @as(u8, '\n'));
        const crNeedle: @Vector(VecSize, u8) = @splat(VecSize, @as(u8, '\r'));
        var i: usize = 0;
        while (i + VecSize <= buf.len) : (i += VecSize) {
            vbuf = buf[i..i+VecSize][0..VecSize].*;
            const lfHits = @bitCast(MaskInt, vbuf == lfNeedle);
            const crHits = @bitCast(MaskInt, vbuf == crNeedle);
            const bitIdx = @ctz(lfHits | crHits);
            if (bitIdx < VecSize) {
                // Found.
                const res = i + bitIdx;
                if (buf[res] == '\n') {
                    return res + 1;
                } else {
                    if (res + 1 < buf.len and buf[res+1] == '\n') {
                        return res + 2;
                    } else {
                        return res + 1;
                    }
                }
            }
        }
        if (i < buf.len) {
            // Remaining use cpu.
            if (getLineEndCpu(buf[i..])) |res| {
                return i + res;
            }
        }
        return null;
    } else {
        return getLineEndCpu(buf);
    }
}

pub fn fileNext(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    if (obj.file.iterLines) {
        const readBuf = obj.file.readBuf[0..obj.file.readBufCap];
        if (getLineEnd(readBuf[obj.file.curPos..obj.file.readBufEnd])) |end| {
            // Found new line.
            const line = vm.allocString(readBuf[obj.file.curPos..obj.file.curPos+end]) catch stdx.fatal();

            // Advance pos.
            obj.file.curPos += @intCast(u32, end);

            return line;
        }

        var lineBuf = std.ArrayList(u8).init(vm.allocator());
        // Start with previous string without line delimiter.
        lineBuf.appendSlice(readBuf[obj.file.curPos..obj.file.readBufEnd]) catch stdx.fatal();

        // Read into buffer.
        const file = std.fs.File{
            .handle = @bitCast(i32, obj.file.fd),
            .capable_io_mode = .blocking,
            .intended_io_mode = .blocking,
        };
        const reader = file.reader();

        while (true) {
            const bytesRead = reader.read(readBuf) catch stdx.fatal();
            if (bytesRead == 0) {
                // End of stream.
                obj.file.iterLines = false;
                if (lineBuf.items.len > 0) {
                    return vm.allocOwnedString(lineBuf.toOwnedSlice() catch stdx.fatal()) catch stdx.fatal();
                } else {
                    return Value.None;
                }
            }
            if (getLineEnd(readBuf[0..bytesRead])) |end| {
                // Found new line.
                lineBuf.appendSlice(readBuf[0..end]) catch stdx.fatal();

                // Advance pos.
                obj.file.curPos = @intCast(u32, end);
                obj.file.readBufEnd = @intCast(u32, bytesRead);

                return vm.allocOwnedString(lineBuf.toOwnedSlice() catch stdx.fatal()) catch stdx.fatal();
            }
        }
    } else {
        return Value.None;
    }
}