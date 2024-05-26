// Copyright (c) 2023 Cyber (See LICENSE)

const std = @import("std");
const stdx = @import("stdx");
const fatal = cy.fatal;
const builtin = @import("builtin");

const cy = @import("../cyber.zig");
const vmc = cy.vmc;
const types = cy.types;
const rt = cy.rt;
const sema = cy.sema;
const bt = cy.types.BuiltinTypes;
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const fmt = @import("../fmt.zig");
const cc = @import("../capi.zig");

const debug = builtin.mode == .Debug;
const log = cy.log.scoped(.bindings);

const NullId = std.math.maxInt(u32);

pub const Symbol = enum {
    b,
    o,
    d,
    x,
    c,

    bool,
    char,
    uchar,
    short,
    ushort,
    int,
    uint,
    long,
    ulong,
    usize,
    double,
    charPtr,
    voidPtr,
    funcPtr,
    void,

    little,
    big,

    left,
    right,
    ends,

    // Encoding.
    utf8,
    bytes,

    AssertError,
    EvalError,
    FileNotFound,
    MissingSymbol,
    EndOfStream,
    OutOfBounds,
    InvalidResult,
    InvalidArgument,
    InvalidSignature,
    InvalidRune,
    StreamTooLong,
    NotAllowed,
    Closed,
    PermissionDenied,
    UnknownError,
    Unicode,

    running,
    paused,
    done,

    float,

    // Open modes.
    read,
    write,
    readWrite,

    // File types.
    file,
    dir,

    unknown,
};

const TupleNone = cy.builtins.TupleNone;
const TupleSome = cy.builtins.TupleSome;
const anyNone = cy.builtins.anyNone;
const anySome = cy.builtins.anySome;

pub fn getBuiltinSymbol(id: u32) ?Symbol {
    return std.meta.intToEnum(Symbol, id) catch {
        return null;
    };
}

pub fn prepareThrowSymbol(vm: *cy.UserVM, sym: Symbol) Value {
    return vm.prepareThrowSymbol(@intFromEnum(sym));
}  

pub fn bindCore(self: *cy.VM) !void {
    @setCold(true);
    for (std.enums.values(Symbol)) |sym| {
        try ensureSymbol(self, @tagName(sym), sym);
    }
}

fn ensureSymbol(vm: *cy.VM, name: []const u8, sym: Symbol) !void {
    const id = try vm.ensureSymbol(name);
    std.debug.assert(id == @intFromEnum(sym));
}

// TODO: Provide sort where the sort fields and compare strategy are provided instead of a lessFn,
//       since Cyber's VM is non reentrant.
pub fn listSort(vm: *cy.VM, args: [*]const Value, nargs: u8) Value {
    const obj = args[0].asHeapObject();
    const list = cy.ptrAlignCast(*cy.List(Value), &obj.list.list);
    const LessContext = struct {
        lessFn: Value,
        vm: *cy.UserVM,
        newFramePtr: u32,
    };
    var lessCtx = LessContext{
        .lessFn = args[1],
        .vm = vm,
        .newFramePtr = vm.getNewFramePtrOffset(args, nargs),
    };

    const S = struct {
        fn less(ctx_: *LessContext, a: Value, b: Value) bool {
            const res = ctx_.vm.callFunc(ctx_.newFramePtr, ctx_.lessFn, &.{a, b}) catch {
                return false;
            };
            return res.toBool();
        }
    };
    std.sort.pdq(Value, list.items(), &lessCtx, S.less);
    return Value.None;
}

pub fn listRemove(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index >= inner.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    } 
    vm.release(inner.buf[@intCast(index)]);
    inner.remove(@intCast(index));
    return Value.Void;
}

/// `mapIndex` with a different error message.
pub fn getGlobal(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const map = args[0].asHeapObject();
    if (map.map.map().get(args[1])) |val| {
        vm.retain(val);
        return val;
    } else {
        return vm.prepPanic("Variable is not defined in `$global`.");
    }
}

pub fn tableInitPair(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const table = args[0].asHeapObject();
    try table.table.set(vm, args[1], args[2]);
    return Value.Void;
}

pub fn tableGet(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const table = args[0].asHeapObject();
    const name = args[1].asString();
    if (table.table.map().getByString(name)) |val| {
        vm.retain(val);
        return val;
    } else {
        return vm.prepPanic("Missing field.");
    }
    return Value.Void;
}

pub fn tableSet(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const table = args[0].asHeapObject();
    try table.table.set(vm, args[1], args[2]);
    return Value.Void;
}

pub fn tableIndex(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const table = args[0].asHeapObject();
    if (table.table.map().get(args[1])) |val| {
        vm.retain(val);
        return val;
    } else {
        return vm.prepPanic("Missing field.");
    }
    return Value.Void;
}

pub fn mapIndex(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const map = args[0].asHeapObject();
    if (map.map.map().get(args[1])) |val| {
        vm.retain(val);
        return val;
    } else {
        return vm.prepPanic("Missing key in map.");
    }
}

pub fn mapSetIndex(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const map = args[0].asHeapObject();
    try map.map.set(vm, args[1], args[2]);
    return Value.Void;
}

pub fn tupleIndex(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const tuple = args[0].asHeapObject();
    if (index < 0 or index > tuple.tuple.len) {
        return vm.prepPanic("Out of bounds.");
    } 
    const value = tuple.tuple.getElemsPtr()[@intCast(index)];
    vm.retain(value);
    return value;
}

pub fn listIndex(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return vm.prepPanic("Out of bounds.");
    } 
    const value = inner.buf[@intCast(index)];
    vm.retain(value);
    return value;
}

pub fn listSetIndex(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return vm.prepPanic("Out of bounds.");
    } 
    vm.release(inner.buf[@intCast(index)]);
    vm.retain(args[2]);
    inner.buf[@intCast(index)] = args[2];
    return Value.Void;
}

pub fn listSlice(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const list = args[0].asHeapObject();
    const range = args[1].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    var start: i64 = undefined;
    if (range.range.has_start) {
        start = range.range.start;
        if (start < 0) {
            return vm.prepPanic("Out of bounds.");
        }
    } else {
        start = 0;
    }

    var end: i64 = undefined;
    if (range.range.has_end) {
        end = range.range.end;
        if (end > inner.len) {
            return vm.prepPanic("Out of bounds.");
        }
    } else {
        end = @intCast(inner.len);
    }

    if (end < start) {
        return vm.prepPanic("Out of bounds.");
    }

    const elems = inner.buf[@intCast(start)..@intCast(end)];
    for (elems) |elem| {
        vm.retain(elem);
    }
    return cy.heap.allocListDyn(vm, elems);
}

pub fn listInsert(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const value = args[2];
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    } 
    inner.growTotalCapacity(vm.alloc, inner.len + 1) catch cy.fatal();
    vm.retain(value);
    inner.insertAssumeCapacity(@intCast(index), value);
    return Value.Void;
}

pub fn listJoin(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const items = obj.list.items();
    if (items.len > 0) {
        var is_ascii = args[1].asHeapObject().string.getType().isAstring();
        const sep = args[1].asString();

        // First record length.
        var byteLen: u32 = 0;
        var out_ascii: bool=  undefined;

        // Record first string part.
        var str = try vm.getOrBufPrintValueStr2(&cy.tempBuf, items[0], &out_ascii);
        is_ascii = is_ascii and out_ascii;
        byteLen += @intCast(str.len);

        // Record other string parts.
        for (items[1..]) |item| {
            str = try vm.getOrBufPrintValueStr2(&cy.tempBuf, item, &out_ascii);
            is_ascii = is_ascii and out_ascii;
            byteLen += @intCast(str.len);
        }
        byteLen += @intCast(sep.len * (items.len-1));

        // Allocate final buffer and perform join.
        var newObj: *cy.HeapObject = undefined;
        var buf: []u8 = undefined;

        if (is_ascii) {
            newObj = try vm.allocUnsetAstringObject(byteLen);
            buf = newObj.astring.getMutSlice();
        } else {
            newObj = try vm.allocUnsetUstringObject(byteLen);
            buf = newObj.ustring.getMutSlice();
        }

        // Copy.
        str = try vm.getOrBufPrintValueStr(&cy.tempBuf, items[0]);
        @memcpy(buf[0..str.len], str);

        var dst: usize = str.len;
        for (items[1..]) |item| {
            @memcpy(buf[dst..dst+sep.len], sep);
            dst += sep.len;

            str = try vm.getOrBufPrintValueStr(&cy.tempBuf, item);
            @memcpy(buf[dst..dst+str.len], str);
            dst += str.len;
        }
        return Value.initNoCycPtr(newObj);
    } else {
        const empty = vm.emptyString;
        vm.retain(empty);
        return empty;
    }
}

pub fn listAppendAll(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const list = args[1].asHeapObject();
    for (list.list.items()) |it| {
        vm.retain(it);
        try obj.list.append(vm.alloc, it);
    }
    return Value.Void;
}

pub fn listAppend(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    vm.retain(args[1]);
    try obj.list.append(vm.alloc, args[1]);
    return Value.Void;
}

pub fn listIteratorNext(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const list = &obj.listIter.inner.list.asHeapObject().list;
    if (obj.listIter.inner.nextIdx < list.list.len) {
        defer obj.listIter.inner.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.inner.nextIdx];
        vm.retain(val);
        return anySome(vm, val) catch cy.fatal();
    } else return anyNone(vm) catch cy.fatal();
}

pub fn listIterator(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    vm.retain(args[0]);
    return vm.allocListIter(@intCast(args[1].asInteger()), args[0]) catch fatal();
}

pub fn listResize(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const recv = args[0];
    const list = recv.asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    const size: u32 = @intCast(args[1].asInteger());
    if (inner.len < size) {
        const oldLen = inner.len;
        inner.resize(vm.alloc, size) catch cy.fatal();
        for (inner.items()[oldLen..size]) |*item| {
            item.* = Value.initInt(0);
        }
    } else if (inner.len > size) {
        // Remove items.
        for (inner.items()[size..inner.len]) |item| {
            vm.release(item);
        }
        inner.resize(vm.alloc, size) catch cy.fatal();
    }
    return Value.Void;
}

pub fn mapIterator(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    vm.retainObject(obj);
    return vm.allocMapIterator(args[0]) catch fatal();
}

pub fn mapIteratorNext(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const map: *cy.ValueMap = @ptrCast(&obj.mapIter.map.castHeapObject(*cy.heap.Map).inner);
    if (map.next(&obj.mapIter.nextIdx)) |entry| {
        vm.retain(entry.key);
        vm.retain(entry.value);
        const res = cy.heap.allocTuple(vm, &.{entry.key, entry.value}) catch cy.fatal();
        return TupleSome(vm, res) catch cy.fatal();
    } else return TupleNone(vm) catch cy.fatal();
}

pub fn mapSize(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    return Value.initInt(@intCast(inner.size));
}

pub fn mapRemove(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    const removed = inner.remove(vm, args[1]);
    return Value.initBool(removed);
}

pub fn listLen(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    return Value.initInt(@intCast(inner.len));
}

// Keep as reference in case resume should be a function call.
// Although it works, it requires native func calls to perform additional copies of pc and framePtr back to the eval loop,
// which is a bad tradeoff for every other function call that doesn't need to.
// One solution is to add another bytecode to call nativeFunc1 with control over execution context.
// fn fiberResume(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) Value {
//     const obj = recv.asHeapObject();
//     if (&obj.fiber != @ptrCast(*cy.VM, vm).curFiber) {
//         // Only resume fiber if it's not done.
//         if (obj.fiber.pc != NullId) {
//             // Obtain the startLocal from looking at previous inst operand.
//             const startLocal = (@ptrCast(*cy.VM, vm).pc - 14 + 1)[0].arg;
//             // Obtain previous framePtr by subtracting from args pointer.
//             const prevFramePtr = @intToPtr([*]Value, @intFromPtr(args - startLocal - 4));

//             const pcOffset = @intCast(u32, @intFromPtr(@ptrCast(*cy.VM, vm).pc) - @intFromPtr(@ptrCast(*cy.VM, vm).ops.ptr));
//             const res = cy.pushFiber(@ptrCast(*cy.VM, vm), pcOffset, prevFramePtr, &obj.fiber, startLocal);
//             @ptrCast(*cy.VM, vm).pc = res.pc;
//             @ptrCast(*cy.VM, vm).framePtr = res.framePtr;
//             return Value.None;
//         }
//     }
//     vm.releaseObject(obj);
//     return Value.None;
// }

pub fn mapContains(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    return Value.initBool(inner.contains(args[1]));
}

pub fn mapGet(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    if (inner.get(args[1])) |val| {
        vm.retain(val);
        return anySome(vm, val) catch cy.fatal();
    } else return anyNone(vm) catch cy.fatal();
}

pub fn intNeg(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(-args[0].asInteger());
}

pub fn intLess(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asInteger() < args[1].asInteger());
}

pub fn intLessEq(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asInteger() <= args[1].asInteger());
}

pub fn intGreater(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asInteger() > args[1].asInteger());
}

pub fn intGreaterEq(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asInteger() >= args[1].asInteger());
}

pub fn intAdd(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(args[0].asInteger() + args[1].asInteger());
}

pub fn intSub(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(args[0].asInteger() - args[1].asInteger());
}

pub fn intMul(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(args[0].asInteger() * args[1].asInteger());
}

pub fn intDiv(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const right = args[1].asInteger();
    if (right == 0) return vm.prepPanic("Division by zero.");
    return Value.initInt(@divTrunc(args[0].asInteger(), right));
}

pub fn intMod(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const right = args[1].asInteger();
    if (right == 0) return vm.prepPanic("Division by zero.");
    return Value.initInt(@mod(args[0].asInteger(), right));
}

pub fn intPow(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const right = args[1].asInteger();
    if (right == 0) return vm.prepPanic("Division by zero.");
    return Value.initInt(std.math.powi(i48, args[0].asInteger(), right) catch |err| {
        switch (err) {
            error.Underflow => return vm.prepPanic("Underflow."),
            error.Overflow => return vm.prepPanic("Overfloat."),
        }
    });
}

pub fn intAnd(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(args[0].asInteger() & args[1].asInteger());
}

pub fn intOr(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(args[0].asInteger() | args[1].asInteger());
}

pub fn intXor(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(args[0].asInteger() ^ args[1].asInteger());
}

pub fn intLeftShift(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const right = args[1].asInteger();
    if (right > 48 or right < 0) return vm.prepPanic("Out of bounds.");
    return Value.initInt(args[0].asInteger() << @intCast(right));
}

pub fn intRightShift(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const right = args[1].asInteger();
    if (right > 48 or right < 0) return vm.prepPanic("Out of bounds.");
    return Value.initInt(args[0].asInteger() >> @intCast(right));
}

pub fn intNot(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initInt(~args[0].asInteger());
}

pub fn floatNeg(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(-args[0].asF64());
}

pub fn floatLess(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asF64() < args[1].asF64());
}

pub fn floatLessEq(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asF64() <= args[1].asF64());
}

pub fn floatGreater(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asF64() > args[1].asF64());
}

pub fn floatGreaterEq(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].asF64() >= args[1].asF64());
}

pub fn floatAdd(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(args[0].asF64() + args[1].asF64());
}

pub fn floatSub(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(args[0].asF64() - args[1].asF64());
}

pub fn floatMul(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(args[0].asF64() * args[1].asF64());
}

pub fn floatDiv(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(args[0].asF64() / args[1].asF64());
}

pub fn floatMod(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(@mod(args[0].asF64(), args[1].asF64()));
}

pub fn floatPow(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initF64(std.math.pow(f64, args[0].asF64(), args[1].asF64()));
}

pub const QuickenType = enum(u8) {
};

pub fn nop(vm: *cy.UserVM, _: [*]const Value, _: u8) Value {
    return vm.returnPanic("Unsupported.");
}

pub const ModuleBuilder = struct {
    sym: *cy.Sym,
    chunk: *cy.Chunk,
    compiler: *cy.Compiler,
    vm: *cy.VM,

    pub fn init(c: *cy.Compiler, symOpt: ?*cy.Sym) ModuleBuilder {
        var new = ModuleBuilder{
            .sym = undefined,
            .compiler = c,
            .chunk = undefined,
            .vm = c.vm,
        };
        if (symOpt) |sym| {
            const chunk = sym.getMod().?.chunk;
            new.chunk = chunk;
            new.sym = sym;
        }
        return new;
    }

    pub fn setVar(self: *const ModuleBuilder, name: []const u8, typeId: types.TypeId, val: Value) !void {
        try self.getMod().setTypedVar(self.compiler, name, typeId, val);
    }

    pub fn declareFuncSig(self: *const ModuleBuilder, name: [*:0]const u8, params: []const types.TypeId, ret: types.TypeId, ptr: cy.ZHostFuncFn) !void {
        cc.declareFunc(self.sym.toC(), name, params.ptr, params.len, ret, @ptrCast(ptr));
    }

    pub fn ensureMethodGroup(self: *const ModuleBuilder, name: []const u8) !vmc.MethodGroupId {
        return self.vm.ensureMethodGroup(name);
    }

    pub fn addOptimizingMethod(
        self: *const ModuleBuilder, typeId: cy.TypeId, mgId: vmc.MethodGroupId,
        params: []const types.TypeId, ret: types.TypeId, ptr: cy.QuickenFuncFn,
    ) !void {
        const funcSigId = try sema.ensureFuncSig(self.compiler, params, ret);
        const funcSig = self.compiler.sema.getFuncSig(funcSigId);
        if (funcSig.isParamsTyped) {
            return error.Unsupported;
        }
        try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostQuicken(funcSigId, ptr, @intCast(params.len)));
    }

    pub fn addMethod(
        self: *const ModuleBuilder, typeId: cy.TypeId, mgId: vmc.MethodGroupId,
        params: []const types.TypeId, ret: types.TypeId, ptr: cy.ZHostFuncFn,
    ) !void {
        const funcSigId = try self.compiler.sema.ensureFuncSig(params, ret);
        const funcSig = self.compiler.sema.getFuncSig(funcSigId);
        if (funcSig.reqCallTypeCheck) {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostTyped(funcSigId, ptr, @intCast(params.len)));
        } else {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostUntyped(funcSigId, ptr, @intCast(params.len)));
        }
    }

    pub fn getMod(self: *const ModuleBuilder) *cy.Module {
        return self.compiler.sema.getModulePtr(self.modId);
    }

    pub fn createAndSetTypeObject(self: *const ModuleBuilder, name: []const u8, fields: []const []const u8) !cy.TypeId {
        const sym = try self.chunk.declareObjectType(self.sym, name, cy.NullId);

        const modFields = try self.compiler.alloc.alloc(cy.sym.FieldInfo, fields.len);

        for (fields, 0..) |field, i| {
            const id = try self.vm.ensureFieldSym(field);
            try self.vm.addFieldSym(sym.type, id, @intCast(i), bt.Any);
            _ = try self.chunk.declareField(@ptrCast(sym), field, @intCast(i), bt.Any, cy.NullId);
        }
        sym.fields = modFields.ptr;
        sym.numFields = @intCast(modFields.len);
        return sym.type;
    }
};
