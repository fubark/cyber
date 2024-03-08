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
    FileNotFound,
    MissingSymbol,
    EndOfStream,
    OutOfBounds,
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

    @"error",
    float,
    object,
    map,
    list,
    function,
    fiber,
    string,
    array,
    none,
    symbol,
    pointer,
    metatype,

    // Open modes.
    read,
    write,
    readWrite,

    // File types.
    file,
    dir,

    unknown,
};

pub fn getBuiltinSymbol(id: u32) ?Symbol {
    return std.meta.intToEnum(Symbol, id) catch {
        return null;
    };
}

pub fn prepareThrowSymbol(vm: *cy.UserVM, sym: Symbol) Value {
    return vm.prepareThrowSymbol(@intFromEnum(sym));
}  

const StdSection = cy.StdSection;
const Section = cy.Section;

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
pub fn listSort(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
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

pub fn listRemove(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index >= inner.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    } 
    vm.release(inner.buf[@intCast(index)]);
    inner.remove(@intCast(index));
    return Value.None;
}

pub fn listInsert(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
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
    return Value.None;
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
        std.mem.copy(u8, buf[0..str.len], str);

        var dst: usize = str.len;
        for (items[1..]) |item| {
            std.mem.copy(u8, buf[dst..dst+sep.len], sep);
            dst += sep.len;

            str = try vm.getOrBufPrintValueStr(&cy.tempBuf, item);
            std.mem.copy(u8, buf[dst..dst+str.len], str);
            dst += str.len;
        }
        return Value.initNoCycPtr(newObj);
    } else {
        const empty = vm.emptyString;
        vm.retain(empty);
        return empty;
    }
}

pub fn listConcat(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const list = args[1].asHeapObject();
    for (list.list.items()) |it| {
        vm.retain(it);
        try obj.list.append(vm.alloc, it);
    }
    return Value.None;
}

pub fn listIteratorNext(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return val;
    } else return Value.None;
}

pub fn listIterator(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    vm.retainObject(obj);
    return vm.allocListIterator(&obj.list) catch fatal();
}

pub fn listResize(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const recv = args[0];
    const list = recv.asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    const size: u32 = @intCast(args[1].asInteger());
    if (inner.len < size) {
        const oldLen = inner.len;
        inner.resize(vm.alloc, size) catch cy.fatal();
        for (inner.items()[oldLen..size]) |*item| {
            item.* = Value.None;
        }
    } else if (inner.len > size) {
        // Remove items.
        for (inner.items()[size..inner.len]) |item| {
            vm.release(item);
        }
        inner.resize(vm.alloc, size) catch cy.fatal();
    }
    return Value.None;
}

pub fn mapIterator(vm: *cy.VM, args: [*]const Value, _: u8) linksection(Section) Value {
    const obj = args[0].asHeapObject();
    vm.retainObject(obj);
    return vm.allocMapIterator(&obj.map) catch fatal();
}

pub fn mapIteratorNext(vm: *cy.VM, args: [*]const Value, _: u8) linksection(Section) Value {
    const obj = args[0].asHeapObject();
    const map: *cy.ValueMap = @ptrCast(&obj.mapIter.map.inner);
    if (map.next(&obj.mapIter.nextIdx)) |entry| {
        vm.retain(entry.key);
        vm.retain(entry.value);
        return cy.heap.allocTuple(vm, &.{entry.key, entry.value}) catch cy.fatal();
    } else return Value.None;
}

pub fn mapSize(_: *cy.VM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    return Value.initInt(@intCast(inner.size));
}

pub fn mapRemove(vm: *cy.VM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(vm, args[1]);
    return Value.None;
}

pub fn listLen(_: *cy.VM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    return Value.initInt(@intCast(inner.len));
}

// Keep as reference in case resume should be a function call.
// Although it works, it requires native func calls to perform additional copies of pc and framePtr back to the eval loop,
// which is a bad tradeoff for every other function call that doesn't need to.
// One solution is to add another bytecode to call nativeFunc1 with control over execution context.
// fn fiberResume(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.HotSection) Value {
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

inline fn inlineUnaryOp(pc: [*]cy.Inst, code: cy.OpCode) void {
    const ret = pc[1].val;
    // Save callObjSym data.
    pc[8].val = ret;
    pc[9] = pc[2];

    // Inline bin op.
    pc[0] = cy.Inst.initOpCode(code);
    pc[1].val = ret + cy.vm.CallArgStart;
    pc[2].val = ret;
}

pub fn intBitwiseNot(vm: *cy.VM, _: [*]const Value, _: u8) Value {
    inlineUnaryOp(vm.pc, .bitwiseNot);
    return Value.None;
}

pub fn intNeg(vm: *cy.VM, _: [*]const Value, _: u8) Value {
    inlineUnaryOp(vm.pc, .negInt);
    return Value.None;
}

pub fn floatNeg(vm: *cy.VM, _: [*]const Value, _: u8) Value {
    inlineUnaryOp(vm.pc, .negFloat);
    return Value.None;
}

pub fn inlineTernOp(comptime code: cy.OpCode) cy.ZHostFuncFn {
    const S = struct {
        pub fn method(vm: *cy.VM, _: [*]const Value, _: u8) Value {
            const pc = vm.pc;
            const ret = pc[1].val;
            // Save callObjSym data.
            pc[8].val = ret;
            pc[9] = pc[2];
            pc[10] = pc[3];
            pc[11] = pc[4];

            pc[0] = cy.Inst.initOpCode(code);
            pc[1].val = ret + cy.vm.CallArgStart;
            pc[2].val = ret + cy.vm.CallArgStart + 1;
            pc[3].val = ret + cy.vm.CallArgStart + 2;
            pc[4].val = ret;
            return Value.None;
        }
    };
    return S.method;
}

pub const QuickenType = enum(u8) {
    binOp,
    binOpOneCopy,
    binOpBothCopies,
};

fn saveInst(vm: *cy.VM, qtype: QuickenType, pc: [*]cy.Inst) !void {
    var data: []u8 = undefined;
    const pcPtr: [*]const u8 = @ptrCast(pc);
    const instLen = vmc.CALL_OBJ_SYM_INST_LEN;

    switch (qtype) {
        .binOp => {
            data = try vm.alloc.alloc(u8, instLen + 1);
            @memcpy(data[1..], pcPtr[0..instLen]);
        },
        .binOpOneCopy => {
            data = try vm.alloc.alloc(u8, instLen + 3 + 1);
            @memcpy(data[1..], (pcPtr - 3)[0..instLen+3]);
        },
        .binOpBothCopies => {
            data = try vm.alloc.alloc(u8, instLen + 6 + 1);
            @memcpy(data[1..], (pcPtr - 6)[0..instLen + 6]);
        },
    }
    data[0] = @intFromEnum(qtype);
    const pcOff = @intFromPtr(pc) - @intFromPtr(vm.ops.ptr);
    try vm.inlineSaves.put(vm.alloc, @intCast(pcOff), data.ptr);
}

pub fn inlineBinOp(comptime code: cy.OpCode) fn (*cy.VM, [*]const Value, u8) anyerror!Value {
    const S = struct {
        pub fn method(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
            const pc = vm.pc;

            // Lower bits contain op inlining type.
            const inlineType = pc[7].val & 0x7f;
            if (inlineType == 1) {
                try saveInst(vm, .binOpOneCopy, pc);

                const ret = pc[1].val;
                pc[0] = cy.Inst.initOpCode(code);
                if (ret + cy.vm.CallArgStart == (pc-3)[2].val) {
                    pc[1].val = (pc-3)[1].val;
                    pc[2].val = ret + cy.vm.CallArgStart + 1;
                } else {
                    pc[1].val = ret + cy.vm.CallArgStart;
                    pc[2].val = (pc-3)[1].val;
                }
                pc[3].val = ret;

                (pc-3)[0] = cy.Inst.initOpCode(.jump);
                @as(*align(1) i16, @ptrCast(pc-2)).* = 3;
            } else if (inlineType == 2) {
                try saveInst(vm, .binOpBothCopies, pc);

                const ret = pc[1].val;
                pc[0] = cy.Inst.initOpCode(code);
                // Lhs copy is always the last inst.
                pc[1].val = (pc-6)[4].val;
                pc[2].val = (pc-6)[1].val;
                pc[3].val = ret;

                (pc-6)[0] = cy.Inst.initOpCode(.jump);
                @as(*align(1) i16, @ptrCast(pc-5)).* = 6;
            } else {
                try saveInst(vm, .binOp, pc);

                // Inline bin op.
                const ret = pc[1].val;
                pc[0] = cy.Inst.initOpCode(code);
                pc[1].val = ret + cy.vm.CallArgStart;
                pc[2].val = ret + cy.vm.CallArgStart + 1;
                pc[3].val = ret;
            }
            return Value.None;
        }
    };
    return S.method;
}

pub fn nop(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return vm.returnPanic("Unsupported.");
}

pub const ModuleBuilder = struct {
    sym: *cy.Sym,
    chunk: *cy.Chunk,
    compiler: *cy.VMcompiler,
    vm: *cy.VM,

    pub fn init(c: *cy.VMcompiler, symOpt: ?*cy.Sym) ModuleBuilder {
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

    pub fn declareFuncSig(self: *const ModuleBuilder, name: []const u8, params: []const types.TypeId, ret: types.TypeId, ptr: cy.ZHostFuncFn) !void {
        _ = try self.chunk.declareHostFuncSig(self.sym, name, params, ret, cy.NullId, ptr, false);
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
