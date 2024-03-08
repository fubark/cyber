const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const rt = cy.rt;
const log = cy.log.scoped(.bytecode);
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const v = fmt.v;
const vmc = @import("vm_c.zig");

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,
    ops: std.ArrayListUnmanaged(Inst),
    consts: std.ArrayListUnmanaged(cy.Value),

    /// Const map for deduping.
    constMap: std.AutoHashMapUnmanaged(u64, u32),

    /// After compilation, consts is merged into the ops buffer.
    /// This should be used by the interpreter to read const values.
    mconsts: []const cy.Value,

    vm: *cy.VM,

    /// Maps bytecode insts back to source code.
    /// Contains entries ordered by `pc`. 
    debugTable: std.ArrayListUnmanaged(DebugSym),
    debugTempIndexTable: std.ArrayListUnmanaged(u32),

    /// Ordered labels by `pc` for debugging only.
    debugMarkers: std.ArrayListUnmanaged(DebugMarker),

    /// Unwinding temp graph.
    unwindTempRegs: std.ArrayListUnmanaged(u8),
    unwindTempPrevIndexes: std.ArrayListUnmanaged(u32),

    /// Inst descriptions. Each instruction can have an optional description.
    instDescs: if (cy.Trace) std.ArrayListUnmanaged(InstDesc) else void,
    instDescExtras: if (cy.Trace) std.ArrayListUnmanaged(InstDescExtra) else void,

    /// The required stack size for the main frame.
    mainStackSize: u32,

    pub fn init(alloc: std.mem.Allocator, vm: *cy.VM) !ByteCodeBuffer {
        var new = ByteCodeBuffer{
            .alloc = alloc,
            .mainStackSize = 0,
            .ops = .{},
            .vm = vm,
            .consts = .{},
            .constMap = .{},
            .debugTable = .{},
            .debugTempIndexTable = .{},
            .debugMarkers = .{},
            .instDescs = if (cy.Trace) .{} else {},
            .instDescExtras = if (cy.Trace) .{} else {},
            .mconsts = &.{},
            .unwindTempRegs = .{},
            .unwindTempPrevIndexes = .{},
        };
        // Perform big allocation for instruction buffer for more consistent heap allocation.
        try new.ops.ensureTotalCapacityPrecise(alloc, 4096);
        return new;
    }

    pub fn deinit(self: *ByteCodeBuffer) void {
        self.ops.deinit(self.alloc);
        self.consts.deinit(self.alloc);
        self.constMap.deinit(self.alloc);
        self.debugTable.deinit(self.alloc);
        self.debugTempIndexTable.deinit(self.alloc);
        self.debugMarkers.deinit(self.alloc);
        self.unwindTempRegs.deinit(self.alloc);
        self.unwindTempPrevIndexes.deinit(self.alloc);
        if (cy.Trace) {
            self.instDescs.deinit(self.alloc);
            for (self.instDescExtras.items) |extra| {
                self.alloc.free(extra.text);
            }
            self.instDescExtras.deinit(self.alloc);
        }
    }

    pub fn clear(self: *ByteCodeBuffer) void {
        self.ops.clearRetainingCapacity();
        self.consts.clearRetainingCapacity();
        self.debugTable.clearRetainingCapacity();
        self.debugTempIndexTable.clearRetainingCapacity();
        self.debugMarkers.clearRetainingCapacity();
        self.unwindTempRegs.clearRetainingCapacity();
        self.unwindTempPrevIndexes.clearRetainingCapacity();
        if (cy.Trace) {
            self.instDescs.clearRetainingCapacity();
            for (self.instDescExtras.items) |extra| {
                self.alloc.free(extra.text);
            }
            self.instDescExtras.clearRetainingCapacity();
        }
    }

    pub inline fn len(self: *ByteCodeBuffer) usize {
        return self.ops.items.len;
    }

    pub fn getOrPushConst(self: *ByteCodeBuffer, val: cy.Value) !u32 {
        const res = try self.constMap.getOrPut(self.alloc, val.val);
        if (res.found_existing) {
            return res.value_ptr.*;
        } else {
            const idx: u32 = @intCast(self.consts.items.len);
            try self.consts.resize(self.alloc, self.consts.items.len + 1);
            self.consts.items[idx] = val;
            res.key_ptr.* = val.val;
            res.value_ptr.* = idx;
            return idx;
        }
    }

    pub fn pushDebugFuncStart(self: *ByteCodeBuffer, func: *cy.Func, chunkId: u32) !void {
        try self.debugMarkers.append(self.alloc, .{
            .type = @intFromEnum(DebugMarkerType.funcStart),
            .pc = @intCast(self.ops.items.len),
            .data = .{
                .funcStart = .{
                    .func = func,
                    .chunkId = chunkId,
                },
            },
        });
    }

    pub fn pushDebugFuncEnd(self: *ByteCodeBuffer, func: *cy.Func, chunkId: u32) !void {
        try self.debugMarkers.append(self.alloc, .{
            .type = @intFromEnum(DebugMarkerType.funcEnd),
            .pc = @intCast(self.ops.items.len),
            .data = .{
                .funcEnd = .{
                    .func = func,
                    .chunkId = chunkId,
                },
            },
        });
    }

    pub fn pushDebugLabel(self: *ByteCodeBuffer, name: []const u8) !void {
        try self.debugMarkers.append(self.alloc, .{
            .type = @intFromEnum(DebugMarkerType.label),
            .pc = @intCast(self.ops.items.len),
            .data = .{
                .label = .{
                    .namePtr = name.ptr,
                    .nameLen = @intCast(name.len),
                },
            },
        });
    }

    pub fn pushFailableDebugSym(self: *ByteCodeBuffer, pc: usize, file: u32, loc: u32, frameLoc: u32, unwindTempIdx: u32, localStart: u8, localEnd: u8) !void {
        try self.debugTable.append(self.alloc, .{
            .pc = @intCast(pc),
            .loc = loc,
            .file = @intCast(file),
            .frameLoc = frameLoc,
            .localStart = localStart,
            .localEnd = localEnd,
        });
        try self.debugTempIndexTable.append(self.alloc, unwindTempIdx);
    }

    pub fn pushOp(self: *ByteCodeBuffer, code: OpCode) !void {
        try self.pushOpExt(code, .{});
    }

    pub fn pushOpExt(self: *ByteCodeBuffer, code: OpCode, desc: InstDesc) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 1);
        self.ops.items[start] = Inst.initOpCode(code);
        if (cy.Trace) {
            try self.instDescs.append(self.alloc, desc);
        }
    }

    pub fn pushOp1(self: *ByteCodeBuffer, code: OpCode, arg: u8) !void {
        try self.pushOp1Ext(code, arg, .{});
    }

    pub fn pushOp1Ext(self: *ByteCodeBuffer, code: OpCode, arg: u8, desc: InstDesc) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 2);
        self.ops.items[start] = Inst.initOpCode(code);
        self.ops.items[start+1] = .{ .val = arg };
        if (cy.Trace) {
            try self.instDescs.append(self.alloc, desc);
        }
    }

    pub fn pushOp2(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8) !void {
        try self.pushOp2Ext(code, arg, arg2, .{});
    }

    pub fn pushOp2Ext(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8, desc: InstDesc) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 3);
        self.ops.items[start] = Inst.initOpCode(code);
        self.ops.items[start+1] = .{ .val = arg };
        self.ops.items[start+2] = .{ .val = arg2 };
        if (cy.Trace) {
            try self.instDescs.append(self.alloc, desc);
        }
    }

    pub fn pushOp3(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8, arg3: u8) !void {
        try self.pushOp3Ext(code, arg, arg2, arg3, .{});
    }

    pub fn pushOp3Ext(self: *ByteCodeBuffer, code: OpCode, arg: u8, arg2: u8, arg3: u8, desc: InstDesc) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 4);
        self.ops.items[start] = Inst.initOpCode(code);
        self.ops.items[start+1] = .{ .val = arg };
        self.ops.items[start+2] = .{ .val = arg2 };
        self.ops.items[start+3] = .{ .val = arg3 };
        if (cy.Trace) {
            try self.instDescs.append(self.alloc, desc);
        }
    }

    pub fn pushOperand(self: *ByteCodeBuffer, arg: u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + 1);
        self.ops.items[start] = .{ .val = arg };
    }

    pub fn pushOperands(self: *ByteCodeBuffer, operands: []const u8) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + operands.len);
        for (operands, 0..) |operand, i| {
            self.ops.items[start+i] = .{ .val = operand };
        }
    }

    pub fn pushOpSlice(self: *ByteCodeBuffer, code: OpCode, args: []const u8) !void {
        try self.pushOpSliceExt(code, args, .{});
    }
    
    pub fn pushOpSliceExt(self: *ByteCodeBuffer, code: OpCode, args: []const u8, desc: InstDesc) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len + 1);
        self.ops.items[start] = Inst.initOpCode(code);
        for (args, 0..) |arg, i| {
            self.ops.items[start+i+1] = .{ .val = arg };
        }
        if (cy.Trace) {
            try self.instDescs.append(self.alloc, desc);
        }
    }

    pub fn reserveData(self: *ByteCodeBuffer, size: usize) !usize {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + size);
        return start;
    }

    pub fn setOpArgU32(self: *ByteCodeBuffer, idx: usize, arg: u32) void {
        @as(*align(1) u32, @ptrCast(&self.ops.items[idx])).* = arg;
    }

    pub fn setOpArgU16(self: *ByteCodeBuffer, idx: usize, arg: u16) void {
        @as(*align(1) u16, @ptrCast(&self.ops.items[idx])).* = arg;
    }

    pub fn setOpArgs1(self: *ByteCodeBuffer, idx: usize, arg: u8) void {
        self.ops.items[idx].val = arg;
    }

    pub fn getOrPushStaticStringConst(self: *ByteCodeBuffer, str: []const u8) !u32 {
        const val = try self.getOrPushStaticStringValue(str);
        // TODO: Reuse the same const.
        return self.getOrPushConst(val);
    }

    pub fn getOrPushStaticUstring(self: *ByteCodeBuffer, str: []const u8) !cy.Value {
        var allocated: bool = undefined;
        const val = try cy.heap.getOrAllocUstring(self.vm, str, &allocated);
        if (allocated) {
            try self.vm.staticObjects.append(self.alloc, @ptrCast(val.asHeapObject()));
        }
        return val;
    }

    pub fn getOrPushStaticAstring(self: *ByteCodeBuffer, str: []const u8) !cy.Value {
        var allocated: bool = undefined;
        const val = try cy.heap.getOrAllocAstring(self.vm, str, &allocated);
        if (allocated) {
            try self.vm.staticObjects.append(self.alloc, @ptrCast(val.asHeapObject()));
        }
        return val;
    }

    pub fn getOrPushStaticStringValue(self: *ByteCodeBuffer, str: []const u8) !cy.Value {
        if (cy.string.isAstring(str)) {
            return try self.getOrPushStaticAstring(str);
        } else {
            return try self.getOrPushStaticUstring(str);
        }
    }
};

pub const InstDescExtra = struct {
    text: []const u8,
};

pub const InstDesc = struct {
    chunkId: cy.ChunkId = cy.NullId,
    nodeId: cy.NodeId = cy.NullId,
    extraIdx: u32 = cy.NullId,
};

fn printStderr(comptime format: []const u8, args: anytype) void {
    if (!cy.isWasmFreestanding) {
        std.debug.print(format, args);
    }
}

fn printInstArgs(w: anytype, names: []const []const u8, args: []const fmt.FmtValue) !u64 {
    const S = struct {
        var format: [256]u8 = undefined;
    };
    var b = std.io.fixedBufferStream(&S.format);
    if (names.len > 0) {
        _ = try b.write(names[0]);
        _ = try b.write("={}");
        for (names[1..]) |name| {
            _ = try b.write(" ");
            _ = try b.write(name);
            _ = try b.write("={}");
        }
    }
    return try fmt.printCount(w, b.getWritten(), args);
}

const DumpInstOptions = struct {
    prefix: ?[]const u8 = null,
    extra: ?[]const u8 = null,
};

pub fn dumpInst(vm: *cy.VM, pcOffset: u32, code: OpCode, pc: [*]const Inst, opts: DumpInstOptions) !void {
    const w = vm.clearTempString();
    var len: u64 = undefined;
    if (opts.prefix) |prefix| {
        len = try fmt.printCount(w, "{}{} {}: ", &.{v(prefix), v(pcOffset), v(code)});
    } else {
        len = try fmt.printCount(w, "{} {}: ", &.{v(pcOffset), v(code)});
    }
    switch (code) {
        .box => {
            const local = pc[1].val;
            const dst = pc[2].val;
            len += try fmt.printCount(w, "local={}, dst={}", &.{v(local), v(dst)});
        },
        .captured => {
            const closure = pc[1].val;
            const varIdx = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "closure={}, varIdx={}, dst={}", &.{v(closure), v(varIdx), v(dst)});
        },
        .boxValue => {
            const local = pc[1].val;
            const dst = pc[2].val;
            len += try fmt.printCount(w, "local={}, dst={}", &.{v(local), v(dst)});
        },
        .negFloat,
        .negInt => {
            const child = pc[1].val;
            const dst = pc[2].val;
            len += try printInstArgs(w, &.{"child", "dst"},
                &.{v(child), v(dst) });
        },
        .appendList => {
            const lhs = pc[1].val;
            const rhs = pc[2].val;
            const dst = pc[3].val;
            len += try printInstArgs(w, &.{"lhs", "rhs", "dst"},
                &.{v(lhs), v(rhs), v(dst) });
        },
        .lessInt,
        .lessFloat,
        .lessEqualInt,
        .lessEqualFloat,
        .greaterInt,
        .greaterFloat,
        .greaterEqualInt,
        .greaterEqualFloat,
        .divFloat,
        .divInt,
        .mulFloat,
        .mulInt,
        .subFloat,
        .subInt,
        .addFloat,
        .addInt => {
            const lhs = pc[1].val;
            const rhs = pc[2].val;
            const dst = pc[3].val;
            const op: []const u8 = switch (code) {
                .lessInt,
                .lessFloat => "<",
                .lessEqualInt,
                .lessEqualFloat => "<=",
                .greaterInt,
                .greaterFloat => ">",
                .greaterEqualInt,
                .greaterEqualFloat => ">=",
                .divFloat,
                .divInt => "/",
                .mulFloat,
                .mulInt => "*",
                .subFloat,
                .subInt => "-",
                .addFloat,
                .addInt => "+",
                else => return error.TODO,
            };
            len += try fmt.printCount(w, "%{} = %{} {} %{}", &.{v(dst), v(lhs), v(op), v(rhs)});
        },
        .callObjNativeFuncIC => {
            const ret = pc[1].val;
            const numArgs = pc[2].val;
            len += try printInstArgs(w, &.{"ret", "nargs"}, &.{v(ret), v(numArgs)});
        },
        .callObjFuncIC => {
            const ret = pc[1].val;
            const funcPc = @as(*const align(1) u32, @ptrCast(pc + 8)).*;
            const typeId = @as(*const align(1) u16, @ptrCast(pc + 14)).*;
            const numLocals = pc[7].val;
            len += try printInstArgs(w, &.{"ret", "pc", "type", "nlocals"},
                &.{v(ret), v(funcPc), v(typeId), v(numLocals)});
        },
        .callObjSym => {
            const ret = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            const symId = pc[4].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            len += try fmt.printCount(w, "%{} = %{}.(methods[{}])(%{}..%{}) nret={} sig={}", &.{
                v(ret), v(ret+5), v(symId), v(ret + 6), v(ret + 6 + numArgs-1), v(numRet), v(funcSigId),
            });
        },
        .callSym => {
            const ret = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            const symId = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            len += try fmt.printCount(w, "%{} = syms[{}](%{}..%{}) nret={}", &.{
                v(ret), v(symId), v(ret + 5), v(ret + 5 + numArgs), v(numRet)
            });
        },
        .callFuncIC => {
            const ret = pc[1].val;
            const numRet = pc[3].val;
            const pcPtr: [*]cy.Inst = @ptrFromInt(@as(usize, @intCast(@as(*const align(1) u48, @ptrCast(pc + 6)).*)));
            len += try fmt.printCount(w, "ret={}, nret={}, pc={}", &.{v(ret), v(numRet), v(pcPtr)});
        },
        .call => {
            const startLocal = pc[1].val;
            const numArgs = pc[2].val;
            const numRet = pc[3].val;
            len += try fmt.printCount(w, "ret={}, narg={}, nret={}", &.{v(startLocal), v(numArgs), v(numRet)});
        },
        .setBoxValue => {
            const box = pc[1].val;
            const rhs = pc[2].val;
            len += try printInstArgs(w, &.{"box", "rhs"}, &.{v(box), v(rhs)});
        },
        .lambda => {
            const funcOff = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const numParams = pc[3].val;
            const numLocals = pc[4].val;
            const reqCallTypeCheck = pc[5].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 6)).*;
            const dst = pc[8].val;
            len += try fmt.printCount(w, "off={}, nparam={}, nlocal={}, typechk={}, fsigId={}, dst={}", &.{
                v(funcOff), v(numParams), v(numLocals), v(reqCallTypeCheck), v(funcSigId), v(dst)});
        },
        .closure => {
            const funcOff = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const numParams = pc[3].val;
            const numCaptured = pc[4].val;
            const numLocals = pc[5].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 6)).*;
            const local = pc[8].val;
            const reqCallTypeCheck = pc[9].val;
            const dst = pc[10].val;
            len += try fmt.printCount(w, "off={}, nparam={}, ncap={}, nlocal={}, fsigId={}, closure={}, typechk={}, dst={} {}", &.{
                v(funcOff), v(numParams), v(numCaptured), v(numLocals),
                v(funcSigId), v(local), v(reqCallTypeCheck), v(dst), fmt.sliceU8(std.mem.sliceAsBytes(pc[11..11+numCaptured])),
            });
        },
        .true => {
            const dst = pc[1].val;
            len += try fmt.printCount(w, "%{} = true", &.{v(dst)});
        },
        .false => {
            const dst = pc[1].val;
            len += try fmt.printCount(w, "%{} = false", &.{v(dst)});
        },
        .constI8 => {
            const val: i8 = @bitCast(pc[1].val);
            const dst = pc[2].val;
            len += try fmt.printCount(w, "%{} = {}", &.{v(dst), v(val)});
        },
        .constRetain,
        .constOp => {
            const idx = @as(*const align (1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = consts[{}]", &.{v(dst), v(idx)});
        },
        .copy,
        .copyReleaseDst,
        .copyRetainSrc,
        .copyRetainRelease => {
            const src = pc[1].val;
            const dst = pc[2].val;
            len += try fmt.printCount(w, "%{} = %{}", &.{v(dst), v(src)});
        },
        .copyObj => {
            const src = pc[1].val;
            const numFields = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = copy(%{}) nfields={}", &.{v(dst), v(src), v(numFields)});
        },
        .copyObjDyn => {
            const src = pc[1].val;
            const dst = pc[2].val;
            len += try fmt.printCount(w, "%{} = copy(%{})", &.{v(dst), v(src)});
        },
        .end => {
            const endLocal = pc[1].val;
            len += try fmt.printCount(w, "endLocal={}", &.{ v(endLocal) });
        },
        .compare => {
            const left = pc[1].val;
            const right = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = (%{} == %{})", &.{v(dst), v(left), v(right)});
        },
        .field => {
            const recv = pc[1].val;
            const fieldIdx = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = (%{}).{}", &.{v(dst), v(recv), v(fieldIdx)});
        },
        .fieldRef => {
            const recv = pc[1].val;
            const fieldIdx = pc[2].val;
            const numNestedFields = pc[3].val;
            const dst = pc[4].val;
            len += try fmt.printCount(w, "%{} = *(%{}.{}", &.{v(dst), v(recv), v(fieldIdx)});
            if (numNestedFields > 0) {
                for (0..numNestedFields) |i| {
                    len += try fmt.printCount(w, ".{}", &.{v(pc[5+i].val)});
                }
            }
            len += try fmt.printCount(w, ")", &.{});
        },
        .fieldDyn => {
            const recv = pc[1].val;
            const dst = pc[2].val;
            const symId = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            len += try fmt.printCount(w, "%{} = %{}.(fields[{}])", &.{v(dst), v(recv), v(symId)});
        },
        .fieldDynIC => {
            const recv = pc[1].val;
            const dst = pc[2].val;
            const typeId = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            const idx = pc[7].val;
            len += try fmt.printCount(w, "recv={}, dst={}, type={}, idx={}", &.{v(recv), v(dst), v(typeId), v(idx)});
        },
        .forRangeInit => {
            const start = pc[1].val;
            const end = pc[2].val;
            const inc = pc[3].val;
            const cnt = pc[4].val;
            const each = pc[5].val;
            const forRangeInstOffset = @as(*const align(1) u16, @ptrCast(pc + 6)).*;
            len += try printInstArgs(w, &.{"start", "end", "inc", "cnt", "each", "off"}, 
                &.{v(start), v(end), v(inc), v(cnt), v(each), v(forRangeInstOffset)});
        },
        .forRangeReverse,
        .forRange => {
            const counter = pc[1].val;
            const end = pc[2].val;
            const userCounter = pc[3].val;
            const negOffset = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            len += try fmt.printCount(w, "counter={}, end={}, userCounter={}, negOffset={}", &.{v(counter), v(end), v(userCounter), v(negOffset)});
        },
        .indexList => {
            const recv = pc[1].val;
            const index = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "recv={}, index={}, dst={}", &.{v(recv), v(index), v(dst)});
        },
        .jump => {
            const jump = @as(*const align(1) i16, @ptrCast(pc + 1)).*;
            const jumpU32: u32 = @bitCast(@as(i32, jump));
            len += try fmt.printCount(w, "jmp @{}", &.{v(pcOffset +% jumpU32)});
        },
        .jumpCond => {
            const jump = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            len += try fmt.printCount(w, "if %{} jmp @{}", &.{v(pc[1].val), v(pcOffset + jump)});
        },
        .jumpNotCond => {
            const jump = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            len += try fmt.printCount(w, "if !%{} jmp @{}", &.{v(pc[1].val), v(pcOffset + jump)});
        },
        .jumpNone => {
            const jump = @as(*const align(1) i16, @ptrCast(pc + 1)).*;
            len += try fmt.printCount(w, "offset={}, cond={}", &.{v(jump), v(pc[3].val)});
        },
        .list => {
            const startLocal = pc[1].val;
            const numElems = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = [List %{}..%{}]", &.{v(dst), v(startLocal), v(startLocal+numElems)});
        },
        .seqDestructure => {
            const src = pc[1].val;
            const numLocals = pc[2].val;
            const locals = std.mem.sliceAsBytes(pc[3..3+numLocals]);
            len += try fmt.printCount(w, "src={}, nlocals={}, {}", &.{
                v(src), v(numLocals), fmt.sliceU8(locals)});
        },
        .map => {
            const startLocal = pc[1].val;
            const numEntries = pc[2].val;
            const dst = pc[3].val;
            const keyIdxes = std.mem.sliceAsBytes(pc[4..4+numEntries*2]);
            len += try fmt.printCount(w, "startLocal={}, numEntries={}, dst={}, {}", &.{
                v(startLocal), v(numEntries), v(dst), fmt.sliceU8(keyIdxes)});
        },
        .object => {
            const typeId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const argStart = pc[3].val;
            const numFields = pc[4].val;
            const dst = pc[5].val;
            len += try fmt.printCount(w, "%{} = [type={} %{}..%{}]", &.{
                v(dst), v(typeId), v(argStart), v(argStart+numFields),
            });
        },
        .objectSmall => {
            const typeId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const argStart = pc[3].val;
            const numFields = pc[4].val;
            const dst = pc[5].val;
            len += try fmt.printCount(w, "%{} = [type={} %{}..%{}]", &.{
                v(dst), v(typeId), v(argStart), v(argStart+numFields)
            });
        },
        .retain => {
            const local = pc[1].val;
            len += try fmt.printCount(w, "%{}.rc += 1", &.{v(local)});
        },
        .release => {
            const local = pc[1].val;
            len += try fmt.printCount(w, "%{}.rc -= 1", &.{v(local)});
        },
        .releaseN => {
            const numRegs = pc[1].val;
            const regs = std.mem.sliceAsBytes(pc[2..2+numRegs]);
            len += try fmt.printCount(w, "{}", &.{fmt.sliceU8(regs)});
        },
        .setFieldCheck => {
            const recv = pc[1].val;
            const fieldT = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            const val = pc[4].val;
            const idx = pc[5].val;
            len += try fmt.printCount(w, "(%{}).{} = %{} ftype={}", &.{v(recv), v(idx), v(val), v(fieldT)});
        },
        .setField => {
            const recv = pc[1].val;
            const idx = pc[2].val;
            const val = pc[3].val;
            len += try fmt.printCount(w, "(%{}).{} = %{}", &.{v(recv), v(idx), v(val)});
        },
        .ref => {
            const ref = pc[1].val;
            const dst = pc[2].val;
            len += try fmt.printCount(w, "%{} = %{}.*", &.{v(dst), v(ref)});
        },
        .setRef => {
            const ref = pc[1].val;
            const val = pc[2].val;
            len += try fmt.printCount(w, "%{}.* = %{}", &.{v(ref), v(val)});
        },
        .setFieldDyn,
        .setFieldDynIC => {
            const recv = pc[1].val;
            const fieldId = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            const val = pc[4].val;
            len += try fmt.printCount(w, "recv={}, fid={}, rhs={}", &.{v(recv), v(fieldId), v(val)});
        },
        .setIndexList => {
            const list = pc[1].val;
            const index = pc[2].val;
            const right = pc[3].val;
            len += try fmt.printCount(w, "list={}, index={}, right={}", &.{v(list), v(index), v(right)});
        },
        .coinit => {
            const startArgs = pc[1].val;
            const numArgs = pc[2].val;
            const argDst = pc[3].val;
            const jump = pc[4].val;
            const initialStackSize = pc[5].val;
            const dst = pc[6].val;
            len += try fmt.printCount(w, "startArgs={}, numArgs={}, argDst={}, jump={}, initStack={}, dst={}",
                &.{v(startArgs), v(numArgs), v(argDst), v(jump), v(initialStackSize), v(dst)});
        },
        .coresume => {
            const fiberLocal = pc[1].val;
            const retLocal = pc[2].val;
            len += try fmt.printCount(w, "fiberLocal={}, retLocal={}", &.{v(fiberLocal), v(retLocal) });
        },
        .sliceList => {
            const recv = pc[1].val;
            const range = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = %{}[Range(%{})]", &.{v(dst), v(recv), v(range)});
        },
        .castAbstract,
        .cast => {
            const child = pc[1].val;
            const expTypeId = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "%{} = cast(type={}, %{})", &.{v(dst), v(expTypeId), v(child)});
        },
        .staticFunc => {
            const id = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "id={} dst={}", &.{v(id), v(dst)});
        },
        .staticVar => {
            const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "sym={} dst={}", &.{v(symId), v(dst)});
        },
        .setStaticVar => {
            const symId = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const src = pc[3].val;
            len += try fmt.printCount(w, "sym={} src={}", &.{v(symId), v(src)});
        },
        .pushTry => {
            const errDst = pc[1].val;
            const dstIsRetained = pc[2].val;
            const catchPcOffset = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            len += try fmt.printCount(w, "errDst={} dstIsRetained={} catchOff={}", &.{v(errDst), v(dstIsRetained), v(catchPcOffset)});
        },
        .popTry => {
            const endOffset = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            len += try fmt.printCount(w, "endOff={}", &.{v(endOffset)});
        },
        .stringTemplate => {
            const startLocal = pc[1].val;
            const exprCount = pc[2].val;
            const dst = pc[3].val;
            len += try fmt.printCount(w, "startLocal={}, exprCount={}, dst={}", &.{v(startLocal), v(exprCount), v(dst)});
        },
        .callTypeCheck => {
            const arg = pc[1].val;
            const numArgs = pc[2].val;
            const funcSigId = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            len += try fmt.printCount(w, "arg={}, nargs={}, sigId={}", &.{v(arg), v(numArgs), v(funcSigId)});
        },
        else => {},
    }

    if (opts.extra) |extra| {
        const ExtraStartCol = 60;
        if (len > ExtraStartCol) {
            fmt.print(w, "\n{}| {}", &.{fmt.repeat(' ', ExtraStartCol), v(extra)});
        } else {
            fmt.print(w, "{}| {}", &.{fmt.repeat(' ', @intCast(ExtraStartCol-len)), v(extra)});
        }
    }
    rt.print(vm, vm.getTempString());
}

pub const StringIndexContext = struct {
    buf: *std.ArrayListUnmanaged(u8),

    pub fn hash(self: StringIndexContext, s: cy.IndexSlice(u32)) u64 {
        return std.hash.Wyhash.hash(0, self.buf.items[s.start..s.end]);
    }

    pub fn eql(self: StringIndexContext, a: cy.IndexSlice(u32), b: cy.IndexSlice(u32)) bool {
        return std.mem.eql(u8, self.buf.items[a.start..a.end], self.buf.items[b.start..b.end]);
    }
};

pub const StringIndexInsertContext = struct {
    buf: *std.ArrayListUnmanaged(u8),

    pub fn hash(self: StringIndexInsertContext, s: []const u8) u64 {
        _ = self;
        return std.hash.Wyhash.hash(0, s);
    }

    pub fn eql(self: StringIndexInsertContext, a: []const u8, b: cy.IndexSlice(u32)) bool {
        return std.mem.eql(u8, a, self.buf.items[b.start..b.end]);
    }
};

pub const Inst = packed struct {
    val: u8,

    pub inline fn initOpCode(code_: OpCode) Inst {
        return .{
            .val = @intFromEnum(code_),
        };
    }

    pub inline fn opcode(self: *const Inst) OpCode {
        return @enumFromInt(self.val);
    }

    pub fn initArg(arg: u8) Inst {
        return .{
            .val = arg,
        };
    }
};

pub const DebugSym = extern struct {
    /// Start position of an inst.
    pc: u32,

    /// Points to a cy.NodeId.
    loc: u32,

    /// Points to the parent function or coinit declaration node. 0 if it's in the main block.
    frameLoc: u32,

    /// CompileChunkId.
    file: u16,

    /// Which locals are alive before this instruction.
    /// Not all locals in this range have a RC but they are guaranteed to have a defined value.
    localStart: u8,
    localEnd: u8,

    pub fn getLocals(sym: cy.DebugSym) cy.IndexSlice(u8) {
        return cy.IndexSlice(u8).init(sym.localStart, @intCast(sym.localEnd));
    }
};

const DebugMarkerType = enum(u8) {
    label,
    funcStart,

    /// `pc` is exclusive.
    funcEnd,
};

const DebugMarker = extern struct {
    data: extern union {
        label: extern struct {
            /// Unowned.
            namePtr: [*]const u8,
            nameLen: u16,
        },
        funcStart: extern struct {
            chunkId: u32,
            func: *cy.Func,
        },
        funcEnd: extern struct {
            chunkId: u32,
            func: *cy.Func,
        },
    },

    /// Inst position of marker.
    pc: u32,
    type: u8,

    pub fn getLabelName(self: DebugMarker) []const u8 {
        return self.data.label.namePtr[0..self.data.label.nameLen];
    }

    pub fn etype(self: *const DebugMarker) DebugMarkerType {
        return @enumFromInt(self.type);
    }
};

pub const CallObjSymInstLen = vmc.CALL_OBJ_SYM_INST_LEN;
pub const CallSymInstLen = vmc.CALL_SYM_INST_LEN;
pub const CallInstLen = vmc.CALL_INST_LEN;

test "getInstLenAt" {
    var code = Inst.initOpCode(.call);
    try t.eq(getInstLenAt(@ptrCast(&code)), CallInstLen);
}

pub fn getInstLenAt(pc: [*]const Inst) u8 {
    switch (pc[0].opcode()) {
        .ret0,
        .ret1,
        .coreturn => {
            return 1;
        },
        .typeCheckOption,
        .throw,
        .retain,
        .end,
        .release,
        .true,
        .false,
        .mapEmpty => {
            return 2;
        },
        .releaseN => {
            const numVars = pc[1].val;
            return 2 + numVars;
        },
        .not,
        .popTry,
        .copy,
        .copyRetainSrc,
        .copyReleaseDst,
        .copyRetainRelease,
        .copyObjDyn,
        .constI8,
        .jump,
        .coyield,
        .coresume,
        .box,
        .setBoxValue,
        .setBoxValueRelease,
        .boxValue,
        .boxValueRetain,
        .ref,
        .setRef,
        .tagLiteral => {
            return 3;
        },
        .seqDestructure => {
            return 3 + pc[2].val;
        },
        .objectTypeCheck => {
            return 3 + pc[2].val * 5;
        },
        .copyObj,
        .typeCheck,
        .call,
        .captured,
        .constOp,
        .constRetain,
        .staticVar,
        .setStaticVar,
        .staticFunc,
        .field,
        .setStaticFunc,
        .setField,
        .jumpNone,
        .jumpCond,
        .compare,
        .compareNot,
        .list,
        .tag,
        .setCaptured,
        .refCopyObj,
        .jumpNotCond => {
            return 4;
        },
        .stringTemplate => {
            const numExprs = pc[2].val;
            return 4 + numExprs + 1;
        },
        .map => {
            const numEntries = pc[2].val;
            return 4 + numEntries * 2;
        },
        .range,
        .unwrapChoice,
        .cast,
        .castAbstract,
        .pushTry,
        .callTypeCheck => {
            return 5;
        },
        .match => {
            const numConds = pc[2].val;
            return 5 + numConds * 3;
        },
        .fieldRef => {
            const numNestedFields = pc[3].val;
            return 5 + numNestedFields;
        },
        .setFieldCheck,
        .object,
        .objectSmall,
        .forRange,
        .forRangeReverse => {
            return 6;
        },
        .coinit,
        .sym => {
            return 7;
        },
        .fieldDyn,
        .fieldDynIC,
        .forRangeInit => {
            return 8;
        },
        .lambda => {
            return 9;
        },
        .setFieldDyn,
        .setFieldDynIC => {
            return 10;
        },
        .closure => {
            const numCaptured = pc[4].val;
            return 11 + numCaptured;
        },
        .callSym,
        .callNativeFuncIC,
        .callFuncIC => {
            return CallSymInstLen;
        },
        .appendList,
        .lessFloat,
        .greaterFloat,
        .lessEqualFloat,
        .greaterEqualFloat,
        .lessInt,
        .greaterInt,
        .lessEqualInt,
        .greaterEqualInt,
        .sliceList,
        .indexList,
        .indexTuple,
        .indexMap,
        .addFloat,
        .subFloat,
        .mulFloat,
        .divFloat,
        .powFloat,
        .modFloat,
        .addInt,
        .subInt,
        .mulInt,
        .divInt,
        .powInt,
        .modInt,
        .negFloat,
        .negInt,
        .bitwiseNot,
        .bitwiseAnd,
        .bitwiseOr,
        .bitwiseXor,
        .bitwiseLeftShift,
        .bitwiseRightShift,
        .callObjSym,
        .callObjNativeFuncIC,
        .setIndexList,
        .setIndexMap,
        .callObjFuncIC => {
            return CallObjSymInstLen;
        },
    }
}

pub const OpCode = enum(u8) {
    /// Copies a constant value from `consts` to a dst local.
    /// [constIdx u16] [dst]
    constOp = vmc.CodeConstOp,
    constRetain = vmc.CodeConstRetain,
    constI8 = vmc.CodeConstI8,
    addFloat = vmc.CodeAddFloat,
    subFloat = vmc.CodeSubFloat,
    /// Push boolean onto register stack.
    true = vmc.CodeTrue,
    false = vmc.CodeFalse,
    /// Pops top register, performs not, and pushes result onto stack.
    not = vmc.CodeNot,
    /// Copies a local from src to dst.
    copy = vmc.CodeCopy,
    copyReleaseDst = vmc.CodeCopyReleaseDst,
    copyRetainSrc = vmc.CodeCopyRetainSrc,
    copyRetainRelease = vmc.CodeCopyRetainRelease,
    copyObj = vmc.CodeCopyObj,
    copyObjDyn = vmc.CodeCopyObjDyn,

    setIndexList = vmc.CodeSetIndexList,
    setIndexMap = vmc.CodeSetIndexMap,

    indexList = vmc.CodeIndexList,
    indexTuple = vmc.CodeIndexTuple,
    indexMap = vmc.CodeIndexMap,

    appendList = vmc.CodeAppendList,

    /// First operand points the first elem and also the dst local. Second operand contains the number of elements.
    list = vmc.CodeList,
    /// First operand points the first entry value and also the dst local. Second operand contains the number of elements.
    /// Const key indexes follow the size operand.
    map = vmc.CodeMap,
    mapEmpty = vmc.CodeMapEmpty,
    sliceList = vmc.CodeSliceList,
    /// Pops top register, if value evals to false, jumps the pc forward by an offset.
    jumpNotCond = vmc.CodeJumpNotCond,
    jumpCond = vmc.CodeJumpCond,
    /// Jumps the pc by an 16-bit integer offset.
    jump = vmc.CodeJump,

    release = vmc.CodeRelease,

    /// Exclusively used for block end to distinguish from temp releases.
    releaseN = vmc.CodeReleaseN,

    callObjSym = vmc.CodeCallObjSym,
    callObjNativeFuncIC = vmc.CodeCallObjNativeFuncIC,
    callObjFuncIC = vmc.CodeCallObjFuncIC,

    /// Runtime type check.
    callTypeCheck = vmc.CodeCallTypeCheck,

    callSym = vmc.CodeCallSym,
    callFuncIC = vmc.CodeCallFuncIC,
    callNativeFuncIC = vmc.CodeCallNativeFuncIC,
    ret1 = vmc.CodeRet1,
    ret0 = vmc.CodeRet0,

    /// Calls a lambda.
    /// [calleeLocal] [numArgs] [numRet=0/1]
    call = vmc.CodeCall,

    typeCheck = vmc.CodeTypeCheck,
    typeCheckOption = vmc.CodeTypeCheckOption,

    field = vmc.CodeField,
    fieldRef = vmc.CodeFieldRef,
    fieldDyn = vmc.CodeFieldDyn,
    fieldDynIC = vmc.CodeFieldDynIC,
    lambda = vmc.CodeLambda,
    closure = vmc.CodeClosure,
    compare = vmc.CodeCompare,
    lessFloat = vmc.CodeLessFloat,
    greaterFloat = vmc.CodeGreaterFloat,
    lessEqualFloat = vmc.CodeLessEqualFloat,
    greaterEqualFloat = vmc.CodeGreaterEqualFloat,
    lessInt = vmc.CodeLessInt,
    greaterInt = vmc.CodeGreaterInt,
    lessEqualInt = vmc.CodeLessEqualInt,
    greaterEqualInt = vmc.CodeGreaterEqualInt,

    mulFloat = vmc.CodeMulFloat,
    divFloat = vmc.CodeDivFloat,
    powFloat = vmc.CodePowFloat,
    modFloat = vmc.CodeModFloat,

    compareNot = vmc.CodeCompareNot,

    /// [startLocal] [exprCount] [dst] [..string consts]
    stringTemplate = vmc.CodeStringTemplate,
    negFloat = vmc.CodeNegFloat,

    objectTypeCheck = vmc.CodeObjectTypeCheck,
    objectSmall = vmc.CodeObjectSmall,
    object = vmc.CodeObject,

    ref = vmc.CodeRef,
    refCopyObj = vmc.CodeRefCopyObj,
    setRef = vmc.CodeSetRef,
    unwrapChoice = vmc.CodeUnwrapChoice,

    setFieldDyn = vmc.CodeSetFieldDyn,
    setFieldDynIC = vmc.CodeSetFieldDynIC,
    setField = vmc.CodeSetField,
    setFieldCheck = vmc.CodeSetFieldCheck,

    coinit = vmc.CodeCoinit,
    coyield = vmc.CodeCoyield,
    coresume = vmc.CodeCoresume,
    coreturn = vmc.CodeCoreturn,
    retain = vmc.CodeRetain,

    /// Lifts a source local to a box object and stores the result in `dstLocal`.
    /// The source local is also retained.
    /// [srcLocal] [dstLocal]
    box = vmc.CodeBox,

    setBoxValue = vmc.CodeSetBoxValue,
    setBoxValueRelease = vmc.CodeSetBoxValueRelease,
    boxValue = vmc.CodeBoxValue,
    boxValueRetain = vmc.CodeBoxValueRetain,
    captured = vmc.CodeCaptured,
    setCaptured = vmc.CodeSetCaptured,
    /// TODO: Rename to enumOp.
    tag = vmc.CodeTag,
    /// TODO: Rename to symbol.
    tagLiteral = vmc.CodeTagLiteral,
    range = vmc.CodeRange,

    seqDestructure = vmc.CodeSeqDestructure,
    bitwiseAnd = vmc.CodeBitwiseAnd,
    bitwiseOr = vmc.CodeBitwiseOr,
    bitwiseXor = vmc.CodeBitwiseXor,
    bitwiseNot = vmc.CodeBitwiseNot,
    bitwiseLeftShift = vmc.CodeBitwiseLeftShift,
    bitwiseRightShift = vmc.CodeBitwiseRightShift,
    jumpNone = vmc.CodeJumpNone,
    addInt = vmc.CodeAddInt,
    subInt = vmc.CodeSubInt,
    mulInt = vmc.CodeMulInt,
    divInt = vmc.CodeDivInt,
    modInt = vmc.CodeModInt,
    powInt = vmc.CodePowInt,
    negInt = vmc.CodeNegInt,
    forRangeInit = vmc.CodeForRangeInit,
    forRange = vmc.CodeForRange,
    forRangeReverse = vmc.CodeForRangeReverse,

    /// Performs an eq comparison with a sequence of locals.
    /// The pc then jumps with the offset of the matching local, otherwise the offset from the end is used.
    /// [exprLocal] [numCases] [case1Local] [case1Jump] ... [elseJump]
    match = vmc.CodeMatch,

    /// Copies and retains a static variable to a destination local.
    /// [symId u16] [dstLocal]
    staticVar = vmc.CodeStaticVar,

    /// Copies a local register to a static variable.
    /// [symId u16] [local]
    setStaticVar = vmc.CodeSetStaticVar,

    /// Wraps a static function in a function value.
    /// [symId u16] [dstLocal]
    staticFunc = vmc.CodeStaticFunc,

    /// Copies a local register to a static function.
    /// [symId u16] [local] 
    setStaticFunc = vmc.CodeSetStaticFunc,

    /// Allocates a symbol object to a destination local.
    /// [symType] [symId] [dst]
    /// TODO: Rename to typeOp.
    sym = vmc.CodeSym,

    cast = vmc.CodeCast,
    castAbstract = vmc.CodeCastAbstract,

    pushTry = vmc.CodePushTry,
    popTry = vmc.CodePopTry,
    throw = vmc.CodeThrow,

    /// Indicates the end of the main script.
    end = vmc.CodeEnd,
};

test "bytecode internals." {
    try t.eq(std.enums.values(OpCode).len, 119);
    try t.eq(@sizeOf(Inst), 1);
    if (cy.is32Bit) {
        try t.eq(@sizeOf(DebugMarker), 16);
    } else {
        try t.eq(@sizeOf(DebugMarker), 24);
    }
    try t.eq(@sizeOf(DebugSym), 16);

    try t.eq(@offsetOf(ByteCodeBuffer, "alloc"), @offsetOf(vmc.ByteCodeBuffer, "alloc"));
    try t.eq(@offsetOf(ByteCodeBuffer, "ops"), @offsetOf(vmc.ByteCodeBuffer, "ops"));
    try t.eq(@offsetOf(ByteCodeBuffer, "consts"), @offsetOf(vmc.ByteCodeBuffer, "consts"));
    try t.eq(@offsetOf(ByteCodeBuffer, "constMap"), @offsetOf(vmc.ByteCodeBuffer, "constMap"));
    try t.eq(@offsetOf(ByteCodeBuffer, "mconsts"), @offsetOf(vmc.ByteCodeBuffer, "mconsts_buf"));
    try t.eq(@offsetOf(ByteCodeBuffer, "vm"), @offsetOf(vmc.ByteCodeBuffer, "vm"));
    try t.eq(@offsetOf(ByteCodeBuffer, "debugTable"), @offsetOf(vmc.ByteCodeBuffer, "debugTable"));
    try t.eq(@offsetOf(ByteCodeBuffer, "debugMarkers"), @offsetOf(vmc.ByteCodeBuffer, "debugMarkers"));
    try t.eq(@offsetOf(ByteCodeBuffer, "mainStackSize"), @offsetOf(vmc.ByteCodeBuffer, "mainStackSize"));
}