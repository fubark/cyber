const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const log = cy.log.scoped(.bytecode);
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const v = fmt.v;
const ast = cy.ast;
const vmc = @import("vmc");
const bcgen = @import("bc_gen.zig");

/// Surface bad inst generation by recording just the opcodes and comparing that by walking the bytecode.
/// Used with `checkBytecode`.
const RecordOpcodes = builtin.mode == .Debug and false;

pub const StringSlice = struct {
    idx: u64,
    len_: u64, // sign bit indicates ascii

    pub fn len(self: *const StringSlice) usize {
        return @intCast(self.len_ & ~(@as(u64, 1) << 63));
    }

    pub fn ascii(self: *const StringSlice) bool {
        return self.len_ & (1 << 63) != 0;
    }
};

/// Holds vm instructions.
pub const ByteCodeBuffer = struct {
    alloc: std.mem.Allocator,

    /// TODO: Rename to `insts`.
    ops: std.ArrayListUnmanaged(Inst),
    consts: std.ArrayListUnmanaged(cy.Value),

    /// TODO: Rename to `ops`.
    opcodes: std.ArrayListUnmanaged(OpCode),

    /// Whether a const is boxed.
    consts_boxed: std.ArrayListUnmanaged(bool),

    /// Const map for deduping.
    constMap: std.AutoHashMapUnmanaged(u64, u32),

    /// Zero delimited. slice -> isAscii.
    const_string_map: std.StringHashMapUnmanaged(bool),

    vm: *cy.VM,

    /// Maps bytecode insts back to source code.
    /// Contains entries ordered by `pc`. 
    debugTable: std.ArrayListUnmanaged(DebugSym),

    /// Ordered pc to each func's `CHK_STK` inst. Used to recover return reg size during unwind.
    func_table: std.ArrayListUnmanaged(usize),

    /// Maps pc to a bitset with active pointer locals.
    ptr_table: std.ArrayListUnmanaged(PcPtrLayout),

    ptr_layouts: std.ArrayListUnmanaged([]bool),
    ptr_layout_map: std.HashMapUnmanaged([]const bool, usize, PtrLayoutContext, 80),

    ptr_layout_builder: std.ArrayListUnmanaged(bool),

    markers: std.AutoHashMapUnmanaged(u32, *cy.Func),

    /// JIT requires BC label locations to know how to map jumps.
    /// Only labels for backward jumps. (loops).
    labels: std.ArrayList(u32),

    /// The required stack size for the main frame.
    mainStackSize: u32,

    main_pc: u32,

    pub fn init(alloc: std.mem.Allocator, vm: *cy.VM) ByteCodeBuffer {
        const new = ByteCodeBuffer{
            .alloc = alloc,
            .mainStackSize = 0,
            .main_pc = 0,
            .ops = .{},
            .opcodes = .{},
            .vm = vm,
            .consts = .{},
            .consts_boxed = .{},
            .constMap = .{},
            .debugTable = .{},
            .func_table = .{},
            .const_string_map = .{},
            .ptr_table = .{},
            .ptr_layouts = .{},
            .ptr_layout_map = .{},
            .ptr_layout_builder = .{},
            .markers = .{},
            .labels = .{},
        };
        return new;
    }

    pub fn deinit(self: *ByteCodeBuffer) void {
        self.ops.deinit(self.alloc);
        self.opcodes.deinit(self.alloc);
        self.consts.deinit(self.alloc);
        self.consts_boxed.deinit(self.alloc);
        self.constMap.deinit(self.alloc);
        {
            var iter = self.const_string_map.iterator();
            while (iter.next()) |e| {
                const str: [:0]const u8 = @ptrCast(e.key_ptr.*);
                self.alloc.free(str);
            }
        }
        self.const_string_map.deinit(self.alloc);
        self.debugTable.deinit(self.alloc);
        self.func_table.deinit(self.alloc);
        self.ptr_table.deinit(self.alloc);
        for (self.ptr_layouts.items) |layout| {
            self.alloc.free(layout);
        }
        self.ptr_layouts.deinit(self.alloc);
        self.ptr_layout_map.deinit(self.alloc);
        self.ptr_layout_builder.deinit(self.alloc);
        self.markers.deinit(self.alloc);
        self.labels.deinit(self.alloc);
    }

    pub fn clear(self: *ByteCodeBuffer) void {
        self.main_pc = 0;
        self.ops.clearRetainingCapacity();
        self.opcodes.clearRetainingCapacity();
        self.consts.clearRetainingCapacity();
        self.consts_boxed.clearRetainingCapacity();
        self.debugTable.clearRetainingCapacity();
        self.func_table.clearRetainingCapacity();
        self.unwind_table.clearRetainingCapacity();
        self.unwind_slot_obj.clearRetainingCapacity();
        self.unwind_slot_obj_prev.clearRetainingCapacity();
        self.unwind_slot_typed.clearRetainingCapacity();
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

    pub fn push_label(self: *ByteCodeBuffer) !void {
        try self.labels.append(self.alloc, @intCast(self.len()));
    }

    const ConstString = struct {
        slice: [:0]const u8,
        ascii: bool,
    };

    pub fn getOrPushConstString(self: *ByteCodeBuffer, str: []const u8) !ConstString {
        const res = try self.const_string_map.getOrPut(self.alloc, str);
        if (!res.found_existing) {
            res.key_ptr.* = try self.alloc.dupeZ(u8, str);
            res.value_ptr.* = cy.string.isAstring(str);
        }
        return .{
            .slice = @ptrCast(res.key_ptr.*),
            .ascii = res.value_ptr.*,
        };
    }

    pub fn getOrPushConst(self: *ByteCodeBuffer, boxed: bool, val: cy.Value) !u32 {
        const res = try self.constMap.getOrPut(self.alloc, val.val);
        if (res.found_existing) {
            return res.value_ptr.*;
        } else {
            const idx: u32 = @intCast(self.consts.items.len);
            try self.consts_boxed.resize(self.alloc, self.consts.items.len + 1);
            try self.consts.resize(self.alloc, self.consts.items.len + 1);
            self.consts.items[idx] = val;
            self.consts_boxed.items[idx] = boxed;
            res.key_ptr.* = val.val;
            res.value_ptr.* = idx;
            return idx;
        }
    }

    pub fn pushFailableDebugSym(self: *ByteCodeBuffer, pc: usize, file: u32, loc: u32, frameLoc: u32) !void {
        try self.debugTable.append(self.alloc, .{
            .pc = @intCast(pc),
            .loc = loc,
            .file = @intCast(file),
            .frameLoc = frameLoc,
        });
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

    pub fn pushOpSlice2(self: *ByteCodeBuffer, code: OpCode, tmp_dst: bool, args: []const u16) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len + 1);
        self.ops.items[start] = Inst.initOpCode2(code, tmp_dst);
        if (RecordOpcodes) {
            try self.opcodes.append(self.alloc, code);
        }
        for (args, 0..) |arg, i| {
            self.ops.items[start+i+1] = .{ .val = arg };
        }
    }

    pub fn pushOpSlice(self: *ByteCodeBuffer, code: OpCode, args: []const u16) !void {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + args.len + 1);
        self.ops.items[start] = Inst.initOpCode(code);
        if (RecordOpcodes) {
            try self.opcodes.append(self.alloc, code);
        }
        for (args, 0..) |arg, i| {
            self.ops.items[start+i+1] = .{ .val = arg };
        }
    }
    
    pub fn reserveData(self: *ByteCodeBuffer, size: usize) !usize {
        const start = self.ops.items.len;
        try self.ops.resize(self.alloc, self.ops.items.len + size);
        return start;
    }

    pub fn setOpArgU64(self: *ByteCodeBuffer, idx: usize, arg: u64) void {
        if (cy.Trace) {
            if (idx + 4 > self.ops.items.len) @panic("OutOfBounds");
        }
        @as(*align(2) u64, @ptrCast(&self.ops.items[idx])).* = arg;
    }

    pub fn setOpArgU48(self: *ByteCodeBuffer, idx: usize, arg: u48) void {
        if (cy.Trace) {
            if (idx + 3 > self.ops.items.len) @panic("OutOfBounds");
        }
        @as(*align(2) u48, @ptrCast(&self.ops.items[idx])).* = arg;
    }

    pub fn setOpArgU32(self: *ByteCodeBuffer, idx: usize, arg: u32) void {
        if (cy.Trace) {
            if (idx + 2 > self.ops.items.len) @panic("OutOfBounds");
        }
        @as(*align(2) u32, @ptrCast(&self.ops.items[idx])).* = arg;
    }

    pub fn setOpArgs1(self: *ByteCodeBuffer, idx: usize, arg: u8) void {
        self.ops.items[idx].val = arg;
    }
};

fn printStderr(comptime format: []const u8, args: anytype) void {
    if (!cy.isWasmFreestanding) {
        std.debug.print(format, args);
    }
}

const DumpInstOptions = struct {
    prefix: ?[]const u8 = null,
    extra: ?[]const u8 = null,
};

pub fn write_inst(vm: *cy.VM, w: *std.Io.Writer, code: OpCode, chunk_id: ?usize, pc_off: ?usize, pc: [*]const Inst, opts: DumpInstOptions) !void {
    if (opts.prefix) |prefix| {
        if (pc_off) |off| {
            try w.print("{s}{}:{} {}: ", .{prefix, chunk_id.?, off, code});
        } else {
            try w.print("{s}@{x} {}: ", .{prefix, @intFromPtr(pc), code});
        }
    } else {
        if (pc_off) |off| {
            try w.print("{}:{} {}: ", .{chunk_id.?, off, code});
        } else {
            try w.print("@{x} {}: ", .{@intFromPtr(pc), code});
        }
    }
    switch (code) {
        .captured => {
            const dst = pc[1].val;
            const closure = pc[2].val;
            const member_idx = pc[3].val;
            try w.print("%{} = [%{} + {}]", .{dst, closure, member_idx});
        },
        .fneg32,
        .fneg,
        .neg => {
            const dst = pc[1].val;
            const child = pc[2].val;
            try w.print("%{} = -%{}", .{dst, child});
        },
        .ret_n => {
            const ret_size = pc[1].val;
            try w.print("ret %0..%{}", .{ret_size});
        },
        .or_,
        .lsl,
        .feq,
        .feq32,
        .lt,
        .flt,
        .flt32,
        .le,
        .fle,
        .fle32,
        .gt,
        .fgt,
        .fgt32,
        .ge,
        .fge,
        .fge32,
        .fdiv,
        .fdiv32,
        .idiv,
        .div,
        .fmul,
        .fmul32,
        .imul,
        .mul,
        .fsub,
        .fsub32,
        .sub,
        .fadd,
        .fadd32,
        .add => {
            const dst = pc[1].val;
            const lhs = pc[2].val;
            const rhs = pc[3].val;
            const op: []const u8 = switch (code) {
                .or_ => "||",
                .lsl => "<<",
                .feq32,
                .feq => "=",
                .lt,
                .flt32,
                .flt => "<",
                .le,
                .fle32,
                .fle => "<=",
                .gt,
                .fgt32,
                .fgt => ">",
                .ge,
                .fge32,
                .fge => ">=",
                .fdiv,
                .fdiv32,
                .idiv,
                .div => "/",
                .fmul,
                .fmul32,
                .imul,
                .mul => "*",
                .fsub,
                .fsub32,
                .sub => "-",
                .fadd,
                .fadd32,
                .add => "+",
                else => return error.TODO,
            };
            try w.print("%{} = %{} {s} %{}", .{dst, lhs, op, rhs});
        },
        .call_union => {
            const base = pc[1].val;
            try w.print("?..%{} = call_union(%{}..)", .{
                base, base + 4,
            });
        },
        .chk_stk => {
            const ret_size = pc[1].val;
            const frame_size = pc[2].val;
            try w.print("ret={}, frame={}", .{
                ret_size, frame_size,
            });
        },
        .call => {
            const base = pc[1].val;
            const func_pc: usize = @intCast(std.mem.readInt(u48, @ptrCast(pc + 2), .little));
            const func_id: u32 = @as(*const align(2) u32, @ptrFromInt(func_pc - 4)).*;

            const name = vm.funcSymDetails.items[func_id].name();
            // if (vm.funcSymDetails.items[func_id].func) |func| {
            //     if (func.parent.parent) |parent| {
            //         if (parent.type != .chunk) {
            //             _ = try fmt.printCount(w, "%{} = {}.{}(%{}..%{})", &.{
            //                 v(ret), v(parent.name()), v(name), v(ret + 5), v(ret + 5 + vm.funcSyms.items[func_id].nparams),
            //             });
            //             break :b;
            //         }
            //     }
            // }

            const sig = vm.sema.getFuncSig(vm.funcSymDetails.items[func_id].sig);
            var reg_size: usize = 0;
            for (sig.params()) |param| {
                reg_size += param.get_type().reg_size();
            }
            if (sig.ret.reg_size() > 0) {
                if (sig.ret.reg_size() == 1) {
                    try w.print("%{} = ", .{base - sig.ret.reg_size()});
                } else {
                    try w.print("%{}..%{} = ", .{base - sig.ret.reg_size(), base});
                }
            }
            if (vm.funcSymDetails.items[func_id].func) |func| {
                const parent = func.parent_mod_sym().name();
                try w.print("{s}.{s}(%{}..%{}) {}", .{
                    parent, name, base + 4, base + 4 + reg_size, func_id,
                });
            } else {
                try w.print("{s}(%{}..%{}) {}", .{
                    name, base + 4, base + 4 + reg_size, func_id,
                });
            }
        },
        .call_host => {
            const base = pc[1].val;
            const ptr_int = std.mem.readInt(u48, @ptrCast(pc + 2), .little);
            if (ptr_int == 0) {
                try w.print("?..%{} = <placeholder>(%{}..?)", .{
                    base, base + 4,
                });
            } else {
                const ptr: *anyopaque = @ptrFromInt(@as(usize, @intCast(ptr_int)));
                const func_id = vm.host_funcs.get(ptr).?;
                const name = vm.funcSymDetails.items[func_id].name();
                const sig = vm.sema.getFuncSig(vm.funcSymDetails.items[func_id].sig);
                try w.print("%{} = {s}(%{}..%{})", .{
                    base - bcgen.type_reg_size(sig.ret), name, base + 4, base + 4 + sig.numParams(),
                });
            }
        },
        .call_ptr => {
            const base = pc[1].val;
            try w.print("?..%{} = %{}(%{}..)", .{
                base, base + 3, base + 4,
            });
        },
        .call_trait => {
            const base = pc[1].val;
            const vtable_idx = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            try w.print("?..%{} = %{}.vtable[{}](%{}..)", .{
                base, base + 3, vtable_idx, base + 4,
            });
        },
        .closure => {
            const dst = pc[1].val;
            // const func_id = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const func_id: usize = 0;
            const union_t = pc[2].val;
            const captured_info = pc[6].val;
            const num_captured = captured_info & 0x7fff;
            const is_stack_closure = (captured_info & 0x8000) != 0;
            try w.print("%{} = closure(id={}, union_t={}, ncap={}, stack={}, vals={any})", .{
                dst, func_id, union_t, num_captured, is_stack_closure,
                std.mem.sliceAsBytes(pc[7..7+num_captured]),
            });
        },
        .true => {
            const dst = pc[1].val;
            try w.print("%{} = true", .{dst});
        },
        .false => {
            const dst = pc[1].val;
            try w.print("%{} = false", .{dst});
        },
        .nops => {
            const ptr_len = @as(*const align(2) u64, @ptrCast(pc + 1)).*;
            const ptr: [*]u8 = @ptrFromInt(@as(usize, @intCast(ptr_len & 0xffffffffffff)));
            const len_: usize = @intCast(ptr_len >> 48);
            const str = ptr[0..len_];
            try w.print("'{s}'", .{str});
        },
        .const_str => {
            const dst = pc[1].val;
            const ptr_len = @as(*const align(2) u64, @ptrCast(pc + 2)).*;
            const ptr: [*]u8 = @ptrFromInt(@as(usize, @intCast(ptr_len & 0xffffffffffff)));
            const len_: usize = @intCast(ptr_len >> 48);
            const ascii = pc[6].val == 1;
            const str = ptr[0..len_];
            try w.print("%{}..%{} = '{s}' ascii={}", .{dst, dst+3, str, ascii});
        },
        .const_16 => {
            const dst = pc[1].val;
            const val = pc[2].val;
            try w.print("%{} = {}", .{dst, val});
        },
        .const_16s => {
            const dst = pc[1].val;
            const val: i16 = @bitCast(pc[2].val);
            try w.print("%{} = {}", .{dst, val});
        },
        .const_64 => {
            const dst = pc[1].val;
            const val = @as(*const align (2) u64, @ptrCast(pc + 2)).*;
            try w.print("%{} = {}", .{dst, val});
        },
        .mov_4,
        .mov_3,
        .mov_2,
        .mov => {
            const dst = pc[1].val;
            const src = pc[2].val;
            try w.print("%{} = %{}", .{dst, src});
        },
        .cmp_str,
        .cmp_8,
        .cmp => {
            const dst = pc[1].val;
            const left = pc[2].val;
            const right = pc[3].val;
            try w.print("%{} = (%{} == %{})", .{dst, left, right});
        },
        // .indexMap => {
        //     const map = pc[1].val;
        //     const index = pc[2].val;
        //     const dst = pc[3].val;
        //     _ = try fmt.printCount(w, "%{} = %{}[%{}]", &.{v(dst), v(map), v(index)});
        // },
        // .indexList => {
        //     const list = pc[1].val;
        //     const index = pc[2].val;
        //     const dst = pc[3].val;
        //     _ = try fmt.printCount(w, "%{} = %{}[%{}]", &.{v(dst), v(list), v(index)});
        // },
        .jump => {
            const jump = pc[1].val;
            const abs = @abs(jump);
            if (jump < 0) {
                if (pc_off) |off| {
                    try w.print("jmp @{}", .{off - abs});
                } else {
                    try w.print("jmp {*}", .{pc - abs});
                }
            } else {
                if (pc_off) |off| {
                    try w.print("jmp @{}", .{off + abs});
                } else {
                    try w.print("jmp {*}", .{pc + abs});
                }
            }
        },
        .jump_t => {
            const cond = pc[1].val;
            const jump = pc[2].val;
            if (pc_off) |off| {
                try w.print("if (%{}) jmp @{}", .{cond, off + jump});
            } else {
                try w.print("if (%{}) jmp {*}", .{cond, &pc[jump]});
            }
        },
        .jump_f => {
            const cond = pc[1].val;
            const jump = pc[2].val;
            if (pc_off) |off| {
                try w.print("if (!%{}) jmp @{}", .{cond, off + jump});
            } else {
                try w.print("if (!%{}) jmp {*}", .{cond, &pc[jump]});
            }
        },
        .is_zero => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const val = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            try w.print("%{} = (%{} == 0)", .{dst, val});
        },
        // .list => {
        //     const startLocal = pc[1].val;
        //     const numElems = pc[2].val;
        //     const type_id = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
        //     const dst = pc[5].val;
        //     _ = try fmt.printCount(w, "%{} = List(type={}){{%{}..%{}}", &.{v(dst), v(type_id), v(startLocal), v(startLocal+numElems)});
        // },
        // .map => {
        //     const dst = pc[1].val;
        //     _ = try fmt.printCount(w, "%{} = new Map", &.{ v(dst) });
        // },
        .trait => {
            const src = pc[1].val;
            const type_id = @as(*const align(1) u16, @ptrCast(pc + 2)).*;
            const vtable = @as(*const align(1) u16, @ptrCast(pc + 4)).*;
            const dst = pc[6].val;

            try w.print("%{} = trait(type={}, vtable={}, %{})", .{
                dst, type_id, vtable, src,
            });
        },
        .new => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const type_id = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            const name = try vm.sema.allocTypeName(vm.sema.types.items[type_id]);
            defer vm.alloc.free(name);
            const size = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            try w.print("%{} = {s}{{size={}}}", .{
                dst, name, size,
            });
        },
        .retain => {
            const reg = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            try w.print("%{}.rc += 1", .{reg});
        },
        .release_opt,
        .release => {
            const reg = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const jump = @as(*const align(1) i16, @ptrCast(pc + 3)).*;
            try w.print("%{}.rc -= 1; jmp {}", .{reg, jump});
        },
        .dtor_str => {
            const reg = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            try w.print("destruct [%{}]", .{reg});
        },
        .store_4w,
        .store_3w,
        .store_2w,
        .store_64,
        .store_32,
        .store_16,
        .store_8 => {
            const dst_ptr = pc[1].val;
            const dst_off = pc[2].val;
            const src = pc[3].val;
            try w.print("[%{} + {}] = %{}", .{dst_ptr, dst_off, src});
        },
        .store_n => {
            const dst_ptr = pc[1].val;
            const dst_off = pc[2].val;
            const src = pc[3].val;
            const size = pc[4].val;
            try w.print("[%{} + {}] = %{}.. size={}", .{dst_ptr, dst_off, src, size});
        },
        .await_op => {
            const local = pc[1].val;
            try w.print("await(%{})", .{local});
        },
        .addr => {
            const dst = pc[1].val;
            const reg = pc[2].val;
            try w.print("%{} = *%{}", .{dst, reg});
        },
        .add_i16 => {
            const dst = pc[1].val;
            const left = pc[2].val;
            const right_amt: i16 = @bitCast(pc[3].val);
            try w.print("%{} = %{} + {}", .{dst, left, right_amt});
        },
        .load_n => {
            const dst = pc[1].val;
            const src_ptr = pc[2].val;
            const src_off = pc[3].val;
            const size = pc[4].val;
            try w.print("%{} = [%{} + {}] size={}", .{dst, src_ptr, src_off, size});
        },
        .load_2w,
        .load_3w,
        .load_4w,
        .load_8,
        .load_32,
        .load_16,
        .load_64 => {
            const dst = pc[1].val;
            const src = pc[2].val;
            const src_off = pc[3].val;
            try w.print("%{} = [%{} + {}]", .{dst, src, src_off});
        },
        // .setIndexList => {
        //     const list = pc[1].val;
        //     const index = pc[2].val;
        //     const right = pc[3].val;
        //     _ = try fmt.printCount(w, "list={}, index={}, right={}", &.{v(list), v(index), v(right)});
        // },
        .ret_gen => {
            const type_id = pc[1].val;
            const ret_size = pc[2].val;
            const reg_end = pc[3].val;
            const resume_off = pc[4].val;
            try w.print("ret gen(type={}, %{}..%{}, resume_off={}, ret_size={})",
                .{type_id, ret_size+4, reg_end, resume_off, ret_size});
        },
        .gen_next => {
            const ret = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const base = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            const gen = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            try w.print("%{}..%{} = next(%{})", .{ret, base, gen });
        },
        .ret_y => {
            const ret_size = pc[1].val;
            const resume_off = pc[2].val;
            const done = pc[3].val == 1;
            try w.print("yield ret={}, resume_off={}, done={}", .{ret_size, resume_off, done});
        },
        // .sliceList => {
        //     const recv = pc[1].val;
        //     const range = pc[2].val;
        //     const dst = pc[3].val;
        //     _ = try fmt.printCount(w, "%{} = %{}[Range(%{})]", &.{v(dst), v(recv), v(range)});
        // },
        .castAbstract => {
            const dst = pc[1].val;
            const child = pc[2].val;
            const expTypeId = pc[3].val;
            try w.print("%{} = cast(type={}, %{})", .{dst, expTypeId, child});
        },
        .cast => {
            const dst = pc[1].val;
            const child = pc[2].val;
            const expTypeId = pc[3].val;
            try w.print("%{} = cast(type={}, %{})", .{dst, expTypeId, child});
        },
        .memsetz => {
            const dst_ptr = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const offset = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            const len = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            try w.print("[%{}+{}..{}] = 0", .{dst_ptr, offset, offset+len});
        },
        .lnot => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const cond = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            try w.print("%{} = !%{}", .{dst, cond});
        },
        .sext => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const src = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            const bits = pc[5].val;
            try w.print("%{} = sext({}, %{})", .{dst, bits, src});
        },
        .zext => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const src = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            const bits = pc[5].val;
            try w.print("%{} = zext({}, %{})", .{dst, bits, src});
        },
        .fn_union => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const src = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            const type_id = @as(*const align(1) u16, @ptrCast(pc + 5)).*;
            try w.print("%{} = fnunion(%{}, type={})", .{dst, src, type_id});
        },
        .unwrap_nz => {
            const dst = @as(*const align(1) u16, @ptrCast(pc + 1)).*;
            const src = @as(*const align(1) u16, @ptrCast(pc + 3)).*;
            try w.print("%{} = %{}.?", .{dst, src});
        },
        .unwrap_addr => {
            const dst = pc[1].val;
            const choice_s = pc[2].val;
            const tag = pc[3].val;
            const offset = pc[4].val;
            try w.print("%{} = %{}.!{} + {}", .{dst, choice_s, tag, offset});
        },
        else => {},
    }

    if (opts.extra) |extra| {
        if (extra.len > 0) {
            try w.print(" | {s}", .{extra});
        }
    }
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
    val: u16,

    pub inline fn initOpCode(code_: OpCode) Inst {
        return .{
            .val = @intFromEnum(code_),
        };
    }

    pub inline fn initOpCode2(code_: OpCode, tmp_dst: bool) Inst {
        return .{
            .val = @intFromEnum(code_) | @as(u16, if (tmp_dst) 0x100 else 0),
        };
    }

    pub inline fn opcode(self: *const Inst) OpCode {
        const code: u8 = @truncate(self.val);
        return @enumFromInt(code);
    }

    pub inline fn temp_dst(self: *const Inst) bool {
        return (self.val >> 8) == 1;
    }

    pub fn initArg(arg: u8) Inst {
        return .{
            .val = arg,
        };
    }
};

pub const DebugTableEntry = extern struct {
    ptr: [*]Inst,
    chunk: *cy.Chunk,
};

pub const DebugSym = extern struct {
    /// Start position of an inst.
    pc: u32,

    /// Source pos.
    loc: u32,

    /// Indexes into `Chunk.funcs_debug`. The source chunk is `FuncDebugInfo.src_chunk`.
    frameLoc: u32,

    /// ChunkId.
    file: u16,
};

pub const PcPtrLayout = extern struct {
    pc: u32,
    layout: u32,
};

pub const CallTraitInstLen = vmc.CALL_TRAIT_INST_LEN;
pub const CallPtrInstLen = vmc.CALL_PTR_INST_LEN;
pub const CallUnionInstLen = vmc.CALL_UNION_INST_LEN;

test "getInstLenAt" {
    var code = Inst.initOpCode(.call);
    try t.eq(vmc.CALL_INST_LEN, getInstLenAt(@ptrCast(&code)));
}

pub fn getInstLenAt(pc: [*]const Inst) u8 {
    switch (pc[0].opcode()) {
        .end,
        .ret_0,
        .ret,
        .trap_debug,
        .trap => {
            return 1;
        },
        .retain_nz => {
            return 2;
        },
        .await_op,
        .retain,
        .true,
        .false,
        .dtor_str,
        .ret_n,
        .call_ptr,
        .call_union,
        .jump => {
            return 2;
        },
        .const_16s,
        .const_16 => {
            return 3;
        },
        .fneg32,
        .fneg,
        .neg,
        .release_opt,
        .release,
        .unwrap_nz,
        .i2f,
        .i2f32,
        .f2i,
        .f32_2i,
        .fabs,
        .f32abs,
        .is_zero,
        .lnot,
        .not,
        // .map,
        // .appendList,
        // .sliceList,
        .jump_t,
        .jump_f,
        .extern_func,
        .call_trait,
        .mov,
        .mov_2,
        .mov_3,
        .mov_4,
        .addr,
        .chk_stk,
        // .setIndexList,
        // .setIndexMap,
        // .indexList,
        // .indexMap,
        .nop32 => {
            return 3;
        },
        .enter_jit,
        .ret_y,
        .captured,
        // .list,
        .zext,
        .sext => {
            return 4;
        },
        .gen_next,
        .gen_end,
        .fn_union,
        .lsl,
        .lsr,
        .fadd,
        .fadd32,
        .fsub,
        .fsub32,
        .fmul,
        .fmul32,
        .fdiv,
        .fdiv32,
        .fmod,
        .fmod32,
        .add,
        .sub,
        .imul,
        .mul,
        .idiv,
        .div,
        .pow,
        .imod,
        .mod,
        .and_,
        .or_,
        .xor,
        .cmp_8,
        .cmp,
        .cmp_str,
        .castAbstract,
        .memsetz,
        .feq32,
        .feq,
        .flt,
        .fgt,
        .fle,
        .fge,
        .flt32,
        .fgt32,
        .fle32,
        .fge32,
        .lt,
        .gt,
        .le,
        .ge,
        .ult,
        .ugt,
        .ule,
        .uge,
        .add_i16,
        .mov_n,
        .new,
        .const_32,
        .load_8,
        .load_16,
        .load_32,
        .load_64,
        .load_2w,
        .load_3w,
        .load_4w,
        .store_8,
        .store_16,
        .store_32,
        .store_64,
        .store_2w,
        .store_3w,
        .store_4w => {
            return 4;
        },
        .ret_gen,
        .cast => {
            return 5;
        },
        .unwrap_addr,
        .fn_vm,
        .fn_host,
        .trait,
        .load_n,
        .store_n,
        .call,
        .call_host,
        .nops => {
            return 5;
        },
        .const_64 => {
            return 6;
        },
        .const_str => {
            return 7;
        },
        .closure => {
            const numCaptured = pc[6].val & 0x7fff;
            return 7 + @as(u8, @intCast(numCaptured));
        },
    }
}

pub const OpCode = enum(u8) {
    /// Copies a constant value from `consts` to a dst local.
    const_64 = vmc.CodeCONST_64,
    const_str = vmc.CodeCONST_STR,
    const_16s = vmc.CodeCONST_16S,
    const_16 = vmc.CodeCONST_16,
    const_32 = vmc.CodeCONST_32,
    true = vmc.CodeTrue,
    false = vmc.CodeFalse,
    lnot = vmc.CodeLNOT,
    is_zero = vmc.CodeIsZero,
    mov = vmc.CodeMOV,
    mov_2 = vmc.CodeMOV_2,
    mov_3 = vmc.CodeMOV_3,
    mov_4 = vmc.CodeMOV_4,
    mov_n = vmc.CodeMOV_N,

    // setIndexList = vmc.CodeSetIndexList,
    // setIndexMap = vmc.CodeSetIndexMap,

    // indexList = vmc.CodeIndexList,
    // indexMap = vmc.CodeIndexMap,

    // appendList = vmc.CodeAppendList,

    /// First operand points the first elem and also the dst local. Second operand contains the number of elements.
    // list = vmc.CodeList,
    /// First operand points the first entry value and also the dst local. Second operand contains the number of elements.
    /// Const key indexes follow the size operand.
    // map = vmc.CodeMap,
    // sliceList = vmc.CodeSliceList,
    jump_f = vmc.CodeJUMP_F,
    jump_t = vmc.CodeJUMP_T,
    /// Jumps the pc by an 16-bit integer offset.
    jump = vmc.CodeJUMP,

    release_opt = vmc.CodeReleaseOpt,
    release = vmc.CodeRelease,
    dtor_str = vmc.CodeDTOR_STR,

    chk_stk = vmc.CodeCHK_STK,
    call = vmc.CodeCALL,
    call_host = vmc.CodeCALL_HOST,
    call_trait = vmc.CodeCALL_TRAIT,
    enter_jit = vmc.CodeENTER_JIT,
    ret_0 = vmc.CodeRET_0,
    ret = vmc.CodeRET,
    ret_n = vmc.CodeRET_N,
    call_ptr = vmc.CodeCALL_PTR,
    call_union = vmc.CodeCALL_UNION,

    load_8 = vmc.CodeLOAD_8,
    load_16 = vmc.CodeLOAD_16,
    load_32 = vmc.CodeLOAD_32,
    load_64 = vmc.CodeLOAD_64,
    load_2w = vmc.CodeLOAD_2W,
    load_3w = vmc.CodeLOAD_3W,
    load_4w = vmc.CodeLOAD_4W,
    load_n = vmc.CodeLOAD_N,

    closure = vmc.CodeCLOSURE,
    cmp_8 = vmc.CodeCMP_8,
    cmp = vmc.CodeCMP,
    cmp_str = vmc.CodeCMP_STR,

    feq32 = vmc.CodeFEQ32,
    flt32 = vmc.CodeFLT32,
    fgt32 = vmc.CodeFGT32,
    fle32 = vmc.CodeFLE32,
    fge32 = vmc.CodeFGE32,
    fadd32 = vmc.CodeFADD32,
    fsub32 = vmc.CodeFSUB32,
    fmul32 = vmc.CodeFMUL32,
    fdiv32 = vmc.CodeFDIV32,
    fmod32 = vmc.CodeFMOD32,
    fneg32 = vmc.CodeFNEG32,

    feq = vmc.CodeFEQ,
    flt = vmc.CodeFLT,
    fgt = vmc.CodeFGT,
    fle = vmc.CodeFLE,
    fge = vmc.CodeFGE,
    fadd = vmc.CodeFADD,
    fsub = vmc.CodeFSUB,
    fmul = vmc.CodeFMUL,
    fdiv = vmc.CodeFDIV,
    fmod = vmc.CodeFMOD,
    fneg = vmc.CodeFNEG,

    lt = vmc.CodeLT,
    gt = vmc.CodeGT,
    le = vmc.CodeLE,
    ge = vmc.CodeGE,
    ult = vmc.CodeULT,
    ugt = vmc.CodeUGT,
    ule = vmc.CodeULE,
    uge = vmc.CodeUGE,

    new = vmc.CodeNEW,
    trait = vmc.CodeTrait,

    addr = vmc.CodeADDR,
    unwrap_addr = vmc.CodeUnwrapAddr,
    unwrap_nz = vmc.CodeUnwrapNZ,

    store_8 = vmc.CodeSTORE_8,
    store_16 = vmc.CodeSTORE_16,
    store_32 = vmc.CodeSTORE_32,
    store_64 = vmc.CodeSTORE_64,
    store_2w = vmc.CodeSTORE_2W,
    store_3w = vmc.CodeSTORE_3W,
    store_4w = vmc.CodeSTORE_4W,
    store_n = vmc.CodeSTORE_N,
    memsetz = vmc.CodeMEMSETZ,

    ret_gen = vmc.CodeRET_GEN,
    ret_y = vmc.CodeRET_Y,
    gen_next = vmc.CodeGEN_NEXT,
    gen_end = vmc.CodeGEN_END,
    retain_nz = vmc.CodeRetainNZ,
    retain = vmc.CodeRetain,
    captured = vmc.CodeCaptured,

    and_ = vmc.CodeAND,
    or_ = vmc.CodeOR,
    xor = vmc.CodeXOR,
    not = vmc.CodeNOT,
    lsl = vmc.CodeLSL,
    lsr = vmc.CodeLSR,
    add = vmc.CodeAdd,
    add_i16 = vmc.CodeAddI16,
    sub = vmc.CodeSub,
    imul = vmc.CodeIMUL,
    mul = vmc.CodeMUL,
    idiv = vmc.CodeIDIV,
    div = vmc.CodeDIV,
    mod = vmc.CodeMOD,
    imod = vmc.CodeIMOD,
    pow = vmc.CodePow,
    neg = vmc.CodeNeg,
    zext = vmc.CodeZEXT,
    sext = vmc.CodeSEXT,
    f2i = vmc.CodeF2I,
    f32_2i = vmc.CodeF32_2I,
    i2f = vmc.CodeI2F,
    i2f32 = vmc.CodeI2F32,
    fabs = vmc.CodeFABS,
    f32abs = vmc.CodeF32ABS,
    fn_vm = vmc.CodeFN_VM,
    fn_host = vmc.CodeFN_HOST,
    fn_union = vmc.CodeFN_UNION,
    extern_func = vmc.CodeExternFunc,

    cast = vmc.CodeCast,
    castAbstract = vmc.CodeCastAbstract,

    await_op = vmc.CodeAWAIT,

    nop32 = vmc.CodeNOP32,
    nops = vmc.CodeNOPS,
    trap = vmc.CodeTRAP,
    trap_debug = vmc.CodeTRAP_DEBUG,

    /// Indicates the end of the main script.
    end = vmc.CodeEND,
};

test "bytecode internals." {
    try t.eq(130, std.enums.values(OpCode).len);
    try t.eq(@sizeOf(Inst), 2);
    try t.eq(@sizeOf(DebugSym), 16);
}

pub const PtrLayoutContext = struct {
    pub fn hash(_: @This(), key: []const bool) u64 {
        var c = std.hash.Wyhash.init(0);
        c.update(std.mem.sliceAsBytes(key));
        return c.final();
    }
    pub fn eql(_: @This(), a: []const bool, b: []const bool) bool {
        return std.mem.eql(bool, a, b);
    }
};
