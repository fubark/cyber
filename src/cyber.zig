const builtin = @import("builtin");

pub const ast = @import("ast.zig");
pub const BinaryExprOp = ast.BinaryExprOp;
pub const UnaryOp = ast.UnaryOp;

pub const tokenizer = @import("tokenizer.zig");
pub const Tokenizer = tokenizer.Tokenizer;
pub const TokenType = tokenizer.TokenType;

pub const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const ParseResultView = parser.ResultView;
pub const ParseResult = parser.Result;

pub const sema = @import("sema.zig");
pub const sema_func = @import("sema_func.zig");
pub const sema_type = @import("sema_type.zig");
pub const ct_inline = @import("ct_inline.zig");
pub const Sema = sema.Sema;
pub const unescapeString = sema.unescapeString;
pub const FuncSig = sema.FuncSig;

pub const cte = @import("cte.zig");
pub const template = @import("template.zig");
pub const Instance = template.Instance;

pub const ir = @import("ir.zig");
pub const dce = @import("dce.zig");

pub const cgen = @import("cgen.zig");
pub const jitgen = @import("jit/gen.zig");

pub const types = @import("types.zig");
pub const TypeId = types.TypeId;
pub const Type = types.Type;

pub const module = @import("module.zig");
pub const Module = module.Module;

pub const sym = @import("sym.zig");
pub const Sym = sym.Sym;
pub const Func = sym.Func;

pub const compiler = @import("compiler.zig");
pub const Compiler = compiler.Compiler;
pub const Report = compiler.Report;
pub const CompileResult = compiler.CompileResult;

pub const chunk = @import("chunk.zig");
pub const Chunk = chunk.Chunk;
pub const ChunkId = chunk.ChunkId;
pub const FuncId = chunk.FuncId;

pub const core = @import("builtins/core.zig");
pub const bindings = @import("builtins/bindings.zig");

pub const hash = @import("hash.zig");
pub const fmt = @import("fmt.zig");

pub const value = @import("value.zig");
pub const Value = value.Value;
pub const TypeValue = value.TypeValue;

pub const vm = @import("vm.zig");
pub const VM = vm.VM;

pub const heap = @import("heap.zig");
pub const Heap = heap.Heap;
pub const HeapObject = heap.HeapObject;
pub const MapValue = heap.MapValue;
pub const Pointer = heap.Pointer;

pub const sync = @import("sync.zig");
pub const thread = @import("thread.zig");
pub const Thread = thread.Thread;
pub const worker = @import("worker.zig");

pub const vmc = @import("vmc");
pub const Fiber = vmc.Fiber;

// pub const fifo = @import("fifo.zig");
pub const BoundedArray = @import("bounded_array.zig").BoundedArray;

pub const debug = @import("debug.zig");
pub const StackTrace = debug.StackTrace;

pub const string = @import("string.zig");

pub const bytecode = @import("bytecode.zig");
pub const ByteCodeBuffer = bytecode.ByteCodeBuffer;
pub const OpCode = bytecode.OpCode;
pub const Inst = bytecode.Inst;
pub const DebugSym = bytecode.DebugSym;
pub const DebugTableEntry = bytecode.DebugTableEntry;
pub const getInstLenAt = bytecode.getInstLenAt;

pub const simd = @import("simd.zig");

pub const isFreestanding = builtin.os.tag == .freestanding;
pub const isWasm = builtin.cpu.arch.isWasm();
pub const isWasmFreestanding = isWasm and builtin.os.tag == .freestanding;
pub const is32Bit = build_config.is32Bit;
pub const hasStdFiles = !isWasm;
pub const hasCYC = build_config.cyc;
pub const hasFFI = build_config.ffi;
pub const hasJIT = build_config.jit;
pub const hasCLI = build_config.cli;

const build_config = @import("build_config");
pub var event_id: u64 = 1;
pub const Trace = build_config.trace;
pub const TraceRC = Trace and true;
pub const Malloc = build_config.malloc;
pub var tempBuf: [1000]u8 align(4) = undefined;

const std = @import("std");
pub const NullId = std.math.maxInt(u32);
pub const NullU8 = std.math.maxInt(u8);
pub const NullU16 = std.math.maxInt(u16);
pub const NullU24 = std.math.maxInt(u24);

/// Document that a id type can contain NullId.
pub fn Nullable(comptime T: type) type {
    return T;
}

pub const ConstEvalContext = extern struct {
    func: *Func,
    args: [*]const Value,
    node: *ast.Node,
};

pub const BuiltinContext = extern struct {
    func: *Func,
    args: [*]const *ast.Node,
    node: *ast.Node,
};

pub const ZHostFn = *const fn(*VM) callconv(.c) C.Ret;
pub const ZBuiltinFn = *const fn(*Chunk, *const BuiltinContext, *sema.ExprResult) callconv(.c) bool;
pub const ZBuiltinEvalFn = *const fn(*Chunk, *const BuiltinContext) callconv(.c) TypeValue;
pub const ZConstEvalFn = *const fn(*Chunk, *const ConstEvalContext) callconv(.c) TypeValue;

pub const log = @import("log.zig");
pub const utils = @import("utils.zig");
pub const IndexSlice = utils.IndexSlice;
pub const ptrAlignCast = utils.ptrAlignCast;
pub const panic = utils.panic;
pub const panicFmt = utils.panicFmt;
pub const dassert = utils.dassert;

pub inline fn unexpected() noreturn {
    panic("unexpected");
}

pub inline fn unsupported() noreturn {
    panic("unsupported");
}

pub inline fn fatal() noreturn {
    panic("error");
}

pub const C = @import("capi.zig");
