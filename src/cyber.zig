// Copyright (c) 2023 Cyber (See LICENSE)

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
pub const Sema = sema.Sema;
pub const unescapeString = sema.unescapeString;

pub const cte = @import("cte.zig");

pub const ir = @import("ir.zig");

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

pub const builtins = @import("builtins/builtins.zig");
pub const bindings = @import("builtins/bindings.zig");

pub const hash = @import("hash.zig");
pub const rt = @import("runtime.zig");
pub const Context = rt.Context;
pub const fmt = @import("fmt.zig");
pub const http = @import("http.zig");

pub const value = @import("value.zig");
pub const Value = value.Value;

pub const vm = @import("vm.zig");
pub const VM = vm.VM;
pub const getStackOffset = vm.getStackOffset;
pub const getInstOffset = vm.getInstOffset;

pub const heap = @import("heap.zig");
pub const HeapObject = heap.HeapObject;
pub const MapInner = heap.MapInner;
pub const Map = heap.Map;
pub const CyList = heap.List;
pub const Pointer = heap.Pointer;
pub const Astring = heap.Astring;
pub const MaxPoolObjectStringByteLen = heap.MaxPoolObjectAstringByteLen;

pub const fiber = @import("fiber.zig");

pub const vmc = @import("vm_c.zig");
pub const Fiber = vmc.Fiber;

pub const arc = @import("arc.zig");

const map = @import("map.zig");
pub const ValueMap = map.ValueMap;
pub const ValueMapEntry = map.ValueMapEntry;

const list = @import("list.zig");
pub const List = list.List;
pub const ListAligned = list.ListAligned;

pub const debug = @import("debug.zig");
pub const StackTrace = debug.StackTrace;
pub const StackFrame = debug.StackFrame;

pub const string = @import("string.zig");

pub const bytecode = @import("bytecode.zig");
pub const ByteCodeBuffer = bytecode.ByteCodeBuffer;
pub const OpCode = bytecode.OpCode;
pub const Inst = bytecode.Inst;
pub const DebugSym = bytecode.DebugSym;
pub const getInstLenAt = bytecode.getInstLenAt;

pub const simd = @import("simd.zig");

pub const isFreestanding = builtin.os.tag == .freestanding;
pub const isWasm = builtin.cpu.arch.isWasm();
pub const isWasmFreestanding = isWasm and builtin.os.tag == .freestanding;
pub const is32Bit = build_options.is32Bit;
pub const hasStdFiles = !isWasm;
pub const hasGC = build_options.gc;
pub const hasFFI = build_options.ffi;
pub const hasJIT = build_options.jit;
pub const hasCLI = build_options.cli;

const build_options = @import("build_options");
pub const Trace = build_options.trace;
pub const TraceRC = Trace and true;
pub const TrackGlobalRC = build_options.trackGlobalRC;
pub const Malloc = build_options.malloc;
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

pub const ZHostFuncFn = *const fn (*VM) Value;
pub fn hostFuncEntry(name: []const u8, func: ZHostFuncFn) C.HostFuncEntry {
    return .{
        .name = C.toStr(name),
        .func = @ptrCast(func),
    };
}
pub const ZHostFuncCFn = *const fn (*VM) callconv(.C) Value;

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

pub fn fromTestBackend(backend: @TypeOf(build_options.testBackend)) C.Backend {
    return switch (backend) {
        .jit => C.BackendJIT,
        .vm => C.BackendVM,
        .tcc => C.BackendTCC,
        .cc => C.BackendCC,
    };
}

pub const C = @import("capi.zig");
pub fn isAot(backend: C.Backend) bool {
    return backend == C.BackendTCC or backend == C.BackendCC or backend == C.BackendLLVM;
}