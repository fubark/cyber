// Copyright (c) 2023 Cyber (See LICENSE)

const builtin = @import("builtin");

const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const ParseResultView = parser.ResultView;
pub const ParseResult = parser.Result;
pub const Node = parser.Node;
pub const NodeType = parser.NodeType;
pub const NodeId = parser.NodeId;
pub const BinaryExprOp = parser.BinaryExprOp;
pub const UnaryOp = parser.UnaryOp;
pub const Token = parser.Token;
pub const Tokenizer = parser.Tokenizer;
pub const TokenizeState = parser.TokenizeState;
pub const TokenType = parser.TokenType;
pub const GenBinExprStrategy = parser.GenBinExprStrategy;

pub const sema = @import("sema.zig");
pub const unescapeString = sema.unescapeString;

pub const types = @import("types.zig");
pub const module = @import("module.zig");
pub const ModuleId = module.ModuleId;
pub const Module = module.Module;

pub const vm_compiler = @import("vm_compiler.zig");
pub const VMcompiler = vm_compiler.VMcompiler;
pub const CompileResultView = vm_compiler.CompileResultView;
pub const CompileConfig = vm_compiler.CompileConfig;
pub const ValidateConfig = vm_compiler.ValidateConfig;
pub const CompileErrorType = vm_compiler.CompileErrorType;

pub const chunk = @import("chunk.zig");
pub const Chunk = chunk.Chunk;
pub const ChunkId = chunk.ChunkId;

pub const register = @import("register.zig");

pub const bindings = @import("builtins/bindings.zig");

pub const hash = @import("hash.zig");
pub const rt = @import("runtime.zig");
pub const fmt = @import("fmt.zig");

pub const codegen = @import("codegen.zig");

pub const value = @import("value.zig");
pub const Value = value.Value;
pub const ValuePair = value.ValuePair;
pub const StaticUstringHeader = value.StaticUstringHeader;

pub const ValueUserTag = value.ValueUserTag;

pub const vm = @import("vm.zig");
pub const EvalConfig = vm.EvalConfig;
pub const VM = vm.VM;
pub const EvalError = vm.EvalError;
pub const buildReturnInfo = vm.buildReturnInfo;
pub const getStackOffset = vm.getStackOffset;
pub const getInstOffset = vm.getInstOffset;
pub const StringType = vm.StringType;

const api = @import("api.zig");
pub const UserVM = api.UserVM;

pub const heap = @import("heap.zig");
pub const HeapObject = heap.HeapObject;
pub const MapInner = heap.MapInner;
pub const Map = heap.Map;
pub const Dir = heap.Dir;
pub const DirIterator = heap.DirIterator;
pub const CyList = heap.List;
pub const Closure = heap.Closure;
pub const Pointer = heap.Pointer;
pub const Astring = heap.Astring;
pub const RawString = heap.RawString;
pub const MaxPoolObjectStringByteLen = heap.MaxPoolObjectAstringByteLen;
pub const MaxPoolObjectRawStringByteLen = heap.MaxPoolObjectRawStringByteLen;

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
pub const HeapStringBuilder = string.HeapStringBuilder;
pub const HeapRawStringBuilder = string.HeapRawStringBuilder;
pub const isAstring = string.isAstring;
pub const validateUtf8 = string.validateUtf8;
pub const utf8CharSliceAt = string.utf8CharSliceAt;
pub const indexOfChar = string.indexOfChar;
pub const indexOfAsciiSet = string.indexOfAsciiSet;
pub const toUtf8CharIdx = string.toUtf8CharIdx;
pub const charIndexOfCodepoint = string.charIndexOfCodepoint;
pub const getLineEnd = string.getLineEnd;
pub const prepReplacement = string.prepReplacement;
pub const replaceAtIdxes = string.replaceAtIdxes;
pub const utf8CodeAtNoCheck = string.utf8CodeAtNoCheck;

pub const bytecode = @import("bytecode.zig");
pub const ByteCodeBuffer = bytecode.ByteCodeBuffer;
pub const OpCode = bytecode.OpCode;
pub const Inst = bytecode.Inst;
pub const Const = bytecode.Const;
pub const DebugSym = bytecode.DebugSym;
pub const getInstLenAt = bytecode.getInstLenAt;

const cyon = @import("cyon.zig");
pub const encodeCyon = cyon.encode;
pub const decodeCyonMap = cyon.decodeMap;
pub const decodeCyon = cyon.decode;
pub const EncodeValueContext = cyon.EncodeValueContext;
pub const EncodeMapContext = cyon.EncodeMapContext;
pub const EncodeListContext = cyon.EncodeListContext;
pub const DecodeMapIR = cyon.DecodeMapIR;
pub const DecodeListIR = cyon.DecodeListIR;
pub const DecodeValueIR = cyon.DecodeValueIR;

/// Sections don't work on macOS arm64 builds but they are needed for the build.
pub const HotSection = if (builtin.os.tag == .macos) "__TEXT,.eval" else ".eval";
pub const Section = if (builtin.os.tag == .macos) "__TEXT,.eval2" else ".eval2";
pub const StdSection = if (builtin.os.tag == .macos) "__TEXT,.eval.std" else ".eval.std";
pub const CompilerSection = if (builtin.os.tag == .macos) "__TEXT,.compiler" else ".compiler";
pub const InitSection = if (builtin.os.tag == .macos) "__TEXT,.cyInit" else ".cyInit";

/// Whether to print verbose logs.
pub export var verbose = false;

comptime {
    @export(verbose, .{ .name = "csVerbose", .linkage = .Strong });
}

/// Compile errors are not printed.
pub var silentError = false;

/// Internal messages are not printed.
pub var silentInternal = false;

pub const simd = @import("simd.zig");

pub const isWasm = builtin.cpu.arch.isWasm();
pub const isWasmFreestanding = isWasm and builtin.os.tag == .freestanding;
pub const is32Bit = build_options.is32Bit;
pub const hasStdFiles = !isWasm;
pub const hasGC = build_options.gc;
pub const hasFFI = build_options.ffi;

const build_options = @import("build_options");
pub const Trace = build_options.trace;
pub const TrackGlobalRC = build_options.trackGlobalRC;
pub const Malloc = build_options.malloc;

const std = @import("std");
pub const NullId = std.math.maxInt(u32);
pub const NullU8 = std.math.maxInt(u8);
pub const NullU16 = std.math.maxInt(u16);

/// Document that a id type can contain NullId.
pub fn Nullable(comptime T: type) type {
    return T;
}

pub const QuickenFuncFn = *const fn (*UserVM, pc: [*]Inst, [*]const Value, u8) void;
pub const ZHostFuncFn = *const fn (*UserVM, [*]const Value, u8) Value;
pub const ZHostFuncCFn = *const fn (*UserVM, [*]const Value, u8) callconv(.C) Value;
pub const ZHostFuncPairFn = *const fn (*UserVM, [*]const Value, u8) ValuePair;

/// Overlap with `include/cyber.h` for Cyber types.
pub const Str = extern struct {
    buf: [*]const u8,
    len: usize,

    pub fn initSlice(s: []const u8) Str {
        return .{
            .buf = s.ptr,
            .len = s.len,
        };
    }

    pub fn slice(self: Str) []const u8 {
        return self.buf[0..self.len];
    }
};
pub const PreLoadModuleFn = *const fn (*UserVM, modId: vmc.ModuleId) callconv(.C) void;
pub const PostLoadModuleFn = *const fn (*UserVM, modId: vmc.ModuleId) callconv(.C) void;
pub const ModuleDestroyFn = *const fn (*UserVM, modId: vmc.ModuleId) callconv(.C) void;
pub const ModuleResolverFn = *const fn (*UserVM, ChunkId, curUri: Str, spec: Str, outUri: *Str) callconv(.C) bool;
pub const ModuleLoaderResult = extern struct {
    src: Str,
    srcIsStatic: bool,
    funcLoader: ?HostFuncLoaderFn = null,
    varLoader: ?HostVarLoaderFn = null,
    typeLoader: ?HostTypeLoaderFn = null,
    preLoad: ?PreLoadModuleFn = null,
    postLoad: ?PostLoadModuleFn = null,
    destroy: ?ModuleDestroyFn = null,
};
pub const ModuleLoaderFn = *const fn (*UserVM, Str, out: *ModuleLoaderResult) callconv(.C) bool;
pub const HostFuncInfo = extern struct {
    modId: vmc.ModuleId,
    name: Str,
    funcSigId: vmc.FuncSigId,
    idx: u32,
};
pub const HostFuncType = enum(u8) {
    standard,
    quicken,
};
pub const HostFuncResult = extern struct {
    ptr: vmc.HostFuncFn,
    type: HostFuncType,
};
pub const HostFuncLoaderFn = *const fn (*UserVM, HostFuncInfo, *HostFuncResult) callconv(.C) bool;
pub const HostVarInfo = extern struct {
    modId: vmc.ModuleId,
    name: Str,
    idx: u32,
};
pub const HostVarLoaderFn = *const fn (*UserVM, HostVarInfo, *Value) callconv(.C) bool; 
pub const HostTypeType = enum(u8) {
    object,
    coreObject,
};
pub const HostTypeInfo = extern struct {
    modId: vmc.ModuleId,
    name: Str,
    idx: u32,
};
pub const HostTypeResult = extern struct {
    data: extern union {
        object: extern struct {
            typeId: *rt.TypeId,
            semaTypeId: *types.TypeId,
            finalizer: ?ObjectFinalizerFn,
        },
        coreObject: extern struct {
            typeId: rt.TypeId,
            semaTypeId: types.TypeId,
        },
    },
    type: HostTypeType,
};
pub const HostTypeLoaderFn = *const fn (*UserVM, HostTypeInfo, *HostTypeResult) callconv(.C) bool;
pub const ObjectFinalizerFn = *const fn (*UserVM, ?*anyopaque) callconv (.C) void;
pub const PrintFn = *const fn (*UserVM, str: Str) callconv(.C) void;

pub const cli = @import("cli.zig");
pub const log = @import("log.zig");
pub const utils = @import("utils.zig");
pub const IndexSlice = utils.IndexSlice;
pub const ptrAlignCast = utils.ptrAlignCast;
pub const panic = utils.panic;
pub const panicFmt = utils.panicFmt;

pub inline fn unexpected() noreturn {
    panic("unexpected");
}

pub inline fn unsupported() noreturn {
    panic("unsupported");
}

pub inline fn fatal() noreturn {
    panic("error");
}