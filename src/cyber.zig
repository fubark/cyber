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
pub const FuncDecl = parser.FuncDecl;
pub const FunctionParam = parser.FunctionParam;
pub const Token = parser.Token;
pub const Tokenizer = parser.Tokenizer;
pub const TokenizeState = parser.TokenizeState;
pub const TokenType = parser.TokenType;

pub const sema = @import("sema.zig");
pub const Module = sema.Module;

const vm_compiler = @import("vm_compiler.zig");
pub const VMcompiler = vm_compiler.VMcompiler;
pub const CompileChunk = vm_compiler.CompileChunk;
pub const unescapeString = vm_compiler.unescapeString;

pub const bindings = @import("builtins/bindings.zig");

pub const codegen = @import("codegen.zig");

pub const value = @import("value.zig");
pub const Value = value.Value;
pub const ValuePair = value.ValuePair;
pub const BooleanT = value.BooleanT;
pub const NoneT = value.NoneT;
pub const ErrorT = value.ErrorT;
pub const StaticAstringT = value.StaticAstringT;
pub const StaticUstringT = value.StaticUstringT;
pub const UserTagT = value.UserTagT;
pub const UserTagLiteralT = value.UserTagLiteralT;
pub const IntegerT = value.IntegerT;
pub const NumberT = value.NumberT;
pub const StaticUstringHeader = value.StaticUstringHeader;

pub const ValueUserTag = value.ValueUserTag;

pub const vm = @import("vm.zig");
pub const EvalConfig = vm.EvalConfig;
pub const TypeId = vm.TypeId;
pub const getUserVM = vm.getUserVM;
pub const VM = vm.VM;
pub const MethodSym = vm.MethodSym;
pub const FuncSymbolEntry = vm.FuncSymbolEntry;
pub const VarSym = vm.VarSym;
pub const TraceInfo = vm.TraceInfo;
pub const OpCount = vm.OpCount;
pub const EvalError = vm.EvalError;
pub const buildReturnInfo = vm.buildReturnInfo;
pub const framePtrOffset = vm.framePtrOffset;
pub const pcOffset = vm.pcOffset;
pub const FuncSymDetail = vm.FuncSymDetail;
pub const KeyU96 = vm.KeyU96;
pub const KeyU96Context = vm.KeyU96Context;
pub const StringType = vm.StringType;

const api = @import("api.zig");
pub const UserVM = api.UserVM;

pub const heap = @import("heap.zig");
pub const HeapObject = heap.HeapObject;
pub const ListS = heap.ListS;
pub const ListIteratorT = heap.ListIteratorT;
pub const MapS = heap.MapS;
pub const MapIteratorT = heap.MapIteratorT;
pub const MapInner = heap.MapInner;
pub const Map = heap.Map;
pub const ClosureS = heap.ClosureS;
pub const LambdaS = heap.LambdaS;
pub const AstringT = heap.AstringT;
pub const UstringT = heap.UstringT;
pub const StringSliceT = heap.StringSliceT;
pub const RawStringT = heap.RawStringT;
pub const RawStringSliceT = heap.RawStringSliceT;
pub const FiberS = heap.FiberS;
pub const BoxS = heap.BoxS;
pub const NativeFunc1S = heap.NativeFunc1S;
pub const TccStateS = heap.TccStateS;
pub const OpaquePtrS = heap.OpaquePtrS;
pub const FileT = heap.FileT;
pub const DirT = heap.DirT;
pub const DirIteratorT = heap.DirIteratorT;
pub const Dir = heap.Dir;
pub const DirIterator = heap.DirIterator;
pub const SymbolT = heap.SymbolT;
pub const CyList = heap.List;
pub const Closure = heap.Closure;
pub const OpaquePtr = heap.OpaquePtr;
pub const Astring = heap.Astring;
pub const RawString = heap.RawString;
pub const MaxPoolObjectStringByteLen = heap.MaxPoolObjectAstringByteLen;
pub const MaxPoolObjectRawStringByteLen = heap.MaxPoolObjectRawStringByteLen;

pub const fiber = @import("fiber.zig");
pub const Fiber = fiber.Fiber;

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

const bytecode = @import("bytecode.zig");
pub const ByteCodeBuffer = bytecode.ByteCodeBuffer;
pub const OpCode = bytecode.OpCode;
pub const OpData = bytecode.OpData;
pub const Const = bytecode.Const;
pub const DebugSym = bytecode.DebugSym;
pub const getInstLenAt = bytecode.getInstLenAt;

const cdata = @import("cdata.zig");
pub const encodeCyon = cdata.encode;
pub const decodeCyonMap = cdata.decodeMap;
pub const decodeCyon = cdata.decode;
pub const EncodeValueContext = cdata.EncodeValueContext;
pub const EncodeMapContext = cdata.EncodeMapContext;
pub const EncodeListContext = cdata.EncodeListContext;
pub const DecodeMapIR = cdata.DecodeMapIR;
pub const DecodeListIR = cdata.DecodeListIR;
pub const DecodeValueIR = cdata.DecodeValueIR;

/// Sections don't work on macOS arm64 builds but they are needed for the build.
pub const HotSection = if (builtin.os.tag == .macos) "__TEXT,.eval" else ".eval";
pub const Section = if (builtin.os.tag == .macos) "__TEXT,.eval2" else ".eval2";
pub const StdSection = if (builtin.os.tag == .macos) "__TEXT,.eval.std" else ".eval.std";
pub const CompilerSection = if (builtin.os.tag == .macos) "__TEXT,.compiler" else ".compiler";
pub const InitSection = if (builtin.os.tag == .macos) "__TEXT,.cyInit" else ".cyInit";

pub export fn initSection() linksection(InitSection) callconv(.C) void {}
pub export fn compilerSection() linksection(CompilerSection) callconv(.C) void {}
pub export fn stdSection() linksection(StdSection) callconv(.C) void {}
pub export fn section() linksection(Section) callconv(.C) void {}
pub export fn hotSection() linksection(HotSection) callconv(.C) void {}

/// Force the compiler to order linksection first on given function.
/// Use exported c function so release builds don't remove them.
pub fn forceSectionDep(_: *const fn() callconv(.C) void) void {} 

pub fn forceSectionDeps() !void {
    forceSectionDep(hotSection);
    forceSectionDep(section);
    forceSectionDep(stdSection);
    forceSectionDep(compilerSection);
    forceSectionDep(initSection);
}

/// Compile errors are not printed.
pub var silentError = false;

/// Internal messages are not printed.
pub var silentInternal = false;

pub const simd = @import("simd.zig");

pub const isWasm = builtin.cpu.arch.isWasm();
pub const hasJit = !isWasm;
pub const hasStdFiles = !isWasm;

pub const TraceEnabled = @import("build_options").trace;
pub const TrackGlobalRC = builtin.mode != .ReleaseFast;

const std = @import("std");
pub const NullId = std.math.maxInt(u32);
pub const NullU8 = std.math.maxInt(u8);

pub const NativeObjFuncPtr = *const fn (*UserVM, Value, [*]const Value, u8) Value;
pub const NativeObjFunc2Ptr = *const fn (*UserVM, Value, [*]const Value, u8) ValuePair;
pub const NativeFuncPtr = *const fn (*UserVM, [*]const Value, u8) Value;
pub const NativeErrorFunc = fn (*UserVM, [*]const Value, u8) anyerror!Value;
