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

const sema = @import("sema.zig");
pub const Module = sema.Module;

const vm_compiler = @import("vm_compiler.zig");
pub const VMcompiler = vm_compiler.VMcompiler;
pub const unescapeString = vm_compiler.unescapeString;

const value = @import("value.zig");
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

const vm = @import("vm.zig");
pub const getUserVM = vm.getUserVM;
pub const UserVM = vm.UserVM;
pub const VM = vm.VM;
pub const MethodSym = vm.MethodSym;
pub const FuncSymbolEntry = vm.FuncSymbolEntry;
pub const VarSym = vm.VarSym;
pub const TraceInfo = vm.TraceInfo;
pub const OpCount = vm.OpCount;
pub const HeapObject = vm.HeapObject;
pub const ListS = vm.ListS;
pub const ListIteratorT = vm.ListIteratorT;
pub const MapS = vm.MapS;
pub const MapIteratorT = vm.MapIteratorT;
pub const MapInner = vm.MapInner;
pub const ClosureS = vm.ClosureS;
pub const LambdaS = vm.LambdaS;
pub const AstringT = vm.AstringT;
pub const UstringT = vm.UstringT;
pub const RawStringT = vm.RawStringT;
pub const FiberS = vm.FiberS;
pub const BoxS = vm.BoxS;
pub const NativeFunc1S = vm.NativeFunc1S;
pub const TccStateS = vm.TccStateS;
pub const OpaquePtrS = vm.OpaquePtrS;
pub const FileT = vm.FileT;
pub const DirT = vm.DirT;
pub const DirIteratorT = vm.DirIteratorT;
pub const StructId = vm.StructId;
pub const EvalError = vm.EvalError;
pub const StackTrace = vm.StackTrace;
pub const StackFrame = vm.StackFrame;
pub const buildReturnInfo = vm.buildReturnInfo;
pub const framePtrOffset = vm.framePtrOffset;
pub const Dir = vm.Dir;
pub const DirIterator = vm.DirIterator;
pub const CyList = vm.List;
pub const Fiber = vm.Fiber;
pub const Closure = vm.Closure;
pub const OpaquePtr = vm.OpaquePtr;
pub const FuncSymDetail = vm.FuncSymDetail;
pub const RelFuncSigKey = vm.RelFuncSigKey;
pub const RelFuncSigKeyContext = vm.RelFuncSigKeyContext;
pub const AbsFuncSigKey = vm.AbsFuncSigKey;
pub const AbsFuncSigKeyContext = vm.AbsFuncSigKeyContext;
pub const Astring = vm.Astring;
pub const RawString = vm.RawString;
pub const MaxPoolObjectStringByteLen = vm.MaxPoolObjectAstringByteLen;
pub const MaxPoolObjectRawStringByteLen = vm.MaxPoolObjectRawStringByteLen;

const map = @import("map.zig");
pub const ValueMap = map.ValueMap;
pub const ValueMapEntry = map.ValueMapEntry;

const list = @import("list.zig");
pub const List = list.List;
pub const ListAligned = list.ListAligned;

pub const string = @import("string.zig");
pub const HeapStringBuilder = string.HeapStringBuilder;
pub const HeapRawStringBuilder = string.HeapRawStringBuilder;
pub const isAstring = string.isAstring;
pub const validateUtf8 = string.validateUtf8;
pub const ustringSeekCharIndexSliceAt = string.ustringSeekCharIndexSliceAt;
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
pub const OpDebug = bytecode.OpDebug;
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

pub const HotSection = if (builtin.os.tag == .macos) "DATA,.eval" else ".eval";
pub const Section = if (builtin.os.tag == .macos) "DATA,.eval2" else ".eval2";
pub const StdSection = if (builtin.os.tag == .macos) "DATA,.eval.std" else ".eval.std";
pub const CompilerSection = if (builtin.os.tag == .macos) "DATA,.compiler" else ".compiler";
pub const InitSection = if (builtin.os.tag == .macos) "DATA,.cyInit" else ".cyInit";

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

pub var silentError = false;

pub const simd = @import("simd.zig");