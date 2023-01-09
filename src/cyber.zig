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
pub const FiberS = vm.FiberS;
pub const BoxS = vm.BoxS;
pub const NativeFunc1S = vm.NativeFunc1S;
pub const TccStateS = vm.TccStateS;
pub const OpaquePtrS = vm.OpaquePtrS;
pub const FileT = vm.FileT;
pub const StructId = vm.StructId;
pub const EvalError = vm.EvalError;
pub const StackTrace = vm.StackTrace;
pub const StackFrame = vm.StackFrame;
pub const buildReturnInfo = vm.buildReturnInfo;
pub const framePtrOffset = vm.framePtrOffset;
pub const CyList = vm.List;
pub const Fiber = vm.Fiber;
pub const Closure = vm.Closure;
pub const OpaquePtr = vm.OpaquePtr;
pub const FuncSymDetail = vm.FuncSymDetail;
pub const RelFuncSigKey = vm.RelFuncSigKey;
pub const RelFuncSigKeyContext = vm.RelFuncSigKeyContext;
pub const AbsFuncSigKey = vm.AbsFuncSigKey;
pub const AbsFuncSigKeyContext = vm.AbsFuncSigKeyContext;
pub const isAstring = vm.isAstring;

const map = @import("map.zig");
pub const ValueMap = map.ValueMap;
pub const ValueMapEntry = map.ValueMapEntry;

const list = @import("list.zig");
pub const List = list.List;

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

pub var silentError = false;