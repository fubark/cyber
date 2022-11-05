const parser = @import("parser.zig");
pub const Parser = parser.Parser;
pub const ParseResultView = parser.ResultView;
pub const ParseResult = parser.Result;
pub const Node = parser.Node;
pub const NodeId = parser.NodeId;
pub const BinaryExprOp = parser.BinaryExprOp;
pub const FunctionDeclaration = parser.FunctionDeclaration;
pub const FunctionParam = parser.FunctionParam;
pub const Token = parser.Token;
pub const Tokenizer = parser.Tokenizer;
pub const TokenizeState = parser.TokenizeState;
pub const TokenType = parser.TokenType;

const vm_compiler = @import("vm_compiler.zig");
pub const VMcompiler = vm_compiler.VMcompiler;

const js_compiler = @import("js_compiler.zig");
pub const JsTargetCompiler = js_compiler.JsTargetCompiler;
pub const JsTargetResultView = js_compiler.ResultView;

const value = @import("value.zig");
pub const Value = value.Value;
pub const ValuePair = value.ValuePair;
pub const TagBoolean = value.TagBoolean;
pub const TagNone = value.TagNone;
pub const TagError = value.TagError;
pub const TagConstString = value.TagConstString;

const vm = @import("vm.zig");
pub const initVM = vm.initVM;
pub const deinitVM = vm.deinitVM;
pub const eval = vm.eval;
pub const setTrace = vm.setTrace;
pub const VM = vm.VM;
pub const FuncSymbolEntry = vm.FuncSymbolEntry;
pub const TraceInfo = vm.TraceInfo;
pub const OpCount = vm.OpCount;
pub const HeapObject = vm.HeapObject;
pub const ListS = vm.ListS;
pub const MapS = vm.MapS;
pub const StringS = vm.StringS;

const map = @import("map.zig");
pub const ValueMap = map.ValueMap;
pub const ValueMapEntry = map.ValueMapEntry;

const bytecode = @import("bytecode.zig");
pub const ByteCodeBuffer = bytecode.ByteCodeBuffer;
pub const OpCode = bytecode.OpCode;
pub const OpData = bytecode.OpData;
pub const Const = bytecode.Const;

// const js_rt = @import("js_rt.zig");
// pub const JsEngine = js_rt.JsEngine;
// pub const JsValue = js_rt.JsValue;
// pub const JsValueType = js_rt.JsValueType;
// pub const WebJsValue = js_rt.WebJsValue;
// pub const QJS = js_rt.QJS;

const cdata = @import("cdata.zig");
pub const encodeCDATA = cdata.encode;
pub const decodeCDATAmap = cdata.decodeMap;
pub const EncodeValueContext = cdata.EncodeValueContext;
pub const EncodeMapContext = cdata.EncodeMapContext;
pub const EncodeListContext = cdata.EncodeListContext;
pub const DecodeMapIR = cdata.DecodeMapIR;
pub const DecodeListIR = cdata.DecodeListIR;