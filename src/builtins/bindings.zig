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
const bt = cy.types.BuiltinTypeSymIds;
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const fmt = @import("../fmt.zig");

const debug = builtin.mode == .Debug;
const log = cy.log.scoped(.bindings);

const NullId = std.math.maxInt(u32);

pub const Symbol = enum {
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
    void,

    little,
    big,

    left,
    right,
    ends,

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

    running,
    paused,
    done,

    boolean,
    err,
    float,
    object,
    map,
    list,
    function,
    fiber,
    string,
    rawstring,
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

fn getBuiltinSymbol(id: u32) ?Symbol {
    return std.meta.intToEnum(Symbol, id) catch {
        return null;
    };
}

pub fn prepareThrowSymbol(vm: *cy.UserVM, sym: Symbol) Value {
    return vm.prepareThrowSymbol(@intFromEnum(sym));
}  

const StdSection = cy.StdSection;
const Section = cy.Section;

// TODO: Once traits is done, bind all string types using comptime.
// const NameFunc = struct { []const u8, cy.ZHostFuncFn };
// const funcs = b: {
//     var arr: std.BoundedArray(NameFunc, 100) = .{};
//     arr.appendSlice(&.{
//         .{ "sort", listSort},
//     }) catch cy.fatal();

//     const stringTypes = &[_]rt.TypeId{ rt.StaticAstringT, rt.StaticUstringT, rt.AstringT, rt.UstringT, rt.StringSliceT };
//     inline for (stringTypes) |stringT| {
//         const tag: cy.StringType = switch (stringT) {
//             rt.StaticAstringT => .staticAstring,
//             rt.StaticUstringT => .staticUstring,
//             rt.AstringT => .astring,
//             rt.UstringT => .ustring,
//             rt.StringSliceT => .slice,
//             else => unreachable,
//         };
//         arr.appendSlice(&.{
//             .{ "append", stringAppend(tag) },
//         }) catch cy.fatal();
//     }

//     if (arr.len != arr.capacity()) {
//         @compileError(std.fmt.comptimePrint("Len ({}) != Cap ({})", .{arr.len, arr.capacity()}));
//     }
//     break :b arr.slice();
// };

pub fn bindCore(self: *cy.VM) linksection(cy.InitSection) !void {
    @setCold(true);

    const b = ModuleBuilder.init(self.compiler, undefined);

    // Builtin methods.
    self.indexMGID = try b.ensureMethodGroup("$index");
    self.setIndexMGID = try b.ensureMethodGroup("$setIndex");
    self.sliceMGID = try b.ensureMethodGroup("$slice");
    self.@"infix<MGID" = try b.ensureMethodGroup("$infix<");
    self.@"infix<=MGID" = try b.ensureMethodGroup("$infix<=");
    self.@"infix>MGID" = try b.ensureMethodGroup("$infix>");
    self.@"infix>=MGID" = try b.ensureMethodGroup("$infix>=");
    self.@"infix+MGID" = try b.ensureMethodGroup("$infix+");
    self.@"infix-MGID" = try b.ensureMethodGroup("$infix-");
    self.@"infix*MGID" = try b.ensureMethodGroup("$infix*");
    self.@"infix/MGID" = try b.ensureMethodGroup("$infix/");
    self.@"infix%MGID" = try b.ensureMethodGroup("$infix%");
    self.@"infix^MGID" = try b.ensureMethodGroup("$infix^");
    self.@"infix&MGID" = try b.ensureMethodGroup("$infix&");
    self.@"infix|MGID" = try b.ensureMethodGroup("$infix|");
    self.@"infix||MGID" = try b.ensureMethodGroup("$infix||");
    self.@"infix<<MGID" = try b.ensureMethodGroup("$infix<<");
    self.@"infix>>MGID" = try b.ensureMethodGroup("$infix>>");
    self.@"prefix~MGID" = try b.ensureMethodGroup("$prefix~");
    self.@"prefix-MGID" = try b.ensureMethodGroup("$prefix-");
    const append = try b.ensureMethodGroup("append");
    const byteAt = try b.ensureMethodGroup("byteAt");
    const charAt = try b.ensureMethodGroup("charAt");
    const close = try b.ensureMethodGroup("close");
    const codeAt = try b.ensureMethodGroup("codeAt");
    const concat = try b.ensureMethodGroup("concat");
    const endsWith = try b.ensureMethodGroup("endsWith");
    const find = try b.ensureMethodGroup("find");
    const findAnyRune = try b.ensureMethodGroup("findAnyRune");
    const findRune = try b.ensureMethodGroup("findRune");
    const idSym = try b.ensureMethodGroup("id");
    const index = try b.ensureMethodGroup("index");
    const indexChar = try b.ensureMethodGroup("indexChar");
    const indexCharSet = try b.ensureMethodGroup("indexCharSet");
    const indexCode = try b.ensureMethodGroup("indexCode");
    const insert = try b.ensureMethodGroup("insert");
    const insertByte = try b.ensureMethodGroup("insertByte");
    const isAscii = try b.ensureMethodGroup("isAscii");
    self.iteratorMGID = try b.ensureMethodGroup("iterator");
    const len = try b.ensureMethodGroup("len");
    const less = try b.ensureMethodGroup("less");
    const lower = try b.ensureMethodGroup("lower");
    self.nextMGID = try b.ensureMethodGroup("next");
    self.nextPairMGID = try b.ensureMethodGroup("nextPair");
    self.pairIteratorMGID = try b.ensureMethodGroup("pairIterator");
    const read = try b.ensureMethodGroup("read");
    const readToEnd = try b.ensureMethodGroup("readToEnd");
    const remove = try b.ensureMethodGroup("remove");
    const repeat = try b.ensureMethodGroup("repeat");
    const replace = try b.ensureMethodGroup("replace");
    const runeAt = try b.ensureMethodGroup("runeAt");
    const seek = try b.ensureMethodGroup("seek");
    const seekFromCur = try b.ensureMethodGroup("seekFromCur");
    const seekFromEnd = try b.ensureMethodGroup("seekFromEnd");
    const size = try b.ensureMethodGroup("size");
    const slice = try b.ensureMethodGroup("slice");
    const sliceAt = try b.ensureMethodGroup("sliceAt");
    const split = try b.ensureMethodGroup("split");
    const startsWith = try b.ensureMethodGroup("startsWith");
    const stat = try b.ensureMethodGroup("stat");
    const status = try b.ensureMethodGroup("status");
    const streamLines = try b.ensureMethodGroup("streamLines");
    const toString = try b.ensureMethodGroup("toString");
    const trim = try b.ensureMethodGroup("trim");
    const upper = try b.ensureMethodGroup("upper");
    const utf8 = try b.ensureMethodGroup("utf8");
    const value = try b.ensureMethodGroup("value");
    const walk = try b.ensureMethodGroup("walk");
    const write = try b.ensureMethodGroup("write");
    
    // Init compile time builtins.

    // Builtin types.
    var id = try self.addBuiltinType("none", bt.None);
    std.debug.assert(id == rt.NoneT);

    id = try self.addBuiltinType("boolean", bt.Boolean);
    std.debug.assert(id == rt.BooleanT);
    var rsym = self.compiler.sema.getSymbol(bt.Boolean);
    var sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.Boolean, booleanCall);

    id = try self.addBuiltinType("error", bt.Error);
    std.debug.assert(id == rt.ErrorT);
    rsym = self.compiler.sema.getSymbol(bt.Error);
    sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.Error, errorCall);
    try b.addMethod(rt.ErrorT, value, &.{ bt.Any }, bt.Any, errorValue);

    id = try self.addBuiltinType("StaticAstring", bt.String);
    std.debug.assert(id == rt.StaticAstringT);

    // string type module.
    rsym = self.compiler.sema.getSymbol(bt.String);
    sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.String, stringCall);

    id = try self.addBuiltinType("StaticUstring", bt.String);
    std.debug.assert(id == rt.StaticUstringT);

    id = try self.addBuiltinType("enum", cy.NullId);
    std.debug.assert(id == rt.EnumT);

    id = try self.addBuiltinType("symbol", bt.Symbol);
    std.debug.assert(id == rt.SymbolT);

    id = try self.addBuiltinType("integer", bt.Integer);
    std.debug.assert(id == rt.IntegerT);
    rsym = self.compiler.sema.getSymbol(bt.Integer);
    sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.Integer, integerCall);
    try sb.addOptimizingMethod(rt.IntegerT, self.@"prefix~MGID", &.{ bt.Any }, bt.Integer, intBitwiseNot);
    try sb.addOptimizingMethod(rt.IntegerT, self.@"prefix-MGID", &.{ bt.Any }, bt.Integer, intNeg);
    // Inlined opcodes allow the right arg to be dynamic so the compiler can gen more of those.
    // So for now, the runtime signature reflects that.
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix<MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.lessInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix<=MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.lessEqualInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix>MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.greaterInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix>=MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.greaterEqualInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix+MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.addInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix+MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.addInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix+MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.addInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix-MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.subInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix*MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.mulInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix/MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.divInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix%MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.modInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix^MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.powInt));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix&MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.bitwiseAnd));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix|MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.bitwiseOr));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix||MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.bitwiseXor));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix<<MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.bitwiseLeftShift));
    try sb.addOptimizingMethod(rt.IntegerT, self.@"infix>>MGID", &.{ bt.Any, bt.Any }, bt.Integer, inlineBinOp(.bitwiseRightShift));

    id = try self.addBuiltinType("float", bt.Float);
    std.debug.assert(id == rt.FloatT);
    rsym = self.compiler.sema.getSymbol(bt.Float);
    sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.Float, floatCall);
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix<MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.lessFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix<=MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.lessEqualFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix>MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.greaterFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix<=MGID", &.{ bt.Any, bt.Any }, bt.Boolean, inlineBinOp(.greaterEqualFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"prefix-MGID", &.{ bt.Any }, bt.Float, floatNeg);
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix+MGID", &.{ bt.Any, bt.Any }, bt.Float, inlineBinOp(.addFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix-MGID", &.{ bt.Any, bt.Any }, bt.Float, inlineBinOp(.subFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix*MGID", &.{ bt.Any, bt.Any }, bt.Float, inlineBinOp(.mulFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix/MGID", &.{ bt.Any, bt.Any }, bt.Float, inlineBinOp(.divFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix%MGID", &.{ bt.Any, bt.Any }, bt.Float, inlineBinOp(.modFloat));
    try sb.addOptimizingMethod(rt.FloatT, self.@"infix^MGID", &.{ bt.Any, bt.Any }, bt.Float, inlineBinOp(.powFloat));

    id = try self.addBuiltinType("List", bt.List);
    std.debug.assert(id == rt.ListT);

    id = try self.addBuiltinType("ListIterator", cy.NullId);
    std.debug.assert(id == rt.ListIteratorT);
    try b.addMethod(rt.ListIteratorT, self.nextMGID, &.{bt.Any}, bt.Any, listIteratorNext);
    try b.addMethod2(rt.ListIteratorT, self.nextPairMGID, &.{bt.Any}, bt.Any, listIteratorNextPair);

    id = try self.addBuiltinType("Map", bt.Map);
    std.debug.assert(id == rt.MapT);
    try b.addOptimizingMethod(rt.MapT, self.indexMGID, &.{ bt.Any, bt.Any }, bt.Any, inlineBinOp(.indexMap));
    try b.addOptimizingMethod(rt.MapT, self.setIndexMGID, &.{ bt.Any, bt.Any, bt.Any }, bt.None, inlineTernNoRetOp(.setIndexMap));
    try b.addMethod(rt.MapT, remove,                  &.{ bt.Any, bt.Any }, bt.None, mapRemove);
    try b.addMethod(rt.MapT, size,                    &.{ bt.Any }, bt.Integer, mapSize);
    try b.addMethod(rt.MapT, self.iteratorMGID,     &.{ bt.Any }, bt.Any, mapIterator);
    try b.addMethod(rt.MapT, self.pairIteratorMGID, &.{ bt.Any }, bt.Any, mapIterator);

    id = try self.addBuiltinType("MapIterator", cy.NullId);
    std.debug.assert(id == rt.MapIteratorT);
    try b.addMethod(rt.MapIteratorT, self.nextMGID,     &.{bt.Any}, bt.Any, mapIteratorNext);
    try b.addMethod2(rt.MapIteratorT, self.nextPairMGID, &.{bt.Any}, bt.Any, mapIteratorNextPair);

    id = try self.addBuiltinType("Closure", cy.NullId);
    std.debug.assert(id == rt.ClosureT);

    id = try self.addBuiltinType("Lambda", cy.NullId);
    std.debug.assert(id == rt.LambdaT);

    id = try self.addBuiltinType("Astring", bt.String);
    std.debug.assert(id == rt.AstringT);

    id = try self.addBuiltinType("Ustring", bt.String);
    std.debug.assert(id == rt.UstringT);

    id = try self.addBuiltinType("StringSlice", bt.String);
    std.debug.assert(id == rt.StringSliceT);

    const StringTypes = &[_]rt.TypeId{ rt.StaticAstringT, rt.StaticUstringT, rt.AstringT, rt.UstringT, rt.StringSliceT };
    inline for (StringTypes) |typeId| {
        const tag: cy.StringType = switch (typeId) {
            rt.StaticAstringT => .staticAstring,
            rt.StaticUstringT => .staticUstring,
            rt.AstringT => .astring,
            rt.UstringT => .ustring,
            rt.StringSliceT => .slice,
            else => unreachable,
        };
        try b.addMethod(typeId, append,       &.{ bt.Any, bt.Any }, bt.String, stringAppend(tag));
        try b.addMethod(typeId, charAt,       &.{ bt.Any, bt.Integer }, bt.Any, stringCharAt(tag));
        try b.addMethod(typeId, codeAt,       &.{ bt.Any, bt.Integer }, bt.Any, stringCodeAt(tag));
        try b.addMethod(typeId, concat,       &.{ bt.Any, bt.Any }, bt.String, stringConcat(tag));
        try b.addMethod(typeId, endsWith,     &.{ bt.Any, bt.Any }, bt.Boolean, stringEndsWith(tag));
        try b.addMethod(typeId, find,         &.{ bt.Any, bt.Any }, bt.Any, stringFind(tag));
        try b.addMethod(typeId, findAnyRune,  &.{ bt.Any, bt.Any }, bt.Any, stringFindAnyRune(tag));
        try b.addMethod(typeId, findRune,     &.{ bt.Any, bt.Integer }, bt.Any, stringFindRune(tag));
        try b.addMethod(typeId, index,        &.{ bt.Any, bt.Any }, bt.Any, stringIndex(tag));
        try b.addMethod(typeId, indexChar,    &.{ bt.Any, bt.Any }, bt.Any, stringIndexChar(tag));
        try b.addMethod(typeId, indexCharSet, &.{ bt.Any, bt.Any }, bt.Any, stringIndexCharSet(tag));
        try b.addMethod(typeId, indexCode,    &.{ bt.Any, bt.Integer }, bt.Any, stringIndexCode(tag));
        try b.addMethod(typeId, insert,       &.{ bt.Any, bt.Integer, bt.Any }, bt.String, stringInsert(tag));
        try b.addMethod(typeId, isAscii,      &.{ bt.Any }, bt.Boolean, stringIsAscii(tag));
        try b.addMethod(typeId, len,          &.{ bt.Any }, bt.Integer, stringLen(tag));
        try b.addMethod(typeId, less,         &.{ bt.Any, bt.Any }, bt.Boolean, stringLess(tag));
        try b.addMethod(typeId, lower,        &.{ bt.Any }, bt.String, stringLower(tag));
        try b.addMethod(typeId, replace,      &.{ bt.Any, bt.Any, bt.Any }, bt.String, stringReplace(tag));
        try b.addMethod(typeId, repeat,       &.{ bt.Any, bt.Integer }, bt.Any, stringRepeat(tag));
        try b.addMethod(typeId, runeAt,       &.{ bt.Any, bt.Integer }, bt.Any, stringRuneAt(tag));
        try b.addMethod(typeId, slice,        &.{ bt.Any, bt.Integer, bt.Integer }, bt.Any, stringSlice(tag));
        try b.addMethod(typeId, self.sliceMGID, &.{ bt.Any, bt.Integer, bt.Any }, bt.Any, stringSlice(tag));
        try b.addMethod(typeId, sliceAt,      &.{ bt.Any, bt.Integer }, bt.Any, stringSliceAt(tag));
        try b.addMethod(typeId, self.indexMGID, &.{ bt.Any, bt.Integer }, bt.Any, stringSliceAt(tag));
        try b.addMethod(typeId, split,        &.{ bt.Any, bt.Any }, bt.List, stringSplit(tag));
        try b.addMethod(typeId, startsWith,   &.{ bt.Any, bt.Any }, bt.Boolean, stringStartsWith(tag));
        try b.addMethod(typeId, trim,         &.{ bt.Any, bt.Symbol, bt.Any }, bt.Any, stringTrim(tag));
        try b.addMethod(typeId, upper,        &.{ bt.Any }, bt.String, stringUpper(tag));
    }

    id = try self.addBuiltinType("Rawstring", bt.Rawstring);
    std.debug.assert(id == rt.RawstringT);

    // rawstring type module.
    rsym = self.compiler.sema.getSymbol(bt.Rawstring);
    sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.Rawstring, rawstringCall);

    id = try self.addBuiltinType("RawstringSlice", bt.Rawstring);
    std.debug.assert(id == rt.RawstringSliceT);

    const RawstringTypes = &[_]rt.TypeId{ rt.RawstringT, rt.RawstringSliceT };
    inline for (RawstringTypes) |typeId| {
        const tag = switch (typeId) {
            rt.RawstringT => .rawstring,
            rt.RawstringSliceT => .rawSlice,
            else => unreachable,
        };
        try b.addMethod(typeId, append,       &.{ bt.Any, bt.Any }, bt.Rawstring, stringAppend(tag));
        try b.addMethod(typeId, byteAt,       &.{ bt.Any, bt.Integer }, bt.Integer,
            if (tag == .rawstring) rawStringByteAt else rawStringSliceByteAt);
        try b.addMethod(typeId, charAt,       &.{ bt.Any, bt.Integer }, bt.Any, stringCharAt(tag));
        try b.addMethod(typeId, codeAt,       &.{ bt.Any, bt.Integer }, bt.Any, stringCodeAt(tag));
        try b.addMethod(typeId, concat,       &.{ bt.Any, bt.Any }, bt.Rawstring, stringConcat(tag));
        try b.addMethod(typeId, endsWith,     &.{ bt.Any, bt.Any }, bt.Boolean, stringEndsWith(tag));
        try b.addMethod(typeId, find,         &.{ bt.Any, bt.Any }, bt.Any, stringFind(tag));
        try b.addMethod(typeId, findAnyRune,  &.{ bt.Any, bt.Any }, bt.Any, stringFindAnyRune(tag));
        try b.addMethod(typeId, findRune,     &.{ bt.Any, bt.Integer }, bt.Any, stringFindRune(tag));
        try b.addMethod(typeId, index,        &.{ bt.Any, bt.Any }, bt.Any, stringIndex(tag));
        try b.addMethod(typeId, indexChar,    &.{ bt.Any, bt.Any }, bt.Any, stringIndexChar(tag));
        try b.addMethod(typeId, indexCharSet, &.{ bt.Any, bt.Any }, bt.Any, stringIndexCharSet(tag));
        try b.addMethod(typeId, indexCode,    &.{ bt.Any, bt.Integer }, bt.Any, stringIndexCode(tag));
        try b.addMethod(typeId, insert,       &.{ bt.Any, bt.Integer, bt.Any }, bt.Any, stringInsert(tag));
        try b.addMethod(typeId, insertByte,   &.{ bt.Any, bt.Integer, bt.Integer }, bt.Any,
            if (tag == .rawstring) rawStringInsertByte else rawStringSliceInsertByte);
        try b.addMethod(typeId, isAscii,      &.{ bt.Any }, bt.Boolean, stringIsAscii(tag));
        try b.addMethod(typeId, len,          &.{ bt.Any }, bt.Integer, stringLen(tag));
        try b.addMethod(typeId, less,         &.{ bt.Any, bt.Any }, bt.Boolean, stringLess(tag));
        try b.addMethod(typeId, lower,        &.{ bt.Any }, bt.Rawstring, stringLower(tag));
        try b.addMethod(typeId, repeat,       &.{ bt.Any, bt.Integer }, bt.Any, stringRepeat(tag));
        try b.addMethod(typeId, replace,      &.{ bt.Any, bt.Any, bt.Any }, bt.Rawstring, stringReplace(tag));
        try b.addMethod(typeId, runeAt,       &.{ bt.Any, bt.Integer }, bt.Any, stringRuneAt(tag));
        try b.addMethod(typeId, slice,        &.{ bt.Any, bt.Integer, bt.Integer }, bt.Any, stringSlice(tag));
        try b.addMethod(typeId, self.sliceMGID, &.{ bt.Any, bt.Integer, bt.Any }, bt.Any, stringSlice(tag));
        try b.addMethod(typeId, sliceAt,      &.{ bt.Any, bt.Integer }, bt.Any, stringSliceAt(tag));
        try b.addMethod(typeId, self.indexMGID, &.{ bt.Any, bt.Integer }, bt.Any, stringSliceAt(tag));
        try b.addMethod(typeId, split,        &.{ bt.Any, bt.Any }, bt.List, stringSplit(tag));
        try b.addMethod(typeId, startsWith,   &.{ bt.Any, bt.Any }, bt.Boolean, stringStartsWith(tag));
        try b.addMethod(typeId, toString,     &.{ bt.Any }, bt.Any,
            if (tag == .rawstring) rawStringToString else rawStringSliceToString);
        try b.addMethod(typeId, trim,         &.{ bt.Any, bt.Symbol, bt.Any }, bt.Any, stringTrim(tag));
        try b.addMethod(typeId, upper,        &.{ bt.Any }, bt.Rawstring, stringUpper(tag));
        try b.addMethod(typeId, utf8,         &.{ bt.Any }, bt.Any,
            if (tag == .rawstring) rawStringUtf8 else rawStringSliceUtf8);
    }

    id = try self.addBuiltinType("Fiber", bt.Fiber);
    std.debug.assert(id == rt.FiberT);
    try b.addMethod(rt.FiberT, status, &.{ bt.Any }, bt.Symbol, fiberStatus);

    id = try self.addBuiltinType("Box", cy.NullId);
    std.debug.assert(id == rt.BoxT);

    id = try self.addBuiltinType("NativeFunc1", cy.NullId);
    std.debug.assert(id == rt.NativeFuncT);

    id = try self.addBuiltinType("TccState", cy.NullId);
    std.debug.assert(id == rt.TccStateT);

    id = try self.addBuiltinType("pointer", bt.Pointer);
    std.debug.assert(id == rt.PointerT);
    rsym = self.compiler.sema.getSymbol(bt.Pointer);
    sb = ModuleBuilder.init(self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("$call", &.{ bt.Any }, bt.Pointer, pointerCall);
    try b.addMethod(rt.PointerT, value, &.{ bt.Any }, bt.Integer, pointerValue);

    id = try self.addBuiltinType("File", bt.File);
    std.debug.assert(id == rt.FileT);
    if (cy.hasStdFiles) {
        try b.addMethod(rt.FileT, close,               &.{ bt.Any }, bt.None, fileClose);
        try b.addMethod(rt.FileT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, fileIterator);
        try b.addMethod(rt.FileT, self.nextMGID,       &.{ bt.Any }, bt.Any, fileNext);
        try b.addMethod(rt.FileT, read,                &.{ bt.Any, bt.Integer }, bt.Any, fileRead);
        try b.addMethod(rt.FileT, readToEnd,           &.{ bt.Any }, bt.Any, fileReadToEnd);
        try b.addMethod(rt.FileT, seek,                &.{ bt.Any, bt.Integer }, bt.Any, fileSeek);
        try b.addMethod(rt.FileT, seekFromCur,         &.{ bt.Any, bt.Integer }, bt.Any, fileSeekFromCur);
        try b.addMethod(rt.FileT, seekFromEnd,         &.{ bt.Any, bt.Integer }, bt.Any, fileSeekFromEnd);
        try b.addMethod(rt.FileT, stat,                &.{ bt.Any }, bt.Any, fileOrDirStat);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any }, bt.Any, fileStreamLines);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any, bt.Float }, bt.Any, fileStreamLines1);
        try b.addMethod(rt.FileT, write,               &.{ bt.Any, bt.Any }, bt.Any, fileWrite);
    } else {
        try b.addMethod(rt.FileT, close,               &.{ bt.Any }, bt.None, nop);
        try b.addMethod(rt.FileT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.FileT, self.nextMGID,       &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.FileT, read,                &.{ bt.Any, bt.Integer }, bt.Any, nop);
        try b.addMethod(rt.FileT, readToEnd,           &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.FileT, seek,                &.{ bt.Any, bt.Integer }, bt.Any, nop);
        try b.addMethod(rt.FileT, seekFromCur,         &.{ bt.Any, bt.Integer }, bt.Any, nop);
        try b.addMethod(rt.FileT, seekFromEnd,         &.{ bt.Any, bt.Integer }, bt.Any, nop);
        try b.addMethod(rt.FileT, stat,                &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any, bt.Float }, bt.Any, nop);
        try b.addMethod(rt.FileT, write,               &.{ bt.Any, bt.Any }, bt.Any, nop);
    }

    id = try self.addBuiltinType("Dir", cy.NullId);
    std.debug.assert(id == rt.DirT);
    if (cy.hasStdFiles) {
        try b.addMethod(rt.DirT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, dirIterator);
        try b.addMethod(rt.DirT, stat,                &.{ bt.Any }, bt.Any, fileOrDirStat);
        try b.addMethod(rt.DirT, walk,                &.{ bt.Any }, bt.Any, dirWalk);
    } else {
        try b.addMethod(rt.DirT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.DirT, stat,                &.{ bt.Any }, bt.Any, nop);
        try b.addMethod(rt.DirT, walk,                &.{ bt.Any }, bt.Any, nop);
    }

    id = try self.addBuiltinType("DirIterator", cy.NullId);
    std.debug.assert(id == rt.DirIteratorT);
    if (cy.hasStdFiles) {
        try b.addMethod(rt.DirIteratorT, self.nextMGID, &.{ bt.Any }, bt.Any, dirIteratorNext);
    } else {
        try b.addMethod(rt.DirIteratorT, self.nextMGID, &.{ bt.Any }, bt.Any, nop);
    }

    id = try self.addBuiltinType("MetaType", bt.MetaType);
    std.debug.assert(id == rt.MetaTypeT);
    try b.addMethod(rt.MetaTypeT, idSym, &.{ bt.Any }, bt.Integer, metatypeId);

    id = try self.addBuiltinType("any", bt.Any);
    std.debug.assert(id == rt.AnyT);

    id = try self.addBuiltinType("string", bt.String);
    std.debug.assert(id == rt.StringUnionT);

    id = try self.addBuiltinType("rawstring", bt.Rawstring);
    std.debug.assert(id == rt.RawstringUnionT);

    try ensureSymbol(self, "bool", .bool);
    try ensureSymbol(self, "char", .char);
    try ensureSymbol(self, "uchar", .uchar);
    try ensureSymbol(self, "short", .short);
    try ensureSymbol(self, "ushort", .ushort);
    try ensureSymbol(self, "int", .int);
    try ensureSymbol(self, "uint", .uint);
    try ensureSymbol(self, "long", .long);
    try ensureSymbol(self, "ulong", .ulong);
    try ensureSymbol(self, "usize", .usize);
    try ensureSymbol(self, "double", .double);
    try ensureSymbol(self, "charPtr", .charPtr);
    try ensureSymbol(self, "voidPtr", .voidPtr);
    try ensureSymbol(self, "void", .void);

    try ensureSymbol(self, "little", .little);
    try ensureSymbol(self, "big", .big);

    try ensureSymbol(self, "left", .left);
    try ensureSymbol(self, "right", .right);
    try ensureSymbol(self, "ends", .ends);

    try ensureSymbol(self, "AssertError", .AssertError);
    try ensureSymbol(self, "FileNotFound", .FileNotFound);
    try ensureSymbol(self, "MissingSymbol", .MissingSymbol);
    try ensureSymbol(self, "EndOfStream", .EndOfStream);
    try ensureSymbol(self, "OutOfBounds", .OutOfBounds);
    try ensureSymbol(self, "InvalidArgument", .InvalidArgument);
    try ensureSymbol(self, "InvalidSignature", .InvalidSignature);
    try ensureSymbol(self, "InvalidRune", .InvalidRune);
    try ensureSymbol(self, "SteamTooLong", .StreamTooLong);
    try ensureSymbol(self, "NotAllowed", .NotAllowed);
    try ensureSymbol(self, "Closed", .Closed);
    try ensureSymbol(self, "PermissionDenied", .PermissionDenied);
    try ensureSymbol(self, "UnknownError", .UnknownError);

    try ensureSymbol(self, "running", .running);
    try ensureSymbol(self, "paused", .paused);
    try ensureSymbol(self, "done", .done);

    try ensureSymbol(self, "boolean", .boolean);
    try ensureSymbol(self, "error", .err);
    try ensureSymbol(self, "float", .float);
    try ensureSymbol(self, "object", .object);
    try ensureSymbol(self, "map", .map);
    try ensureSymbol(self, "list", .list);
    try ensureSymbol(self, "function", .function);
    try ensureSymbol(self, "fiber", .fiber);
    try ensureSymbol(self, "string", .string);
    try ensureSymbol(self, "rawstring", .rawstring);
    try ensureSymbol(self, "none", .none);
    try ensureSymbol(self, "symbol", .symbol);
    try ensureSymbol(self, "pointer", .pointer);
    try ensureSymbol(self, "metatype", .metatype);

    try ensureSymbol(self, "read", .read);
    try ensureSymbol(self, "write", .write);
    try ensureSymbol(self, "readWrite", .readWrite);

    try ensureSymbol(self, "file", .file);
    try ensureSymbol(self, "dir", .dir);

    try ensureSymbol(self, "unknown", .unknown);
}

fn ensureSymbol(vm: *cy.VM, name: []const u8, sym: Symbol) !void {
    const id = try vm.ensureSymbol(name);
    std.debug.assert(id == @intFromEnum(sym));
}

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
            const res = ctx_.vm.callFunc(ctx_.newFramePtr, ctx_.lessFn, &.{a, b}) catch |err| {
                _ = fromUnsupportedError(ctx_.vm, "less", err, @errorReturnTrace());
                return false;
            };
            return res.toBool();
        }
    };
    std.sort.pdq(Value, list.items(), &lessCtx, S.less);
    return Value.None;
}

pub fn listRemove(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index >= inner.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    vm.release(inner.buf[@intCast(index)]);
    inner.remove(@intCast(index));
    return Value.None;
}

pub fn listInsert(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index: i64 = @intCast(args[1].asInteger());
    const value = args[2];
    const list = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch cy.fatal();
    inner.insertAssumeCapacity(@intCast(index), value);
    return Value.None;
}

pub fn listAdd(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.Section) Value {
    fmt.printDeprecated("list.add()", "0.1", "Use list.append() instead.", &.{});
    return listAppend(vm, args, nargs);
}

pub fn listAppend(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    vm.retain(args[1]);
    obj.list.append(vm.allocator(), args[1]);
    return Value.None;
}

pub fn listJoinString(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const items = obj.list.items();
    if (items.len > 0) {
        var sepCharLen: u32 = undefined;
        const sep = vm.valueToTempString2(args[1], &sepCharLen);

        const alloc = vm.allocator();
        const tempSlices = &@as(*cy.VM, @ptrCast(vm)).u8Buf2;
        tempSlices.clearRetainingCapacity();
        const tempBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
        tempBuf.clearRetainingCapacity();
        const tempBufWriter = tempBuf.writer(alloc);
        defer {
            tempSlices.ensureMaxCapOrClear(alloc, 4096) catch fatal();
            tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();
        }

        var charLenSum: u32 = 0;
        var byteLen: u32 = 0;
        var charLen: u32 = undefined;

        // Record first string part.
        var str = vm.getOrWriteValueString(tempBufWriter, items[0], &charLen);
        tempSlices.appendSlice(alloc, std.mem.asBytes(&str)) catch fatal();
        charLenSum += charLen;
        byteLen += @intCast(str.len);

        // Record other string parts.
        for (items[1..]) |item| {
            str = vm.getOrWriteValueString(tempBufWriter, item, &charLen);
            tempSlices.appendSlice(alloc, std.mem.asBytes(&str)) catch fatal();
            charLenSum += charLen;
            byteLen += @intCast(str.len);
        }
        charLenSum += @intCast(sepCharLen * (items.len-1));
        byteLen += @intCast(sep.len * (items.len-1));

        // Allocate final buffer and perform join.
        var newObj: *cy.HeapObject = undefined;
        var buf: []u8 = undefined;
        if (charLenSum == byteLen) {
            newObj = vm.allocUnsetAstringObject(byteLen) catch fatal();
            buf = newObj.astring.getSlice();
        } else {
            newObj = vm.allocUnsetUstringObject(byteLen, charLenSum) catch fatal();
            buf = newObj.ustring.getSlice();
        }
        const slices = @as([*][]const u8, @ptrCast(tempSlices.buf.ptr))[0..items.len];
        std.mem.copy(u8, buf[0..slices[0].len], slices[0]);
        var dst: usize = slices[0].len;
        for (slices[1..]) |slice| {
            std.mem.copy(u8, buf[dst..dst+sep.len], sep);
            dst += sep.len;
            std.mem.copy(u8, buf[dst..dst+slice.len], slice);
            dst += slice.len;
        }
        return Value.initPtr(newObj);
    } else {
        // Empty string.
        return Value.initStaticAstring(0, 0);
    }
}

pub fn listConcat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const list = args[1].asHeapObject();
    for (list.list.items()) |it| {
        vm.retain(it);
        obj.list.append(vm.allocator(), it);
    }
    return Value.None;
}

fn listIteratorNextPair(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) cy.ValuePair {
    const obj = args[0].asHeapObject();
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return .{
            .left = Value.initInt(@intCast(obj.listIter.nextIdx)),
            .right = val,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn listIteratorNext(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return val;
    } else return Value.None;
}

pub fn listIterator(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    vm.retainObject(obj);
    return vm.allocListIterator(&obj.list) catch fatal();
}

pub fn listResize(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const recv = args[0];
    const list = recv.asHeapObject();
    const inner = cy.ptrAlignCast(*cy.List(Value), &list.list.list);
    const size: u32 = @intCast(args[1].asInteger());
    if (inner.len < size) {
        const oldLen = inner.len;
        inner.resize(vm.allocator(), size) catch cy.fatal();
        for (inner.items()[oldLen..size]) |*item| {
            item.* = Value.None;
        }
    } else if (inner.len > size) {
        // Remove items.
        for (inner.items()[size..inner.len]) |item| {
            vm.release(item);
        }
        inner.resize(vm.allocator(), size) catch cy.fatal();
    }
    return Value.None;
}

fn mapIterator(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(Section) Value {
    const obj = args[0].asHeapObject();
    vm.retainObject(obj);
    return vm.allocMapIterator(&obj.map) catch fatal();
}

fn mapIteratorNextPair(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(Section) cy.ValuePair {
    const obj = args[0].asHeapObject();
    const map: *cy.ValueMap = @ptrCast(&obj.mapIter.map.inner);
    if (map.next(&obj.mapIter.nextIdx)) |entry| {
        vm.retain(entry.key);
        vm.retain(entry.value);
        return .{
            .left = entry.key,
            .right = entry.value,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn mapIteratorNext(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const map: *cy.ValueMap = @ptrCast(&obj.mapIter.map.inner);
    if (map.next(&obj.mapIter.nextIdx)) |entry| {
        vm.retain(entry.value);
        return entry.value;
    } else return Value.None;
}

fn mapSize(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    return Value.initInt(@intCast(inner.size));
}

fn mapRemove(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const inner = cy.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(@ptrCast(vm), args[1]);
    return Value.None;
}

pub fn listLen(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
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

fn errorValue(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const recv = args[0];
    const enumId = (recv.val & 0xFF00) >> 8;
    if (enumId == cy.NullU8) {
        return Value.initSymbol(recv.asErrorSymbol());
    } else {
        log.debug("TODO: error.value() for enums.", .{});
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
}

fn pointerValue(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(@bitCast(@as(u48, (@intCast(@intFromPtr(obj.pointer.ptr))))));
}

fn fiberStatus(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const fiber = args[0].asPointer(*vmc.Fiber);

    if (vm.internal().curFiber == fiber) {
        return Value.initSymbol(@intFromEnum(Symbol.running));
    } else {
        // Check if done.
        if (fiber.pcOffset == NullId) {
            return Value.initSymbol(@intFromEnum(Symbol.done));
        } else {
            return Value.initSymbol(@intFromEnum(Symbol.paused));
        }
    }
}

pub fn stringUpper(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            if (isAstringObject(T, obj)) {
                const new = vm.allocUnsetAstringObject(str.len) catch fatal();
                const newBuf = new.astring.getSlice();
                _ = std.ascii.upperString(newBuf, str);
                return vm.allocOwnedAstring(new) catch fatal();
            } else if (isUstringObject(T, obj)) {
                const new = vm.allocUnsetUstringObject(str.len, getStringCharLen(T, vm, obj)) catch fatal();
                const newBuf = new.ustring.getSlice();
                _ = std.ascii.upperString(newBuf, str);
                return vm.allocOwnedUstring(new) catch fatal();
            } else if (isRawStringObject(T)) {
                const new = vm.allocUnsetRawStringObject(str.len) catch fatal();
                const newBuf = new.rawstring.getSlice();
                _ = std.ascii.upperString(newBuf, str);
                return Value.initPtr(new);
            } else fatal();
        }
    };
    return S.inner;
}

pub fn stringLower(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            if (isAstringObject(T, obj)) {
                const new = vm.allocUnsetAstringObject(str.len) catch fatal();
                const newBuf = new.astring.getSlice();
                _ = std.ascii.lowerString(newBuf, str);
                return vm.allocOwnedAstring(new) catch fatal();
            } else if (isUstringObject(T, obj)) {
                const new = vm.allocUnsetUstringObject(str.len, getStringCharLen(T, vm, obj)) catch fatal();
                const newBuf = new.ustring.getSlice();
                _ = std.ascii.lowerString(newBuf, str);
                return vm.allocOwnedUstring(new) catch fatal();
            } else if (isRawStringObject(T)) {
                const new = vm.allocUnsetRawStringObject(str.len) catch fatal();
                const newBuf = new.rawstring.getSlice();
                _ = std.ascii.lowerString(newBuf, str);
                return Value.initPtr(new);
            } else fatal();
        }
    };
    return S.inner;
}

pub fn stringLess(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            if (isRawStringObject(T)) {
                var right: []const u8 = undefined;
                if (args[1].isRawString()) {
                    right = args[1].asRawString();
                } else {
                    right = vm.valueToTempString(args[1]);
                }
                return Value.initBool(std.mem.lessThan(u8, str, right));
            } else {
                const right = vm.valueToTempString(args[1]);
                return Value.initBool(std.mem.lessThan(u8, str, right));
            }
        }
    };
    return S.inner;
}

pub fn stringLen(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
            const obj = getStringObject(T, args[0]);
            if (isAstringObject(T, obj)) {
                if (T == .astring) {
                    return Value.initInt(@intCast(obj.astring.len));
                } else if (T == .staticAstring) {
                    return Value.initInt(@intCast(obj.len()));
                } else if (T == .slice) {
                    return Value.initInt(@intCast(obj.stringSlice.len));
                } else fatal();
            } else if (isUstringObject(T, obj)) {
                return Value.initInt(@intCast(getStringCharLen(T, vm, obj)));
            } else if (T == .rawstring) {
                return Value.initInt(@intCast(obj.rawstring.len));
            } else if (T == .rawSlice) {
                return Value.initInt(@intCast(obj.rawstringSlice.len));
            } else fatal();
        }
    };
    return S.inner;
}

fn stringCharAt(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.charAt()", "0.2", "Use string.sliceAt() instead.", &.{});
            return @call(.never_inline, stringSliceAt(T), .{vm, args, nargs});
        }
    };
    return S.inner;
}

pub fn stringSliceAt(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            var idx = args[1].asInteger();

            if (isAstringObject(T, obj)) {
                if (idx < 0) {
                    idx = @as(i48, @intCast(str.len)) + idx;
                }
                if (idx < 0 or idx >= str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                if (T == .staticAstring) {
                    return Value.initStaticAstring(obj.start + uidx, 1);
                } else {
                    // TODO: return slice.
                    return vm.allocAstring(str[uidx..uidx + 1]) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                if (idx < 0) {
                    idx = @as(i48, @intCast(getStringCharLen(T, vm, obj))) + idx;
                }
                if (idx < 0 or idx >= getStringCharLen(T, vm, obj)) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
                const slice = cy.string.utf8CharSliceAtNoCheck(str, start);

                setUstringMruChar(T, vm, obj, uidx, start);
                if (slice.len == 1) {
                    if (T == .staticUstring) {
                        return Value.initStaticAstring(obj.start + start, 1);
                    } else {
                        return vm.allocAstring(slice) catch fatal();
                    }
                } else {
                    return vm.allocUstring(slice, 1) catch fatal();
                }
            } else if (isRawStringObject(T)) {
                if (idx < 0) {
                    idx = @as(i48, @intCast(str.len)) + idx;
                }
                if (idx < 0 or idx >= str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                if (cy.utf8CharSliceAt(str, uidx)) |slice| {
                    if (slice.len == 1) {
                        return vm.allocAstring(slice) catch fatal();
                    } else {
                        return vm.allocUstring(slice, 1) catch fatal();
                    }
                } else {
                    return prepareThrowSymbol(vm, .InvalidRune);
                }
            } else fatal();
        }
    };
    return S.inner;
}

pub fn stringCodeAt(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.codeAt()", "0.2", "Use string.runeAt() instead.", &.{});
            return @call(.never_inline, stringRuneAt(T), .{vm, args, nargs});
        }
    };
    return S.inner;
}

fn stringRuneAt(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const idx = args[1].asInteger();
            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx >= str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                return Value.initInt(@intCast(str[@intCast(idx)]));
            } else if (isUstringObject(T, obj)) {
                if (idx < 0 or idx >= getStringCharLen(T, vm, obj)) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
                const slice = cy.string.utf8CharSliceAtNoCheck(str, start);

                const cp = std.unicode.utf8Decode(slice) catch cy.fatal();
                setUstringMruChar(T, vm, obj, uidx, start);
                return Value.initInt(@intCast(cp));
            } else if (isRawStringObject(T)) {
                if (idx < 0 or idx >= str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                if (cy.utf8CharSliceAt(str, uidx)) |slice| {
                    const cp = std.unicode.utf8Decode(slice) catch cy.fatal();
                    return Value.initInt(@intCast(cp));
                } else {
                    return prepareThrowSymbol(vm, .InvalidRune);
                }
            } else fatal();
        }
    };
    return S.inner;
}

fn rawStringInsertByteCommon(vm: *cy.UserVM, str: []const u8, indexv: Value, val: Value) Value {
    const index: i48 = indexv.asInteger();
    if (index < 0 or index > str.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    const byte: u8 = @intCast(val.asInteger());
    const new = vm.allocUnsetRawStringObject(str.len + 1) catch cy.fatal();
    const buf = new.rawstring.getSlice();
    const uidx: usize = @intCast(index);
    std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
    buf[uidx] = byte;
    std.mem.copy(u8, buf[uidx+1..], str[uidx..]);
    return Value.initPtr(new);
}

fn rawStringInsertByte(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const str = obj.rawstring.getConstSlice();
    return rawStringInsertByteCommon(vm, str, args[1], args[2]);
}

fn rawStringSliceInsertByte(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = args[0].asHeapObject();
    const str = obj.rawstringSlice.getConstSlice();
    return rawStringInsertByteCommon(vm, str, args[1], args[2]);
}

fn ustringReplaceCommon(vm: *cy.UserVM, str: []const u8, needlev: Value, replacev: Value) linksection(cy.StdSection) ?Value {
    var ncharLen: u32 = undefined;
    const needle = vm.valueToTempString2(needlev, &ncharLen);
    var rcharLen: u32 = undefined;
    const replacement = vm.valueToNextTempString2(replacev, &rcharLen);

    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        const new = vm.allocUnsetUstringObject(newLen, @intCast(str.len + idxBuf.len * rcharLen - idxBuf.len * ncharLen)) catch fatal();
        const newBuf = new.ustring.getSlice();
        const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
        cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
        return vm.allocOwnedUstring(new) catch fatal();
    } else {
        return null;
    }
}

inline fn isRawStringObject(comptime T: cy.StringType) bool {
    return T == .rawstring or T == .rawSlice;
}

inline fn isUstringObject(comptime T: cy.StringType, obj: StringObject(T)) bool {
    return T == .staticUstring or T == .ustring or (T == .slice and !cy.heap.StringSlice.isAstring(obj.stringSlice));
}

inline fn isAstringObject(comptime T: cy.StringType, obj: StringObject(T)) bool {
    return T == .staticAstring or T == .astring or (T == .slice and cy.heap.StringSlice.isAstring(obj.stringSlice));
}

fn stringRepeat(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);

            const n = args[1].asInteger();
            if (n < 0) {
                return prepareThrowSymbol(vm, .InvalidArgument);
            }

            var un: u32 = @intCast(n);
            const len = un * str.len;
            if (un > 1 and len > 0) {
                var new: *cy.HeapObject = undefined;
                var buf: []u8 = undefined;
                if (isAstringObject(T, obj)) {
                    new = vm.allocUnsetAstringObject(len) catch fatal();
                    buf = new.astring.getSlice();
                } else if (isUstringObject(T, obj)) {
                    const charLen = getStringCharLen(T, vm, obj);
                    new = vm.allocUnsetUstringObject(len, charLen) catch fatal();
                    buf = new.ustring.getSlice();
                } else if (isRawStringObject(T)) {
                    new = vm.allocUnsetRawStringObject(len) catch fatal();
                    buf = new.rawstring.getSlice();
                } else fatal();
                // This is already quite fast since it has good cache locality.
                // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
                var i: u32 = 0;
                var dst: u32 = 0;
                while (i < un) : (i += 1) {
                    std.mem.copy(u8, buf[dst..dst + str.len], str);
                    dst += @intCast(str.len);
                }

                return Value.initPtr(new);
            } else {
                if (un == 0) {
                    if (isRawStringObject(T)) {
                        return vm.allocRawString("") catch fatal();
                    } else {
                        return Value.initStaticAstring(0, 0);
                    }
                } else {
                    if (isHeapString(T)) {
                        vm.retainObject(obj);
                        return Value.initPtr(obj);
                    } else {
                        return args[0];
                    }
                }
            }
        }
    };
    return S.inner;
}

inline fn isHeapString(comptime T: cy.StringType) bool {
    return T == .astring or T == .ustring or T == .rawstring or T == .slice or T == .rawSlice;
}

fn StringObject(comptime T: cy.StringType) type {
    return switch (T) {
        .staticAstring,
        .staticUstring => cy.IndexSlice(u32),
        .astring,
        .ustring,
        .slice,
        .rawSlice,
        .rawstring => *cy.HeapObject,
    };
}

inline fn releaseStringObject(comptime T: cy.StringType, vm: *cy.UserVM, obj: StringObject(T)) void {
    if (StringObject(T) == *cy.HeapObject) {
        vm.releaseObject(obj);
    }
}

inline fn getStringObject(comptime T: cy.StringType, recv: Value) StringObject(T) {
    switch (StringObject(T)) {
        *cy.HeapObject => {
            return recv.asHeapObject();
        },
        cy.IndexSlice(u32) => {
            return recv.asStaticStringSlice();
        },
        else => @compileError("Unexpected type: " ++ @typeName(StringObject(T))),
    }
}

const UstringMruChar = struct {
    charIdx: u32,
    byteIdx: u32,
};

inline fn setUstringMruChar(comptime T: cy.StringType, vm: *cy.UserVM, obj: StringObject(T), charIdx: u32, byteIdx: u32) void {
    if (T == .ustring) {
        obj.ustring.mruCharIdx = charIdx;
        obj.ustring.mruIdx = byteIdx;
    } else if (T == .slice) {
        obj.stringSlice.uMruCharIdx = charIdx;
        obj.stringSlice.uMruIdx = byteIdx;
    } else if (T == .staticUstring) {
        const header = vm.getStaticUstringHeader(obj.start);
        header.mruCharIdx = charIdx;
        header.mruIdx = byteIdx;
    } else @compileError("Unexpected " ++ @tagName(T));
}

inline fn getUstringMruChar(comptime T: cy.StringType, vm: *cy.UserVM, obj: StringObject(T)) UstringMruChar {
    if (T == .ustring) {
        return .{
            .charIdx = obj.ustring.mruCharIdx,
            .byteIdx = obj.ustring.mruIdx,
        };
    } else if (T == .slice) {
        return .{
            .charIdx = obj.stringSlice.uMruCharIdx,
            .byteIdx = obj.stringSlice.uMruIdx,
        };
    } else if (T == .staticUstring) {
        const header = vm.getStaticUstringHeader(obj.start);
        return .{
            .charIdx = header.mruCharIdx,
            .byteIdx = header.mruIdx,
        };
    } else @compileError("Unexpected " ++ @tagName(T));
}

inline fn getStringCharLen(comptime T: cy.StringType, vm: *cy.UserVM, obj: StringObject(T)) u32 {
    if (T == .ustring) {
        return obj.ustring.charLen;
    } else if (T == .slice) {
        return obj.stringSlice.uCharLen;
    } else if (T == .staticUstring) {
        return vm.getStaticUstringHeader(obj.start).charLen;
    } else @compileError("Unexpected " ++ @tagName(T));
}

inline fn getStringSlice(comptime T: cy.StringType, vm: *cy.UserVM, obj: StringObject(T)) []const u8 {
    switch (T) {
        .staticAstring,
        .staticUstring => {
            return vm.getStaticString(obj.start, obj.end);
        },
        .astring => {
            return obj.astring.getConstSlice();
        },
        .ustring => {
            return obj.ustring.getConstSlice();
        },
        .slice => {
            return cy.heap.StringSlice.getConstSlice(obj.stringSlice);
        },
        .rawstring => {
            return obj.rawstring.getConstSlice();
        },
        .rawSlice => {
            return obj.rawstringSlice.getConstSlice();
        },
    }
}

fn stringSplit(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const delim = vm.valueToTempString(args[1]);

            const res = vm.allocEmptyList() catch fatal();
            if (delim.len == 0) {
                return res;
            }
            const list = res.asHeapObject();

            var iter = std.mem.split(u8, str, delim);
            while (iter.next()) |part| {
                switch (T) {
                    .rawstring,
                    .rawSlice => {
                        vm.retainObject(obj);
                        const slice = vm.allocRawStringSlice(part, obj) catch fatal();
                        list.list.append(vm.allocator(), slice);
                    },
                    .staticAstring => {
                        const offset: u32 = @intCast(@intFromPtr(part.ptr) - @intFromPtr(str.ptr));
                        const partv = Value.initStaticAstring(obj.start + offset, @intCast(part.len));
                        list.list.append(vm.allocator(), partv);
                    },
                    .astring => {
                        vm.retainObject(obj);
                        const partv = vm.allocAstringSlice(part, obj) catch fatal();
                        list.list.append(vm.allocator(), partv);
                    },
                    .staticUstring => {
                        const runeLen: u32 = @intCast(cy.string.utf8Len(part));
                        const partv = vm.allocUstringSlice(part, runeLen, null) catch fatal();
                        list.list.append(vm.allocator(), partv);
                    },
                    .ustring => {
                        vm.retainObject(obj);
                        const runeLen: u32 = @intCast(cy.string.utf8Len(part));
                        const partv = vm.allocUstringSlice(part, runeLen, obj) catch fatal();
                        list.list.append(vm.allocator(), partv);
                    },
                    .slice => {
                        if (cy.heap.StringSlice.isAstring(obj.stringSlice)) {
                            vm.retainObject(obj);
                            const partv = vm.allocAstringSlice(part, obj) catch fatal();
                            list.list.append(vm.allocator(), partv);
                        } else {
                            const runeLen: u32 = @intCast(cy.string.utf8Len(part));
                            vm.retainObject(obj);
                            const partv = vm.allocUstringSlice(part, runeLen, obj) catch fatal();
                            list.list.append(vm.allocator(), partv);
                        }
                    }
                }
            }
            return res;
        }
    };
    return S.inner;
}

fn stringTrim(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);

            const str = getStringSlice(T, vm, obj);
            const trimRunes = vm.valueToTempString(args[2]);

            var res: []const u8 = undefined;
            const mode = getBuiltinSymbol(args[1].asSymbolId()) orelse {
                return prepareThrowSymbol(vm, .InvalidArgument);
            };
            switch (mode) {
                .left => res = std.mem.trimLeft(u8, str, trimRunes),
                .right => res = std.mem.trimRight(u8, str, trimRunes),
                .ends => res = std.mem.trim(u8, str, trimRunes),
                else => {
                    return prepareThrowSymbol(vm, .InvalidArgument);
                }
            }

            if (isAstringObject(T, obj)) {
                return vm.allocAstring(res) catch fatal();
            } else if (isUstringObject(T, obj)) {
                const runeLen: u32 = @intCast(cy.string.utf8Len(res));
                return vm.allocUstring(res, runeLen) catch fatal();
            } else if (isRawStringObject(T)) {
                return vm.allocRawString(res) catch fatal();
            } else fatal();
        }
    };
    return S.inner;
}

fn stringReplace(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            if (isAstringObject(T, obj)) {
                if (astringReplaceCommon(vm, str, args[1], args[2])) |val| {
                    return val;
                } else {
                    if (T != .staticAstring) {
                        vm.retainObject(obj);
                        return Value.initPtr(obj);
                    } else {
                        return args[0];
                    }
                }
            } else if (isUstringObject(T, obj)) {
                if (ustringReplaceCommon(vm, str, args[1], args[2])) |val| {
                    return val;
                } else {
                    if (T != .staticUstring) {
                        vm.retainObject(obj);
                        return Value.initPtr(obj);
                    } else {
                        return args[0];
                    }
                }
            } else if (isRawStringObject(T)) {
                const needle = vm.valueToTempString(args[1]);
                const replacement = vm.valueToNextTempString(args[2]);

                const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
                idxBuf.clearRetainingCapacity();
                defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
                const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
                const numIdxes = @divExact(idxBuf.len, 4);
                if (numIdxes > 0) {
                    const new = vm.allocUnsetRawStringObject(newLen) catch fatal();
                    const newBuf = new.rawstring.getSlice();
                    const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
                    cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
                    return Value.initPtr(new);
                } else {
                    vm.retainObject(obj);
                    return Value.initPtr(obj);
                }
            } else fatal();
        }
    };
    return S.inner;
}

fn astringReplaceCommon(vm: *cy.UserVM, str: []const u8, needlev: Value, replacev: Value) linksection(cy.StdSection) ?Value {
    const needle = vm.valueToTempString(needlev);
    var rcharLen: u32 = undefined;
    const replacement = vm.valueToNextTempString2(replacev, &rcharLen);
    const idxBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        if (rcharLen == replacement.len) {
            const new = vm.allocUnsetAstringObject(newLen) catch fatal();
            const newBuf = new.astring.getSlice();
            const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
            cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return vm.allocOwnedAstring(new) catch fatal();
        } else {
            const new = vm.allocUnsetUstringObject(newLen, @intCast(str.len + idxBuf.len * rcharLen - idxBuf.len * needle.len)) catch fatal();
            const newBuf = new.ustring.getSlice();
            const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
            cy.replaceAtIdxes(newBuf, str, @intCast(needle.len), replacement, idxes);
            return vm.allocOwnedUstring(new) catch fatal();
        }
    } else {
        return null;
    }
}

pub fn stringSlice(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);

            if (isAstringObject(T, obj)) {
                var start = args[1].asInteger();
                if (start < 0) {
                    start = @as(i48, @intCast(str.len)) + start;
                }
                var end: i48 = undefined;
                if (args[2].isNone()) {
                    end = @intCast(str.len);
                } else if (args[2].isInteger()) {
                    end = args[2].asInteger();
                } else {
                    return prepareThrowSymbol(vm, .InvalidArgument);
                }
                if (end < 0) {
                    end = @as(i48, @intCast(str.len)) + end;
                }
                if (start < 0 or end > str.len or end < start) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const ustart: u32 = @intCast(start);
                const uend: u32 = @intCast(end);

                if (T == .staticAstring) {
                    return Value.initStaticAstring(obj.start + ustart, @intCast(uend - ustart));
                } else {
                    vm.retainObject(obj);
                    return vm.allocAstringSlice(str[ustart..uend], obj) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                const charLen = getStringCharLen(T, vm, obj);
                var start: i48 = args[1].asInteger();
                if (start < 0) {
                    start = @as(i48, @intCast(charLen)) + start;
                }
                var end: i48 = if (args[2].isNone()) @intCast(charLen) else args[2].asInteger();
                if (end < 0) {
                    end = @as(i48, @intCast(charLen)) + end;
                }
                if (start < 0 or end > charLen or end < start) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const ustart: u32 = @intCast(start);
                const uend: u32 = @intCast(end);

                const mru = getUstringMruChar(T, vm, obj);
                const startByteIdx: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, ustart));
                setUstringMruChar(T, vm, obj, ustart, startByteIdx);
                const endByteIdx: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, startByteIdx, ustart, uend));

                if (T == .staticUstring) {
                    return vm.allocUstringSlice(str[startByteIdx..endByteIdx], uend - ustart, null) catch fatal();
                } else {
                    vm.retainObject(obj);
                    return vm.allocUstringSlice(str[startByteIdx..endByteIdx], uend - ustart, obj) catch fatal();
                }
            } else if (isRawStringObject(T)) {
                var start: i48 = args[1].asInteger();
                if (start < 0) {
                    start = @as(i48, @intCast(str.len)) + start;
                }
                var end: i48 = if (args[2].isNone()) @intCast(str.len) else args[2].asInteger();
                if (end < 0) {
                    end = @as(i48, @intCast(str.len)) + end;
                }
                if (start < 0 or end > str.len or end < start) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }

                vm.retainObject(obj);
                return vm.allocRawStringSlice(str[@intCast(start)..@intCast(end)], obj) catch fatal();
            } else fatal();
        }
    };
    return S.inner;
}

fn stringAppend(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.append()", "0.1", "Use string.concat() instead.", &.{});
            return stringConcat(T)(vm, args, nargs);
        }
    };
    return S.inner;
}

fn stringConcat(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            if (isAstringObject(T, obj)) {
                var rcharLen: u32 = undefined;
                const rstr = vm.valueToTempString2(args[1], &rcharLen);
                if (rcharLen == rstr.len) {
                    return vm.allocAstringConcat(str, rstr) catch fatal();
                } else {
                    return vm.allocUstringConcat(str, rstr, @intCast(str.len + rcharLen)) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                var rcharLen: u32 = undefined;
                const rstr = vm.valueToTempString2(args[1], &rcharLen);
                const charLen = getStringCharLen(T, vm, obj);
                return vm.allocUstringConcat(str, rstr, charLen + rcharLen) catch fatal();
            } else if (isRawStringObject(T)) {
                const rstr = vm.valueToTempString(args[1]);
                return vm.allocRawStringConcat(str, rstr) catch fatal();
            } else fatal();
        }
    };
    return S.inner;
}

fn stringInsert(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const idx = args[1].asInteger();
            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx > str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                var insertCharLen: u32 = undefined;
                const insert = vm.valueToTempString2(args[2], &insertCharLen);
                const uidx: u32 = @intCast(idx);
                if (insertCharLen == insert.len) {
                    return vm.allocAstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
                } else {
                    return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..], @intCast(str.len + insertCharLen)) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                const charLen = getStringCharLen(T, vm, obj);
                if (idx < 0 or idx > charLen) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                var insertCharLen: u32 = undefined;
                const insert = vm.valueToTempString2(args[2], &insertCharLen);
                const uidx: u32 = @intCast(idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));

                setUstringMruChar(T, vm, obj, uidx, start);
                return vm.allocUstringConcat3(str[0..start], insert, str[start..], @intCast(charLen + insertCharLen)) catch fatal();
            } else if (isRawStringObject(T)) {
                if (idx < 0 or idx > str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                } 
                const insert = vm.valueToTempString(args[2]);
                const new = vm.allocUnsetRawStringObject(str.len + insert.len) catch cy.fatal();
                const buf = new.rawstring.getSlice();
                const uidx: u32 = @intCast(idx);
                std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
                std.mem.copy(u8, buf[uidx..uidx+insert.len], insert);
                std.mem.copy(u8, buf[uidx+insert.len..], str[uidx..]);
                return Value.initPtr(new);
            } else fatal();
        }
    };
    return S.inner;
}

fn stringIndex(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.index()", "0.2", "Use string.find() instead.", &.{});
            return @call(.never_inline, stringFind(T), .{vm, args, nargs});
        }
    };
    return S.inner;
}

fn stringFind(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[1]);
            if (needle.len > 0 and needle.len <= str.len) {
                if (needle.len == 1) {
                    // One ascii char special case. Perform indexOfChar.
                    if (cy.string.indexOfChar(str, needle[0])) |idx| {
                        if (isUstringObject(T, obj)) {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initInt(@intCast(charIdx));
                        } else {
                            return Value.initInt(@intCast(idx));
                        }
                    }
                }
                if (cy.string.indexOf(str, needle)) |idx| {
                    if (isUstringObject(T, obj)) {
                        const charIdx = cy.toUtf8CharIdx(str, idx);
                        return Value.initInt(@intCast(charIdx));
                    } else {
                        return Value.initInt(@intCast(idx));
                    }
                }
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringStartsWith(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[1]);
            return Value.initBool(std.mem.startsWith(u8, str, needle));
        }
    };
    return S.inner;
}

fn stringEndsWith(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[1]);
            return Value.initBool(std.mem.endsWith(u8, str, needle));
        }
    };
    return S.inner;
}

fn rawStringSliceToString(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("rawstring.toString()", "0.1", "Use rawstring.utf8() instead.", &.{});
    return rawStringSliceUtf8(vm, args, nargs);
}

fn rawStringSliceUtf8(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.rawstringSlice.getConstSlice();
    if (cy.validateUtf8(str)) |size| {
        if (size == str.len) {
            return vm.allocAstring(str) catch fatal();
        } else {
            return vm.allocUstring(str, @intCast(size)) catch fatal();
        }
    } else {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
}

fn rawStringToString(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("rawstring.toString()", "0.1", "Use rawstring.utf8() instead.", &.{});
    return rawStringUtf8(vm, args, nargs);
}

fn rawStringUtf8(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const str = obj.rawstring.getConstSlice();
    if (cy.validateUtf8(str)) |size| {
        if (size == str.len) {
            return vm.allocAstring(str) catch fatal();
        } else {
            return vm.allocUstring(str, @intCast(size)) catch fatal();
        }
    } else {
        return prepareThrowSymbol(vm, .InvalidRune);
    }
}

fn rawStringSliceByteAt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const idx: i48 = args[1].asInteger();
    if (idx < 0 or idx >= obj.rawstringSlice.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }
    const str = obj.rawstringSlice.getConstSlice();
    const uidx: usize = @intCast(idx);
    return Value.initInt(@intCast(str[uidx]));
}

fn rawStringByteAt(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = args[0].asHeapObject();
    const idx: i48 = args[1].asInteger();
    if (idx < 0 or idx >= obj.rawstring.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }
    const str = obj.rawstring.getConstSlice();
    const uidx: usize = @intCast(idx);
    return Value.initInt(@intCast(str[uidx]));
}

fn valueToChar(vm: *cy.UserVM, val: Value) u8 {
    if (val.isString()) {
        const needle = vm.valueToTempString(val);
        if (needle.len > 0) {
            return needle[0];
        } else {
            return 0;
        }
    } else {
       return @intFromFloat(val.toF64());
    }
}

fn stringIsAscii(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            if (T == .staticAstring) {
                return Value.True;
            } else if (T == .staticUstring) {
                return Value.False;
            } else {
                const obj = getStringObject(T, args[0]);
                if (isAstringObject(T, obj)) {
                    return Value.True;
                } else if (isUstringObject(T, obj)) {
                    return Value.False;
                } else if (isRawStringObject(T)) {
                    return Value.initBool(cy.isAstring(obj.rawstring.getConstSlice()));
                } else fatal();
            }
        }
    };
    return S.inner;
}

pub fn stringIndexCharSet(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.indexCharSet()", "0.2", "Use string.findAnyRune() instead.", &.{});
            return @call(.never_inline, stringFindAnyRune(T), .{vm, args, nargs});
        }
    };
    return S.inner;
}

fn stringFindAnyRune(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            var setCharLen: u32 = undefined;
            const set = vm.valueToTempString2(args[1], &setCharLen);
            const setIsAscii = setCharLen == set.len;

            if (set.len > 0) {
                if (isAstringObject(T, obj)) {
                    if (setIsAscii) {
                        if (@call(.never_inline, cy.indexOfAsciiSet, .{str, set})) |idx| {
                            return Value.initInt(@intCast(idx));
                        }
                    } else {
                        // Filter ascii codepoints.
                        const alloc = vm.allocator();
                        const tempBuf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
                        tempBuf.clearRetainingCapacity();
                        defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();
                        var iter = std.unicode.Utf8Iterator{
                            .bytes = set,
                            .i = 0,
                        };
                        while (iter.nextCodepoint()) |cp| {
                            if (cp < 128) {
                                tempBuf.append(alloc, @intCast(cp)) catch fatal();
                            }
                        }
                        if (tempBuf.len > 0) {
                            if (cy.indexOfAsciiSet(str, tempBuf.items())) |idx| {
                                return Value.initInt(@intCast(idx));
                            }
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (setIsAscii) {
                        if (cy.indexOfAsciiSet(str, set)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initInt(@intCast(charIdx));
                        }
                    } else {
                        // Slow. Checks every utf8 code and applies min.
                        var minIndex: u32 = NullId;
                        var iter = std.unicode.Utf8Iterator{
                            .bytes = set,
                            .i = 0,
                        };
                        while (iter.nextCodepoint()) |cp| {
                            if (cy.charIndexOfCodepoint(str, cp)) |idx| {
                                if (idx < minIndex) {
                                    minIndex = @intCast(idx);
                                }
                            }
                        }
                        if (minIndex != NullId) {
                            return Value.initInt(@intCast(minIndex));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (setIsAscii) {
                        if (cy.indexOfAsciiSet(str, set)) |idx| {
                            return Value.initInt(@intCast(idx));
                        }
                    } else {
                        // Slow. Checks every utf8 code and applies min.
                        var minIndex: u32 = NullId;
                        var iter = std.unicode.Utf8Iterator{
                            .bytes = set,
                            .i = 0,
                        };
                        while (iter.nextCodepointSlice()) |slice| {
                            if (std.mem.indexOf(u8, str, slice)) |idx| {
                                if (idx < minIndex) {
                                    minIndex = @intCast(idx);
                                }
                            }
                        }
                        if (minIndex != NullId) {
                            return Value.initInt(@intCast(minIndex));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

pub fn stringIndexCode(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.indexCode()", "0.2", "Use string.findRune() instead.", &.{});
            return @call(.never_inline, stringFindRune(T), .{vm, args, nargs});
        }
    };
    return S.inner;
}

fn stringFindRune(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, args[0]);
            const str = getStringSlice(T, vm, obj);
            const needle = args[1].asInteger();

            if (needle > 0) {
                const code: u21 = @intCast(needle);
                const needleIsAscii = code < 128;
                if (isAstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, @intCast(code))) |idx| {
                            return Value.initInt(@intCast(idx));
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, @intCast(code))) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initInt(@intCast(charIdx));
                        }
                    } else {
                        var slice: [4]u8 = undefined;
                        _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                        if (cy.string.indexOf(str, &slice)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initInt(@intCast(charIdx));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (needleIsAscii) {
                        if (cy.indexOfChar(str, @intCast(code))) |idx| {
                            return Value.initInt(@intCast(idx));
                        }
                    } else {
                        var slice: [4]u8 = undefined;
                        _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                        if (cy.string.indexOf(str, &slice)) |idx| {
                            return Value.initInt(@intCast(idx));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringIndexChar(comptime T: cy.StringType) cy.ZHostFuncFn {
    const S = struct {
        fn inner(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.indexChar()", "0.2", "Use string.findRune() instead.", &.{});
            const needle = vm.valueToTempString(args[1]);
            if (needle.len > 0) {
                const cp = cy.string.utf8CodeAtNoCheck(needle, 0);
                return @call(.never_inline, stringFindRune(T), .{vm, &[_]Value{Value.initF64(@floatFromInt(cp))}, 1});
            }
            return Value.None;
        }
    };
    return S.inner;
}

pub fn fileStreamLines(vm: *cy.UserVM, args: [*]const Value, nargs: u8) linksection(StdSection) Value {
    return fileStreamLines1(vm, &[_]Value{ args[0], Value.initF64(@floatFromInt(4096)) }, nargs);
}

pub fn fileStreamLines1(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = args[0].asHeapObject();
    const bufSize: u32 = @intFromFloat(args[1].asF64());
    var createReadBuf = true;
    if (obj.file.hasReadBuf) {
        if (bufSize != obj.file.readBufCap) {
            // Cleanup previous buffer.
            vm.allocator().free(obj.file.readBuf[0..obj.file.readBufCap]);
        } else {
            createReadBuf = false;
        }
    }
    // Allocate read buffer.
    obj.file.iterLines = true;
    if (createReadBuf) {
        const readBuf = vm.allocator().alloc(u8, bufSize) catch cy.fatal();
        obj.file.readBuf = readBuf.ptr;
        obj.file.readBufCap = @intCast(readBuf.len);
        obj.file.hasReadBuf = true;
    }
    return args[0];
}

pub fn dirWalk(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();
    if (obj.dir.iterable) {
        vm.retainObject(obj);
        return vm.allocDirIterator(@ptrCast(obj), true) catch fatal();
    } else {
        return prepareThrowSymbol(vm, .NotAllowed);
    }
}

pub fn dirIterator(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();
    if (obj.dir.iterable) {
        vm.retainObject(obj);
        return vm.allocDirIterator(@ptrCast(obj), false) catch fatal();
    } else {
        return prepareThrowSymbol(vm, .NotAllowed);
    }
}

pub fn fileIterator(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = args[0].asHeapObject();
    obj.file.curPos = 0;
    obj.file.readBufEnd = 0;
    return args[0];
}

pub fn fileSeekFromEnd(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();
    if (numBytes > 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const file = obj.file.getStdFile();
    file.seekFromEnd(numBytes) catch |err| {
        return fromUnsupportedError(vm, "seekFromEnd", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileSeekFromCur(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();

    const file = obj.file.getStdFile();
    file.seekBy(numBytes) catch |err| {
        return fromUnsupportedError(vm, "seekFromCur", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileSeek(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();
    if (numBytes < 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const file = obj.file.getStdFile();
    const unumBytes: u32 = @intCast(numBytes);
    file.seekTo(unumBytes) catch |err| {
        return fromUnsupportedError(vm, "seek", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileWrite(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    var buf = vm.valueToTempRawString(args[1]);
    const file = obj.file.getStdFile();
    const numWritten = file.write(buf) catch |err| {
        return fromUnsupportedError(vm, "write", err, @errorReturnTrace());
    };

    return Value.initInt(@intCast(numWritten));
}

pub fn fileClose(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();
    obj.file.close();
    return Value.None;
}

pub fn fileRead(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes = args[1].asInteger();
    if (numBytes <= 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
    const unumBytes: usize = @intCast(numBytes);
    const file = obj.file.getStdFile();

    const alloc = vm.allocator();
    const ivm: *cy.VM = @ptrCast(vm);
    const tempBuf = &ivm.u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();
    tempBuf.ensureTotalCapacityPrecise(alloc, unumBytes) catch fatal();

    const numRead = file.read(tempBuf.buf[0..unumBytes]) catch |err| {
        return fromUnsupportedError(vm, "read", err, @errorReturnTrace());
    };
    // Can return empty string when numRead == 0.
    return vm.allocRawString(tempBuf.buf[0..numRead]) catch fatal();
}

pub fn fileReadToEnd(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const file = obj.file.getStdFile();

    const alloc = vm.allocator();
    const ivm: *cy.VM = @ptrCast(vm);
    const tempBuf = &ivm.u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();

    const MinReadBufSize = 4096;
    tempBuf.ensureTotalCapacity(alloc, MinReadBufSize) catch fatal();

    while (true) {
        const buf = tempBuf.buf[tempBuf.len .. tempBuf.buf.len];
        const numRead = file.readAll(buf) catch |err| {
            return fromUnsupportedError(vm, "readToEnd", err, @errorReturnTrace());
        };
        tempBuf.len += numRead;
        if (numRead < buf.len) {
            // Done.
            const all = tempBuf.items();
            // Can return empty string.
            return vm.allocRawString(all) catch fatal();
        } else {
            tempBuf.ensureUnusedCapacity(alloc, MinReadBufSize) catch fatal();
        }
    }
}

pub fn fileOrDirStat(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    if (obj.getTypeId() == rt.FileT) {
        if (obj.file.closed) {
            return prepareThrowSymbol(vm, .Closed);
        }
    } else {
        if (obj.dir.closed) {
            return prepareThrowSymbol(vm, .Closed);
        }
    }

    // File/Dir share the same fd member offset.
    const file = obj.file.getStdFile();
    const stat = file.stat() catch |err| {
        return fromUnsupportedError(vm, "stat", err, @errorReturnTrace());
    };

    const ivm = vm.internal();

    const map = vm.allocEmptyMap() catch fatal();
    const sizeKey = vm.allocAstring("size") catch fatal();
    const modeKey = vm.allocAstring("mode") catch fatal();
    const typeKey = vm.allocAstring("type") catch fatal();
    const atimeKey = vm.allocAstring("atime") catch fatal();
    const ctimeKey = vm.allocAstring("ctime") catch fatal();
    const mtimeKey = vm.allocAstring("mtime") catch fatal();
    defer {
        vm.release(sizeKey);
        vm.release(modeKey);
        vm.release(typeKey);
        vm.release(atimeKey);
        vm.release(ctimeKey);
        vm.release(mtimeKey);
    }
    map.asHeapObject().map.set(ivm, sizeKey, Value.initF64(@floatFromInt(stat.size))) catch fatal();
    map.asHeapObject().map.set(ivm, modeKey, Value.initF64(@floatFromInt(stat.mode))) catch fatal();
    const typeTag: Symbol = switch (stat.kind) {
        .file => .file,
        .directory => .dir,
        else => .unknown,
    };
    map.asHeapObject().map.set(ivm, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch fatal();
    map.asHeapObject().map.set(ivm, atimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.atime, 1000000)))) catch fatal();
    map.asHeapObject().map.set(ivm, ctimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.ctime, 1000000)))) catch fatal();
    map.asHeapObject().map.set(ivm, mtimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.mtime, 1000000)))) catch fatal();
    return map;
}

pub fn metatypeId(_: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(obj.metatype.symId);
}

pub fn dirIteratorNext(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();

    const ivm = vm.internal();
    const iter: *cy.DirIterator = @ptrCast(obj);
    if (iter.recursive) {
        const walker = cy.ptrAlignCast(*std.fs.IterableDir.Walker, &iter.inner.walker);
        const entryOpt = walker.next() catch |err| {
            return fromUnsupportedError(vm, "next", err, @errorReturnTrace());
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch fatal();
            const pathKey = vm.allocAstring("path") catch fatal();
            const nameKey = vm.allocAstring("name") catch fatal();
            const typeKey = vm.allocAstring("type") catch fatal();
            defer {
                vm.release(pathKey);
                vm.release(nameKey);
                vm.release(typeKey);
            }
            const entryPath = vm.allocRawString(entry.path) catch fatal();
            const entryName = vm.allocRawString(entry.basename) catch fatal();
            defer {
                vm.release(entryPath);
                vm.release(entryName);
            }
            map.asHeapObject().map.set(ivm, pathKey, entryPath) catch fatal();
            map.asHeapObject().map.set(ivm, nameKey, entryName) catch fatal();
            const typeTag: Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            map.asHeapObject().map.set(ivm, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch fatal();
            return map;
        } else {
            return Value.None;
        }
    } else {
        const stdIter = cy.ptrAlignCast(*std.fs.IterableDir.Iterator, &iter.inner.iter);
        const entryOpt = stdIter.next() catch |err| {
            return fromUnsupportedError(vm, "next", err, @errorReturnTrace());
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch fatal();
            const nameKey = vm.allocAstring("name") catch fatal();
            const typeKey = vm.allocAstring("type") catch fatal();
            const entryName = vm.allocRawString(entry.name) catch fatal();
            defer {
                vm.release(nameKey);
                vm.release(typeKey);
                vm.release(entryName);
            }
            map.asHeapObject().map.set(ivm, nameKey, entryName) catch fatal();
            const typeTag: Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            map.asHeapObject().map.set(ivm, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch fatal();
            return map;
        } else {
            return Value.None;
        }
    }
}

pub fn fileNext(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = args[0].asHeapObject();
    if (obj.file.iterLines) {
        const alloc = vm.allocator();
        const readBuf = obj.file.readBuf[0..obj.file.readBufCap];
        if (cy.getLineEnd(readBuf[obj.file.curPos..obj.file.readBufEnd])) |end| {
            // Found new line.
            const line = vm.allocRawString(readBuf[obj.file.curPos..obj.file.curPos+end]) catch cy.fatal();

            // Advance pos.
            obj.file.curPos += @intCast(end);

            return line;
        }

        var lineBuf = cy.HeapRawStringBuilder.init(@ptrCast(vm)) catch fatal();
        defer lineBuf.deinit();
        // Start with previous string without line delimiter.
        lineBuf.appendString(alloc, readBuf[obj.file.curPos..obj.file.readBufEnd]) catch cy.fatal();

        // Read into buffer.
        const file = obj.file.getStdFile();
        const reader = file.reader();

        while (true) {
            const bytesRead = reader.read(readBuf) catch cy.fatal();
            if (bytesRead == 0) {
                // End of stream.
                obj.file.iterLines = false;
                if (lineBuf.len > 0) {
                    return Value.initPtr(lineBuf.ownObject(alloc));
                } else {
                    return Value.None;
                }
            }
            if (cy.getLineEnd(readBuf[0..bytesRead])) |end| {
                // Found new line.
                lineBuf.appendString(alloc, readBuf[0..end]) catch cy.fatal();

                // Advance pos.
                obj.file.curPos = @intCast(end);
                obj.file.readBufEnd = @intCast(bytesRead);

                return Value.initPtr(lineBuf.ownObject(alloc));
            } else {
                lineBuf.appendString(alloc, readBuf[0..bytesRead]) catch cy.fatal();

                // Advance pos.
                obj.file.curPos = @intCast(bytesRead);
                obj.file.readBufEnd = @intCast(bytesRead);
            }
        }
    } else {
        return Value.None;
    }
}

pub fn booleanCall(_: *cy.UserVM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].toBool());
}

pub fn integerCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => {
            return Value.initInt(@intFromFloat(@trunc(val.asF64())));
        },
        .string => {
            var str = vm.valueToTempString(val);
            if (std.mem.indexOfScalar(u8, str, '.')) |idx| {
                str = str[0..idx];
            }
            const res = std.fmt.parseInt(i32, str, 10) catch {
                return Value.initInt(0);
            };
            return Value.initInt(res);
        },
        .enumT => return Value.initInt(@intCast(val.val & @as(u64, 0xFF))),
        .symbol => return Value.initInt(@intCast(val.val & @as(u64, 0xFF))),
        .int => {
            return val;
        },
        else => {
            return Value.initInt(0);
        }
    }
}

pub fn floatCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => return val,
        .string => {
            const res = std.fmt.parseFloat(f64, vm.valueToTempString(val)) catch {
                return Value.initF64(0);
            };
            return Value.initF64(res);
        },
        .enumT => return Value.initF64(@floatFromInt(val.val & @as(u64, 0xFF))),
        .symbol => return Value.initF64(@floatFromInt(val.val & @as(u64, 0xFF))),
        .int => return Value.initF64(@floatFromInt(val.asInteger())),
        .none => return Value.initF64(0),
        .boolean => return Value.initF64(if (val.asBool()) 1 else 0),
        else => {
            vm.release(val);
            return vm.returnPanic("Not a type that can be converted to `float`.");
        }
    }
}

inline fn inlineUnaryOp(pc: [*]cy.Inst, code: cy.OpCode) void {
    const startLocal = pc[1].val;
    // Save callObjSym data.
    pc[8].val = startLocal;
    pc[9] = pc[2];

    // Inline bin op.
    pc[0] = cy.Inst.initOpCode(code);
    pc[1].val = startLocal + 4;
    pc[2].val = startLocal;
}

pub fn intBitwiseNot(_: *cy.UserVM, pc: [*]cy.Inst, _: [*]const Value, _: u8) void {
    inlineUnaryOp(pc, .bitwiseNot);
}

pub fn intNeg(_: *cy.UserVM, pc: [*]cy.Inst, _: [*]const Value, _: u8) void {
    inlineUnaryOp(pc, .negInt);
}

pub fn floatNeg(_: *cy.UserVM, pc: [*]cy.Inst, _: [*]const Value, _: u8) void  {
    inlineUnaryOp(pc, .negFloat);
}

pub fn inlineTernNoRetOp(comptime code: cy.OpCode) cy.QuickenFuncFn {
    const S = struct {
        pub fn method(_: *cy.UserVM, pc: [*]cy.Inst, _: [*]const Value, _: u8) void {
            const startLocal = pc[1].val;
            // Save callObjSym data.
            pc[8].val = startLocal;
            pc[9] = pc[2];
            pc[10] = pc[3];

            pc[0] = cy.Inst.initOpCode(code);
            pc[1].val = startLocal + 4;
            pc[2].val = startLocal + 5;
            pc[3].val = startLocal + 6;
        }
    };
    return S.method;
}

pub fn inlineBinOp(comptime code: cy.OpCode) cy.QuickenFuncFn {
    const S = struct {
        pub fn method(_: *cy.UserVM, pc: [*]cy.Inst, _: [*]const Value, _: u8) void {
            const startLocal = pc[1].val;
            // Save callObjSym data.
            pc[8].val = startLocal;
            pc[9] = pc[2];
            pc[10] = pc[3];

            // Inline bin op.
            pc[0] = cy.Inst.initOpCode(code);
            pc[1].val = startLocal + 4;
            pc[2].val = startLocal + 5;
            pc[3].val = startLocal;
        }
    };
    return S.method;
}

pub fn pointerCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isPointerT()) {
        vm.retain(val);
        return val;
    } else if (val.isInteger()) {
        const i: usize = @intCast(val.asInteger());
        return cy.heap.allocPointer(vm.internal(), @ptrFromInt(i)) catch fatal();
    } else {
        return vm.returnPanic("Not a `pointer`.");
    }
}

pub fn rawstringCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const str = vm.valueToTempRawString(args[0]);
    return vm.allocRawString(str) catch cy.fatal();
}

pub fn stringCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isString()) {
        return val; 
    } else {
        const str = vm.valueToTempString(val);
        return vm.allocStringInfer(str) catch cy.fatal();
    }
}

pub fn errorCall(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = args[0];
    if (val.isPointer()) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    } else {
        if (val.assumeNotPtrIsSymbol()) {
            return Value.initErrorSymbol(@intCast(val.asSymbolId()));
        } else if (val.assumeNotPtrIsEnum()) {
            const enumv = val.asEnum();
            return Value.initErrorEnum(@intCast(enumv.enumId), @intCast(enumv.memberId));
        } else {
            return prepareThrowSymbol(vm, .InvalidArgument);
        }
    }
}

pub fn nop(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return vm.returnPanic("Unsupported.");
}

/// In debug mode, the unsupported error's stack trace is dumped and program panics.
/// In release mode, the error is logged and UnknownError is returned.
pub fn fromUnsupportedError(vm: *cy.UserVM, msg: []const u8, err: anyerror, trace: ?*std.builtin.StackTrace) Value {
    fmt.printStderr("{}: {}\n", &.{fmt.v(msg), fmt.v(err)});
    if (builtin.mode == .Debug) {
        if (!cy.silentError) {
            std.debug.dumpStackTrace(trace.?.*);
        }
        fatal();
    }
    return prepareThrowSymbol(vm, .UnknownError);
}

pub const ModuleBuilder = struct {
    modId: cy.ModuleId,
    compiler: *cy.VMcompiler,
    vm: *cy.VM,

    pub fn init(c: *cy.VMcompiler, modId: cy.ModuleId) ModuleBuilder {
        return .{
            .modId = modId,
            .compiler = c,
            .vm = c.vm,
        };
    }

    pub fn setVar(self: *const ModuleBuilder, name: []const u8, typeSymId: sema.SymbolId, val: Value) !void {
        try self.mod().setTypedVar(self.compiler, name, typeSymId, val);
    }

    pub fn setFunc(self: *const ModuleBuilder, name: []const u8, params: []const sema.SymbolId, ret: sema.SymbolId, ptr: cy.ZHostFuncFn) !void {
        try self.mod().setNativeTypedFunc(self.compiler, name, params, ret, ptr);
    }

    pub fn ensureMethodGroup(self: *const ModuleBuilder, name: []const u8) !vmc.MethodGroupId {
        return self.vm.ensureMethodGroup(name);
    }

    pub fn addOptimizingMethod(
        self: *const ModuleBuilder, typeId: rt.TypeId, mgId: vmc.MethodGroupId,
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
        self: *const ModuleBuilder, typeId: rt.TypeId, mgId: vmc.MethodGroupId,
        params: []const types.TypeId, ret: types.TypeId, ptr: cy.ZHostFuncFn,
    ) !void {
        const funcSigId = try sema.ensureFuncSig(self.compiler, params, ret);
        const funcSig = self.compiler.sema.getFuncSig(funcSigId);
        if (funcSig.isParamsTyped) {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostTyped(funcSigId, ptr, @intCast(params.len)));
        } else {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initHostUntyped(funcSigId, ptr, @intCast(params.len)));
        }
    }

    pub fn addMethod2(
        self: *const ModuleBuilder, typeId: rt.TypeId, mgId: vmc.MethodGroupId,
        params: []const types.TypeId, ret: types.TypeId, ptr: cy.ZHostFuncPairFn,
    ) !void {
        const funcSigId = try sema.ensureFuncSig(self.compiler, params, ret);
        const funcSig = self.compiler.sema.getFuncSig(funcSigId);
        if (funcSig.isParamsTyped) {
            return error.Unsupported;
        } else {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initUntypedNative2(funcSigId, ptr, @intCast(params.len)));
        }
    }

    pub fn mod(self: *const ModuleBuilder) *cy.Module {
        return self.compiler.sema.getModulePtr(self.modId);
    }

    pub fn createAndSetTypeObject(self: *const ModuleBuilder, name: []const u8, fields: []const []const u8) !rt.TypeId {
        const nameId = try cy.sema.ensureNameSym(self.compiler, name);
        const key = sema.ResolvedSymKey{
            .resolvedSymKey = .{
                .parentSymId = self.mod().resolvedRootSymId,
                .nameId = nameId,
            },
        };
        const modId = try cy.module.declareTypeObject(self.compiler, self.modId, name, cy.NullId, cy.NullId);
        const res = try cy.sema.resolveObjectSym(self.compiler, key, modId);
        const sym = self.compiler.sema.getSymbol(res.sTypeId);
        const typeId = sym.inner.object.typeId;

        for (fields, 0..) |field, i| {
            const id = try self.vm.ensureFieldSym(field);
            try self.vm.addFieldSym(typeId, id, @intCast(i), bt.Any);
        }
        self.vm.types.buf[typeId].numFields = @intCast(fields.len);
        return typeId;
    }
};
