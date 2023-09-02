// Copyright (c) 2023 Cyber (See LICENSE)

const std = @import("std");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const builtin = @import("builtin");

const cy = @import("../cyber.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const sema = cy.sema;
const bt = cy.types.BuiltinTypeSymIds;
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const fmt = @import("../fmt.zig");

const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.bindings);

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

pub fn bindCore(self: *cy.VM) linksection(cy.InitSection) !void {
    @setCold(true);

    const b = ModuleBuilder.init(&self.compiler, undefined);

    // Builtin methods.
    const add = try b.ensureMethodGroup("add");
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
    const joinString = try b.ensureMethodGroup("joinString");
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
    const resize = try b.ensureMethodGroup("resize");
    const runeAt = try b.ensureMethodGroup("runeAt");
    const seek = try b.ensureMethodGroup("seek");
    const seekFromCur = try b.ensureMethodGroup("seekFromCur");
    const seekFromEnd = try b.ensureMethodGroup("seekFromEnd");
    const size = try b.ensureMethodGroup("size");
    const slice = try b.ensureMethodGroup("slice");
    const sliceAt = try b.ensureMethodGroup("sliceAt");
    const sort = try b.ensureMethodGroup("sort");
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
    var rsym = self.compiler.sema.getResolvedSym(bt.Boolean);
    var sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("<call>", &.{ bt.Any }, bt.Boolean, booleanCall);

    id = try self.addBuiltinType("error", bt.Error);
    std.debug.assert(id == rt.ErrorT);
    rsym = self.compiler.sema.getResolvedSym(bt.Error);
    sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("<call>", &.{ bt.Any }, bt.Error, errorCall);
    try b.addMethod(rt.ErrorT, value, &.{ bt.Any }, bt.Any, errorValue);

    id = try self.addBuiltinType("StaticAstring", bt.String);
    std.debug.assert(id == rt.StaticAstringT);

    // string type module.
    rsym = self.compiler.sema.getResolvedSym(bt.String);
    sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("<call>", &.{ bt.Any }, bt.String, stringCall);

    id = try self.addBuiltinType("StaticUstring", bt.String);
    std.debug.assert(id == rt.StaticUstringT);

    id = try self.addBuiltinType("enum", cy.NullId);
    std.debug.assert(id == rt.EnumT);

    id = try self.addBuiltinType("symbol", bt.Symbol);
    std.debug.assert(id == rt.SymbolT);

    id = try self.addBuiltinType("integer", bt.Integer);
    std.debug.assert(id == rt.IntegerT);
    rsym = self.compiler.sema.getResolvedSym(bt.Integer);
    sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("<call>", &.{ bt.Any }, bt.Integer, integerCall);

    id = try self.addBuiltinType("float", bt.Float);
    std.debug.assert(id == rt.FloatT);
    rsym = self.compiler.sema.getResolvedSym(bt.Float);
    sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("<call>", &.{ bt.Any }, bt.Number, numberCall);

    id = try self.addBuiltinType("List", bt.List);
    std.debug.assert(id == rt.ListT);

    try b.addMethod(rt.ListT, add, &.{bt.Any, bt.Any}, bt.None, listAdd);
    try b.addMethod(rt.ListT, append, &.{bt.Any, bt.Any}, bt.None, listAppend);
    try b.addMethod(rt.ListT, concat, &.{bt.Any, bt.List}, bt.None, listConcat);
    try b.addMethod(rt.ListT, insert, &.{bt.Any, bt.Float, bt.Any}, bt.Any, listInsert);
    try b.addMethod(rt.ListT, self.iteratorMGID, &.{bt.Any}, bt.Any, listIterator);
    try b.addMethod(rt.ListT, joinString, &.{bt.Any, bt.Any}, bt.String, listJoinString);
    try b.addMethod(rt.ListT, len, &.{bt.Any}, bt.Float, listLen);
    try b.addMethod(rt.ListT, self.pairIteratorMGID, &.{bt.Any}, bt.Any, listIterator);
    try b.addMethod(rt.ListT, remove, &.{bt.Any, bt.Float}, bt.Any, listRemove);
    try b.addMethod(rt.ListT, resize, &.{bt.Any, bt.Float}, bt.Any, listResize);
    try b.addMethod(rt.ListT, sort, &.{bt.Any, bt.Any }, bt.Any, listSort);

    id = try self.addBuiltinType("ListIterator", cy.NullId);
    std.debug.assert(id == rt.ListIteratorT);
    try b.addMethod(rt.ListIteratorT, self.nextMGID, &.{bt.Any}, bt.Any, listIteratorNext);
    try b.addMethod2(rt.ListIteratorT, self.nextPairMGID, &.{bt.Any}, bt.Any, listIteratorNextPair);

    id = try self.addBuiltinType("Map", bt.Map);
    std.debug.assert(id == rt.MapT);
    try b.addMethod(rt.MapT, remove,                  &.{ bt.Any, bt.Any }, bt.None, mapRemove);
    try b.addMethod(rt.MapT, size,                    &.{ bt.Any }, bt.Float, mapSize);
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
        try b.addMethod(typeId, charAt,       &.{ bt.Any, bt.Float }, bt.Any, stringCharAt(tag));
        try b.addMethod(typeId, codeAt,       &.{ bt.Any, bt.Float }, bt.Any, stringCodeAt(tag));
        try b.addMethod(typeId, concat,       &.{ bt.Any, bt.Any }, bt.String, stringConcat(tag));
        try b.addMethod(typeId, endsWith,     &.{ bt.Any, bt.Any }, bt.Boolean, stringEndsWith(tag));
        try b.addMethod(typeId, find,         &.{ bt.Any, bt.Any }, bt.Any, stringFind(tag));
        try b.addMethod(typeId, findAnyRune,  &.{ bt.Any, bt.Any }, bt.Any, stringFindAnyRune(tag));
        try b.addMethod(typeId, findRune,     &.{ bt.Any, bt.Float }, bt.Any, stringFindRune(tag));
        try b.addMethod(typeId, index,        &.{ bt.Any, bt.Any }, bt.Any, stringIndex(tag));
        try b.addMethod(typeId, indexChar,    &.{ bt.Any, bt.Any }, bt.Any, stringIndexChar(tag));
        try b.addMethod(typeId, indexCharSet, &.{ bt.Any, bt.Any }, bt.Any, stringIndexCharSet(tag));
        try b.addMethod(typeId, indexCode,    &.{ bt.Any, bt.Float }, bt.Any, stringIndexCode(tag));
        try b.addMethod(typeId, insert,       &.{ bt.Any, bt.Float, bt.Any }, bt.String, stringInsert(tag));
        try b.addMethod(typeId, isAscii,      &.{ bt.Any }, bt.Boolean, stringIsAscii(tag));
        try b.addMethod(typeId, len,          &.{ bt.Any }, bt.Float, stringLen(tag));
        try b.addMethod(typeId, less,         &.{ bt.Any, bt.Any }, bt.Boolean, stringLess(tag));
        try b.addMethod(typeId, lower,        &.{ bt.Any }, bt.String, stringLower(tag));
        try b.addMethod(typeId, replace,      &.{ bt.Any, bt.Any, bt.Any }, bt.String, stringReplace(tag));
        try b.addMethod(typeId, repeat,       &.{ bt.Any, bt.Float }, bt.Any, stringRepeat(tag));
        try b.addMethod(typeId, runeAt,       &.{ bt.Any, bt.Float }, bt.Any, stringRuneAt(tag));
        try b.addMethod(typeId, slice,        &.{ bt.Any, bt.Float, bt.Float }, bt.Any, stringSlice(tag));
        try b.addMethod(typeId, sliceAt,      &.{ bt.Any, bt.Float }, bt.Any, stringSliceAt(tag));
        try b.addMethod(typeId, split,        &.{ bt.Any, bt.Any }, bt.List, stringSplit(tag));
        try b.addMethod(typeId, startsWith,   &.{ bt.Any, bt.Any }, bt.Boolean, stringStartsWith(tag));
        try b.addMethod(typeId, trim,         &.{ bt.Any, bt.Symbol, bt.Any }, bt.Any, stringTrim(tag));
        try b.addMethod(typeId, upper,        &.{ bt.Any }, bt.String, stringUpper(tag));
    }

    id = try self.addBuiltinType("Rawstring", bt.Rawstring);
    std.debug.assert(id == rt.RawstringT);

    // rawstring type module.
    rsym = self.compiler.sema.getResolvedSym(bt.Rawstring);
    sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
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
        try b.addMethod(typeId, byteAt,       &.{ bt.Any, bt.Float }, bt.Any,
            if (tag == .rawstring) rawStringByteAt else rawStringSliceByteAt);
        try b.addMethod(typeId, charAt,       &.{ bt.Any, bt.Float }, bt.Any, stringCharAt(tag));
        try b.addMethod(typeId, codeAt,       &.{ bt.Any, bt.Float }, bt.Any, stringCodeAt(tag));
        try b.addMethod(typeId, concat,       &.{ bt.Any, bt.Any }, bt.Rawstring, stringConcat(tag));
        try b.addMethod(typeId, endsWith,     &.{ bt.Any, bt.Any }, bt.Boolean, stringEndsWith(tag));
        try b.addMethod(typeId, find,         &.{ bt.Any, bt.Any }, bt.Any, stringFind(tag));
        try b.addMethod(typeId, findAnyRune,  &.{ bt.Any, bt.Any }, bt.Any, stringFindAnyRune(tag));
        try b.addMethod(typeId, findRune,     &.{ bt.Any, bt.Float }, bt.Any, stringFindRune(tag));
        try b.addMethod(typeId, index,        &.{ bt.Any, bt.Any }, bt.Any, stringIndex(tag));
        try b.addMethod(typeId, indexChar,    &.{ bt.Any, bt.Any }, bt.Any, stringIndexChar(tag));
        try b.addMethod(typeId, indexCharSet, &.{ bt.Any, bt.Any }, bt.Any, stringIndexCharSet(tag));
        try b.addMethod(typeId, indexCode,    &.{ bt.Any, bt.Float }, bt.Any, stringIndexCode(tag));
        try b.addMethod(typeId, insert,       &.{ bt.Any, bt.Float, bt.Any }, bt.Any, stringInsert(tag));
        try b.addMethod(typeId, insertByte,   &.{ bt.Any, bt.Float, bt.Float }, bt.Any,
            if (tag == .rawstring) rawStringInsertByte else rawStringSliceInsertByte);
        try b.addMethod(typeId, isAscii,      &.{ bt.Any }, bt.Boolean, stringIsAscii(tag));
        try b.addMethod(typeId, len,          &.{ bt.Any }, bt.Float, stringLen(tag));
        try b.addMethod(typeId, less,         &.{ bt.Any, bt.Any }, bt.Boolean, stringLess(tag));
        try b.addMethod(typeId, lower,        &.{ bt.Any }, bt.Rawstring, stringLower(tag));
        try b.addMethod(typeId, repeat,       &.{ bt.Any, bt.Float }, bt.Any, stringRepeat(tag));
        try b.addMethod(typeId, replace,      &.{ bt.Any, bt.Any, bt.Any }, bt.Rawstring, stringReplace(tag));
        try b.addMethod(typeId, runeAt,       &.{ bt.Any, bt.Float }, bt.Any, stringRuneAt(tag));
        try b.addMethod(typeId, slice,        &.{ bt.Any, bt.Float, bt.Float }, bt.Any, stringSlice(tag));
        try b.addMethod(typeId, sliceAt,      &.{ bt.Any, bt.Float }, bt.Any, stringSliceAt(tag));
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
    rsym = self.compiler.sema.getResolvedSym(bt.Pointer);
    sb = ModuleBuilder.init(&self.compiler, rsym.inner.builtinType.modId);
    try sb.setFunc("<call>", &.{ bt.Any }, bt.Pointer, pointerCall);
    try b.addMethod(rt.PointerT, value, &.{ bt.Any }, bt.Number, pointerValue);

    id = try self.addBuiltinType("File", bt.File);
    std.debug.assert(id == rt.FileT);
    if (cy.hasStdFiles) {
        try b.addMethod(rt.FileT, close,               &.{ bt.Any }, bt.None, fileClose);
        try b.addMethod(rt.FileT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, fileIterator);
        try b.addMethod(rt.FileT, self.nextMGID,       &.{ bt.Any }, bt.Any, fileNext);
        try b.addMethod(rt.FileT, read,                &.{ bt.Any, bt.Float }, bt.Any, fileRead);
        try b.addMethod(rt.FileT, readToEnd,           &.{ bt.Any }, bt.Any, fileReadToEnd);
        try b.addMethod(rt.FileT, seek,                &.{ bt.Any, bt.Float }, bt.Any, fileSeek);
        try b.addMethod(rt.FileT, seekFromCur,         &.{ bt.Any, bt.Float }, bt.Any, fileSeekFromCur);
        try b.addMethod(rt.FileT, seekFromEnd,         &.{ bt.Any, bt.Float }, bt.Any, fileSeekFromEnd);
        try b.addMethod(rt.FileT, stat,                &.{ bt.Any }, bt.Any, fileOrDirStat);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any }, bt.Any, fileStreamLines);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any, bt.Float }, bt.Any, fileStreamLines1);
        try b.addMethod(rt.FileT, write,               &.{ bt.Any, bt.Any }, bt.Any, fileWrite);
    } else {
        try b.addMethod(rt.FileT, close,               &.{ bt.Any }, bt.None, objNop0);
        try b.addMethod(rt.FileT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, objNop0);
        try b.addMethod(rt.FileT, self.nextMGID,       &.{ bt.Any }, bt.Any, objNop0);
        try b.addMethod(rt.FileT, read,                &.{ bt.Any, bt.Float }, bt.Any, objNop1);
        try b.addMethod(rt.FileT, readToEnd,           &.{ bt.Any }, bt.Any, objNop1);
        try b.addMethod(rt.FileT, seek,                &.{ bt.Any, bt.Float }, bt.Any, objNop1);
        try b.addMethod(rt.FileT, seekFromCur,         &.{ bt.Any, bt.Float }, bt.Any, objNop1);
        try b.addMethod(rt.FileT, seekFromEnd,         &.{ bt.Any, bt.Float }, bt.Any, objNop1);
        try b.addMethod(rt.FileT, stat,                &.{ bt.Any }, bt.Any, objNop0);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any }, bt.Any, objNop0);
        try b.addMethod(rt.FileT, streamLines,         &.{ bt.Any, bt.Float }, bt.Any, objNop1);
        try b.addMethod(rt.FileT, write,               &.{ bt.Any, bt.Any }, bt.Any, objNop1);
    }

    id = try self.addBuiltinType("Dir", cy.NullId);
    std.debug.assert(id == rt.DirT);
    if (cy.hasStdFiles) {
        try b.addMethod(rt.DirT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, dirIterator);
        try b.addMethod(rt.DirT, stat,                &.{ bt.Any }, bt.Any, fileOrDirStat);
        try b.addMethod(rt.DirT, walk,                &.{ bt.Any }, bt.Any, dirWalk);
    } else {
        try b.addMethod(rt.DirT, self.iteratorMGID,   &.{ bt.Any }, bt.Any, objNop0);
        try b.addMethod(rt.DirT, stat,                &.{ bt.Any }, bt.Any, objNop0);
        try b.addMethod(rt.DirT, walk,                &.{ bt.Any }, bt.Any, objNop0);
    }

    id = try self.addBuiltinType("DirIterator", cy.NullId);
    std.debug.assert(id == rt.DirIteratorT);
    if (cy.hasStdFiles) {
        try b.addMethod(rt.DirIteratorT, self.nextMGID, &.{ bt.Any }, bt.Any, dirIteratorNext);
    } else {
        try b.addMethod(rt.DirIteratorT, self.nextMGID, &.{ bt.Any }, bt.Any, objNop0);
    }

    id = try self.addBuiltinType("MetaType", bt.MetaType);
    std.debug.assert(id == rt.MetaTypeT);
    try b.addMethod(rt.MetaTypeT, idSym, &.{ bt.Any }, bt.Float, metatypeId);

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

fn listSort(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    const compare = args[0];
    defer {
        vm.releaseObject(obj);
        vm.release(compare);
    }
    const list = stdx.ptrAlignCast(*cy.List(Value), &obj.list.list);
    const LessContext = struct {
        lessFn: Value,
        vm: *cy.UserVM,
        newFramePtr: u32,
    };
    var lessCtx = LessContext{
        .lessFn = args[0],
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

fn listRemove(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index: i64 = @intFromFloat(args[0].asF64());
    const list = recv.asHeapObject();
    defer vm.releaseObject(list);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index >= inner.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    vm.release(inner.buf[@intCast(index)]);
    inner.remove(@intCast(index));
    return Value.None;
}

fn listInsert(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index: i64 = @intFromFloat(args[0].asF64());
    const value = args[1];
    const list = recv.asHeapObject();
    defer vm.releaseObject(list);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch stdx.fatal();
    inner.insertAssumeCapacity(@intCast(index), value);
    return Value.None;
}

fn listAdd(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.Section) Value {
    fmt.printDeprecated("list.add()", "0.1", "Use list.append() instead.", &.{});
    return listAppend(vm, recv, args, nargs);
}

fn listAppend(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = recv.asHeapObject();
    obj.list.append(vm.allocator(), args[0]);
    vm.releaseObject(obj);
    return Value.None;
}

fn listJoinString(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const items = obj.list.items();
    if (items.len > 0) {
        var sepCharLen: u32 = undefined;
        const sep = vm.valueToTempString2(args[0], &sepCharLen);

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

fn listConcat(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const list = args[0].asHeapObject();
    for (list.list.items()) |it| {
        vm.retain(it);
        obj.list.append(vm.allocator(), it);
    }
    return Value.None;
}

fn listIteratorNextPair(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) cy.ValuePair {
    _ = args;
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return .{
            .left = Value.initF64(@floatFromInt(obj.listIter.nextIdx)),
            .right = val,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn listIteratorNext(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    _ = args;
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return val;
    } else return Value.None;
}

fn listIterator(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = recv.asHeapObject();
    // Don't need to release recv since it's retained by the iterator.
    return vm.allocListIterator(&obj.list) catch fatal();
}

fn listResize(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const list = recv.asHeapObject();
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    const size: u32 = @intFromFloat(args[0].asF64());
    if (inner.len < size) {
        const oldLen = inner.len;
        inner.resize(vm.allocator(), size) catch stdx.fatal();
        for (inner.items()[oldLen..size]) |*item| {
            item.* = Value.None;
        }
    } else if (inner.len > size) {
        // Remove items.
        for (inner.items()[size..inner.len]) |item| {
            vm.release(item);
        }
        inner.resize(vm.allocator(), size) catch stdx.fatal();
    }
    vm.releaseObject(list);
    return Value.None;
}

fn mapIterator(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(Section) Value {
    const obj = recv.asHeapObject();
    // Don't need to release recv since it's retained by the iterator.
    return vm.allocMapIterator(&obj.map) catch fatal();
}

fn mapIteratorNextPair(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(Section) cy.ValuePair {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
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

fn mapIteratorNext(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const map: *cy.ValueMap = @ptrCast(&obj.mapIter.map.inner);
    if (map.next(&obj.mapIter.nextIdx)) |entry| {
        vm.retain(entry.value);
        return entry.value;
    } else return Value.None;
}

fn mapSize(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = recv.asHeapObject();
    const inner = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    vm.releaseObject(obj);
    return Value.initF64(@floatFromInt(inner.size));
}

fn mapRemove(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const inner = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(@ptrCast(vm), args[0]);
    return Value.None;
}

fn listLen(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const list = recv.asHeapObject();
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    vm.releaseObject(list);
    return Value.initF64(@floatFromInt(inner.len));
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

fn errorValue(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) Value {
    const enumId = (recv.val & 0xFF00) >> 8;
    if (enumId == cy.NullU8) {
        return Value.initSymbol(recv.asErrorSymbol());
    } else {
        log.debug("TODO: error.value() for enums.", .{});
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
}

fn pointerValue(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    return Value.initF64(@floatFromInt(@intFromPtr(obj.pointer.ptr)));
}

fn fiberStatus(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) Value {
    const fiber = recv.asPointer(*vmc.Fiber);
    defer vm.releaseObject(stdx.ptrAlignCast(*cy.HeapObject, fiber));

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

pub fn stringUpper(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
            }
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

pub fn stringLower(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
            }
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

pub fn stringLess(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            if (isRawStringObject(T)) {
                var right: []const u8 = undefined;
                if (args[0].isRawString()) {
                    right = args[0].asRawString();
                } else {
                    right = vm.valueToTempString(args[0]);
                }
                return Value.initBool(std.mem.lessThan(u8, str, right));
            } else {
                const right = vm.valueToTempString(args[0]);
                return Value.initBool(std.mem.lessThan(u8, str, right));
            }
        }
    };
    return S.inner;
}

pub fn stringLen(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.Section) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
            }
            if (isAstringObject(T, obj)) {
                if (T == .astring) {
                    return Value.initF64(@floatFromInt(obj.astring.len));
                } else if (T == .staticAstring) {
                    return Value.initF64(@floatFromInt(obj.len()));
                } else if (T == .slice) {
                    return Value.initF64(@floatFromInt(obj.stringSlice.len));
                } else fatal();
            } else if (isUstringObject(T, obj)) {
                return Value.initF64(@floatFromInt(getStringCharLen(T, vm, obj)));
            } else if (T == .rawstring) {
                return Value.initF64(@floatFromInt(obj.rawstring.len));
            } else if (T == .rawSlice) {
                return Value.initF64(@floatFromInt(obj.rawstringSlice.len));
            } else fatal();
        }
    };
    return S.inner;
}

fn stringCharAt(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.charAt()", "0.2", "Use string.sliceAt() instead.", &.{});
            return @call(.never_inline, stringSliceAt(T), .{vm, recv, args, nargs});
        }
    };
    return S.inner;
}

pub fn stringSliceAt(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const idx: i32 = @intFromFloat(args[0].toF64());

            if (isAstringObject(T, obj)) {
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

pub fn stringCodeAt(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.codeAt()", "0.2", "Use string.runeAt() instead.", &.{});
            return @call(.never_inline, stringRuneAt(T), .{vm, recv, args, nargs});
        }
    };
    return S.inner;
}

fn stringRuneAt(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer releaseStringObject(T, vm, obj);
            const str = getStringSlice(T, vm, obj);
            const idx: i32 = @intFromFloat(args[0].asF64());
            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx >= str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                return Value.initF64(@floatFromInt(str[@intCast(idx)]));
            } else if (isUstringObject(T, obj)) {
                if (idx < 0 or idx >= getStringCharLen(T, vm, obj)) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
                const slice = cy.string.utf8CharSliceAtNoCheck(str, start);

                const cp = std.unicode.utf8Decode(slice) catch stdx.fatal();
                setUstringMruChar(T, vm, obj, uidx, start);
                return Value.initF64(@floatFromInt(cp));
            } else if (isRawStringObject(T)) {
                if (idx < 0 or idx >= str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                const uidx: u32 = @intCast(idx);
                if (cy.utf8CharSliceAt(str, uidx)) |slice| {
                    const cp = std.unicode.utf8Decode(slice) catch stdx.fatal();
                    return Value.initF64(@floatFromInt(cp));
                } else {
                    return prepareThrowSymbol(vm, .InvalidRune);
                }
            } else fatal();
        }
    };
    return S.inner;
}

fn rawStringInsertByteCommon(vm: *cy.UserVM, str: []const u8, indexv: Value, val: Value) Value {
    const index: i64 = @intFromFloat(indexv.asF64());
    if (index < 0 or index > str.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    } 
    const byte: u8 = @intFromFloat(val.asF64());
    const new = vm.allocUnsetRawStringObject(str.len + 1) catch stdx.fatal();
    const buf = new.rawstring.getSlice();
    const uidx: u32 = @intCast(index);
    std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
    buf[uidx] = byte;
    std.mem.copy(u8, buf[uidx+1..], str[uidx..]);
    return Value.initPtr(new);
}

fn rawStringInsertByte(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const str = obj.rawstring.getConstSlice();
    return rawStringInsertByteCommon(vm, str, args[0], args[1]);
}

fn rawStringSliceInsertByte(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const str = obj.rawstringSlice.getConstSlice();
    return rawStringInsertByteCommon(vm, str, args[0], args[1]);
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
    return T == .staticUstring or T == .ustring or (T == .slice and !obj.stringSlice.isAstring());
}

inline fn isAstringObject(comptime T: cy.StringType, obj: StringObject(T)) bool {
    return T == .staticAstring or T == .astring or (T == .slice and obj.stringSlice.isAstring());
}

fn stringRepeat(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                if (isHeapString(T)) {
                    vm.releaseObject(obj);
                }
            }
            const str = getStringSlice(T, vm, obj);

            const n: i32 = @intFromFloat(args[0].asF64());
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
                        return recv;
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
        .staticUstring => stdx.IndexSlice(u32),
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
        stdx.IndexSlice(u32) => {
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
            return obj.stringSlice.getConstSlice();
        },
        .rawstring => {
            return obj.rawstring.getConstSlice();
        },
        .rawSlice => {
            return obj.rawstringSlice.getConstSlice();
        },
    }
}

fn stringSplit(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const delim = vm.valueToTempString(args[0]);

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
                        if (obj.stringSlice.isAstring()) {
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

fn stringTrim(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[1]);
            }

            const str = getStringSlice(T, vm, obj);
            const trimRunes = vm.valueToTempString(args[1]);

            var res: []const u8 = undefined;
            const mode = getBuiltinSymbol(args[0].asSymbolId()) orelse {
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

fn stringReplace(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
                vm.release(args[1]);
            }
            const str = getStringSlice(T, vm, obj);
            if (isAstringObject(T, obj)) {
                if (astringReplaceCommon(vm, str, args[0], args[1])) |val| {
                    return val;
                } else {
                    if (T != .staticAstring) {
                        vm.retainObject(obj);
                        return Value.initPtr(obj);
                    } else {
                        return recv;
                    }
                }
            } else if (isUstringObject(T, obj)) {
                if (ustringReplaceCommon(vm, str, args[0], args[1])) |val| {
                    return val;
                } else {
                    if (T != .staticUstring) {
                        vm.retainObject(obj);
                        return Value.initPtr(obj);
                    } else {
                        return recv;
                    }
                }
            } else if (isRawStringObject(T)) {
                const needle = vm.valueToTempString(args[0]);
                const replacement = vm.valueToNextTempString(args[1]);

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

pub fn stringSlice(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer releaseStringObject(T, vm, obj);
            const str = getStringSlice(T, vm, obj);

            if (isAstringObject(T, obj)) {
                var start: i32 = @intFromFloat(args[0].asF64());
                if (start < 0) {
                    start = @as(i32, @intCast(str.len)) + start;
                }
                var end: i32 = if (args[1].isNone()) @intCast(str.len) else @intFromFloat(args[1].asF64());
                if (end < 0) {
                    end = @as(i32, @intCast(str.len)) + end;
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
                var start: i32 = @intFromFloat(args[0].asF64());
                if (start < 0) {
                    start = @as(i32, @intCast(charLen)) + start;
                }
                var end: i32 = if (args[1].isNone()) @intCast(charLen) else @intFromFloat(args[1].asF64());
                if (end < 0) {
                    end = @as(i32, @intCast(charLen)) + end;
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
                var start: i32 = @intFromFloat(args[0].asF64());
                if (start < 0) {
                    start = @as(i32, @intCast(str.len)) + start;
                }
                var end: i32 = if (args[1].isNone()) @intCast(str.len) else @intFromFloat(args[1].asF64());
                if (end < 0) {
                    end = @as(i32, @intCast(str.len)) + end;
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

fn stringAppend(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.append()", "0.1", "Use string.concat() instead.", &.{});
            return stringConcat(T)(vm, recv, args, nargs);
        }
    };
    return S.inner;
}

fn stringConcat(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            if (isAstringObject(T, obj)) {
                var rcharLen: u32 = undefined;
                const rstr = vm.valueToTempString2(args[0], &rcharLen);
                if (rcharLen == rstr.len) {
                    return vm.allocAstringConcat(str, rstr) catch fatal();
                } else {
                    return vm.allocUstringConcat(str, rstr, @intCast(str.len + rcharLen)) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                var rcharLen: u32 = undefined;
                const rstr = vm.valueToTempString2(args[0], &rcharLen);
                const charLen = getStringCharLen(T, vm, obj);
                return vm.allocUstringConcat(str, rstr, charLen + rcharLen) catch fatal();
            } else if (isRawStringObject(T)) {
                const rstr = vm.valueToTempString(args[0]);
                return vm.allocRawStringConcat(str, rstr) catch fatal();
            } else fatal();
        }
    };
    return S.inner;
}

fn stringInsert(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[1]);
            }
            const str = getStringSlice(T, vm, obj);
            const idx: i32 = @intFromFloat(args[0].asF64());
            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx > str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                }
                var insertCharLen: u32 = undefined;
                const insert = vm.valueToTempString2(args[1], &insertCharLen);
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
                const insert = vm.valueToTempString2(args[1], &insertCharLen);
                const uidx: u32 = @intCast(idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start: u32 = @intCast(cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));

                setUstringMruChar(T, vm, obj, uidx, start);
                return vm.allocUstringConcat3(str[0..start], insert, str[start..], @intCast(charLen + insertCharLen)) catch fatal();
            } else if (isRawStringObject(T)) {
                if (idx < 0 or idx > str.len) {
                    return prepareThrowSymbol(vm, .OutOfBounds);
                } 
                const insert = vm.valueToTempString(args[1]);
                const new = vm.allocUnsetRawStringObject(str.len + insert.len) catch stdx.fatal();
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

fn stringIndex(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.index()", "0.2", "Use string.find() instead.", &.{});
            return @call(.never_inline, stringFind(T), .{vm, recv, args, nargs});
        }
    };
    return S.inner;
}

fn stringFind(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[0]);
            if (needle.len > 0 and needle.len <= str.len) {
                if (needle.len == 1) {
                    // One ascii char special case. Perform indexOfChar.
                    if (cy.string.indexOfChar(str, needle[0])) |idx| {
                        if (isUstringObject(T, obj)) {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@floatFromInt(charIdx));
                        } else {
                            return Value.initF64(@floatFromInt(idx));
                        }
                    }
                }
                if (cy.string.indexOf(str, needle)) |idx| {
                    if (isUstringObject(T, obj)) {
                        const charIdx = cy.toUtf8CharIdx(str, idx);
                        return Value.initF64(@floatFromInt(charIdx));
                    } else {
                        return Value.initF64(@floatFromInt(idx));
                    }
                }
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringStartsWith(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[0]);
            return Value.initBool(std.mem.startsWith(u8, str, needle));
        }
    };
    return S.inner;
}

fn stringEndsWith(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[0]);
            return Value.initBool(std.mem.endsWith(u8, str, needle));
        }
    };
    return S.inner;
}

fn rawStringSliceToString(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("rawstring.toString()", "0.1", "Use rawstring.utf8() instead.", &.{});
    return rawStringSliceUtf8(vm, recv, args, nargs);
}

fn rawStringSliceUtf8(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
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

fn rawStringToString(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("rawstring.toString()", "0.1", "Use rawstring.utf8() instead.", &.{});
    return rawStringUtf8(vm, recv, args, nargs);
}

fn rawStringUtf8(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
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

fn rawStringSliceByteAt(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const idx: i32 = @intFromFloat(args[0].asF64());
    if (idx < 0 or idx >= obj.rawstringSlice.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }
    const str = obj.rawstringSlice.getConstSlice();
    const uidx: u32 = @intCast(idx);
    return Value.initF64(@floatFromInt(str[uidx]));
}

fn rawStringByteAt(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    const idx: i32 = @intFromFloat(args[0].asF64());
    if (idx < 0 or idx >= obj.rawstring.len) {
        return prepareThrowSymbol(vm, .OutOfBounds);
    }
    const str = obj.rawstring.getConstSlice();
    const uidx: u32 = @intCast(idx);
    return Value.initF64(@floatFromInt(str[uidx]));
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

fn stringIsAscii(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            if (T == .staticAstring) {
                return Value.True;
            } else if (T == .staticUstring) {
                return Value.False;
            } else {
                const obj = getStringObject(T, recv);
                defer releaseStringObject(T, vm, obj);
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

pub fn stringIndexCharSet(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.indexCharSet()", "0.2", "Use string.findAnyRune() instead.", &.{});
            return @call(.never_inline, stringFindAnyRune(T), .{vm, recv, args, nargs});
        }
    };
    return S.inner;
}

fn stringFindAnyRune(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            var setCharLen: u32 = undefined;
            const set = vm.valueToTempString2(args[0], &setCharLen);
            const setIsAscii = setCharLen == set.len;

            if (set.len > 0) {
                if (isAstringObject(T, obj)) {
                    if (setIsAscii) {
                        if (@call(.never_inline, cy.indexOfAsciiSet, .{str, set})) |idx| {
                            return Value.initF64(@floatFromInt(idx));
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
                                return Value.initF64(@floatFromInt(idx));
                            }
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (setIsAscii) {
                        if (cy.indexOfAsciiSet(str, set)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@floatFromInt(charIdx));
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
                            return Value.initF64(@floatFromInt(minIndex));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (setIsAscii) {
                        if (cy.indexOfAsciiSet(str, set)) |idx| {
                            return Value.initF64(@floatFromInt(idx));
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
                            return Value.initF64(@floatFromInt(minIndex));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

pub fn stringIndexCode(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.indexCode()", "0.2", "Use string.findRune() instead.", &.{});
            return @call(.never_inline, stringFindRune(T), .{vm, recv, args, nargs});
        }
    };
    return S.inner;
}

fn stringFindRune(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, recv);
            defer releaseStringObject(T, vm, obj);
            const str = getStringSlice(T, vm, obj);
            const needle: i32 = @intFromFloat(args[0].asF64());

            if (needle > 0) {
                const code: u21 = @intCast(needle);
                const needleIsAscii = code < 128;
                if (isAstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, @intCast(code))) |idx| {
                            return Value.initF64(@floatFromInt(idx));
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, @intCast(code))) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@floatFromInt(charIdx));
                        }
                    } else {
                        var slice: [4]u8 = undefined;
                        _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                        if (cy.string.indexOf(str, &slice)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@floatFromInt(charIdx));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (needleIsAscii) {
                        if (cy.indexOfChar(str, @intCast(code))) |idx| {
                            return Value.initF64(@floatFromInt(idx));
                        }
                    } else {
                        var slice: [4]u8 = undefined;
                        _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                        if (cy.string.indexOf(str, &slice)) |idx| {
                            return Value.initF64(@floatFromInt(idx));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringIndexChar(comptime T: cy.StringType) cy.NativeObjFuncPtr {
    const S = struct {
        fn inner(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.indexChar()", "0.2", "Use string.findRune() instead.", &.{});
            defer vm.release(args[0]);
            const needle = vm.valueToTempString(args[0]);
            if (needle.len > 0) {
                const cp = cy.string.utf8CodeAtNoCheck(needle, 0);
                return @call(.never_inline, stringFindRune(T), .{vm, recv, &[_]Value{Value.initF64(@floatFromInt(cp))}, 1});
            }
            return Value.None;
        }
    };
    return S.inner;
}

pub fn fileStreamLines(vm: *cy.UserVM, recv: Value, _: [*]const Value, nargs: u8) linksection(StdSection) Value {
    return fileStreamLines1(vm, recv, &[_]Value{ Value.initF64(@floatFromInt(4096)) }, nargs);
}

pub fn fileStreamLines1(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = recv.asHeapObject();
    const bufSize: u32 = @intFromFloat(args[0].asF64());
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
        const readBuf = vm.allocator().alloc(u8, bufSize) catch stdx.fatal();
        obj.file.readBuf = readBuf.ptr;
        obj.file.readBufCap = @intCast(readBuf.len);
        obj.file.hasReadBuf = true;
    }
    return recv;
}

pub fn dirWalk(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    if (obj.dir.iterable) {
        vm.retainObject(obj);
        return vm.allocDirIterator(@ptrCast(obj), true) catch fatal();
    } else {
        return prepareThrowSymbol(vm, .NotAllowed);
    }
}

pub fn dirIterator(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    if (obj.dir.iterable) {
        vm.retainObject(obj);
        return vm.allocDirIterator(@ptrCast(obj), false) catch fatal();
    } else {
        return prepareThrowSymbol(vm, .NotAllowed);
    }
}

pub fn fileIterator(_: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = recv.asHeapObject();
    obj.file.curPos = 0;
    obj.file.readBufEnd = 0;
    return recv;
}

pub fn fileSeekFromEnd(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes: i32 = @intFromFloat(args[0].asF64());
    if (numBytes > 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }

    const file = obj.file.getStdFile();
    file.seekFromEnd(numBytes) catch |err| {
        return fromUnsupportedError(vm, "seekFromEnd", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileSeekFromCur(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes: i32 = @intFromFloat(args[0].asF64());

    const file = obj.file.getStdFile();
    file.seekBy(numBytes) catch |err| {
        return fromUnsupportedError(vm, "seekFromCur", err, @errorReturnTrace());
    };
    return Value.None;
}

pub fn fileSeek(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes: i32 = @intFromFloat(args[0].asF64());
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

pub fn fileWrite(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    var buf = vm.valueToTempRawString(args[0]);
    const file = obj.file.getStdFile();
    const numWritten = file.write(buf) catch |err| {
        return fromUnsupportedError(vm, "write", err, @errorReturnTrace());
    };

    return Value.initF64(@floatFromInt(numWritten));
}

pub fn fileClose(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    obj.file.close();
    return Value.None;
}

pub fn fileRead(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

    if (obj.file.closed) {
        return prepareThrowSymbol(vm, .Closed);
    }

    const numBytes: i32 = @intFromFloat(args[0].asF64());
    if (numBytes <= 0) {
        return prepareThrowSymbol(vm, .InvalidArgument);
    }
    const unumBytes: u32 = @intCast(numBytes);
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

pub fn fileReadToEnd(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

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

pub fn fileOrDirStat(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

    if (obj.head.typeId == rt.FileT) {
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
    ivm.setIndex(map, sizeKey, Value.initF64(@floatFromInt(stat.size))) catch fatal();
    ivm.setIndex(map, modeKey, Value.initF64(@floatFromInt(stat.mode))) catch fatal();
    const typeTag: Symbol = switch (stat.kind) {
        .file => .file,
        .directory => .dir,
        else => .unknown,
    };
    ivm.setIndex(map, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch fatal();
    ivm.setIndex(map, atimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.atime, 1000000)))) catch fatal();
    ivm.setIndex(map, ctimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.ctime, 1000000)))) catch fatal();
    ivm.setIndex(map, mtimeKey, Value.initF64(@floatFromInt(@divTrunc(stat.mtime, 1000000)))) catch fatal();
    return map;
}

pub fn metatypeId(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    return Value.initF64(@floatFromInt(obj.metatype.symId));
}

pub fn dirIteratorNext(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);

    const ivm = vm.internal();
    const iter: *cy.DirIterator = @ptrCast(obj);
    if (iter.recursive) {
        const walker = stdx.ptrAlignCast(*std.fs.IterableDir.Walker, &iter.inner.walker);
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
            ivm.setIndex(map, pathKey, vm.allocRawString(entry.path) catch fatal()) catch fatal();
            ivm.setIndex(map, nameKey, vm.allocRawString(entry.basename) catch fatal()) catch fatal();
            const typeTag: Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            ivm.setIndex(map, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch fatal();
            return map;
        } else {
            return Value.None;
        }
    } else {
        const stdIter = stdx.ptrAlignCast(*std.fs.IterableDir.Iterator, &iter.inner.iter);
        const entryOpt = stdIter.next() catch |err| {
            return fromUnsupportedError(vm, "next", err, @errorReturnTrace());
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch fatal();
            const nameKey = vm.allocAstring("name") catch fatal();
            const typeKey = vm.allocAstring("type") catch fatal();
            defer {
                vm.release(nameKey);
                vm.release(typeKey);
            }
            ivm.setIndex(map, nameKey, vm.allocRawString(entry.name) catch fatal()) catch fatal();
            const typeTag: Symbol = switch (entry.kind) {
                .file => .file,
                .directory => .dir,
                else => .unknown,
            };
            ivm.setIndex(map, typeKey, Value.initSymbol(@intFromEnum(typeTag))) catch fatal();
            return map;
        } else {
            return Value.None;
        }
    }
}

pub fn fileNext(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    defer vm.releaseObject(obj);
    if (obj.file.iterLines) {
        const alloc = vm.allocator();
        const readBuf = obj.file.readBuf[0..obj.file.readBufCap];
        if (cy.getLineEnd(readBuf[obj.file.curPos..obj.file.readBufEnd])) |end| {
            // Found new line.
            const line = vm.allocRawString(readBuf[obj.file.curPos..obj.file.curPos+end]) catch stdx.fatal();

            // Advance pos.
            obj.file.curPos += @intCast(end);

            return line;
        }

        var lineBuf = cy.HeapRawStringBuilder.init(@ptrCast(vm)) catch fatal();
        defer lineBuf.deinit();
        // Start with previous string without line delimiter.
        lineBuf.appendString(alloc, readBuf[obj.file.curPos..obj.file.readBufEnd]) catch stdx.fatal();

        // Read into buffer.
        const file = obj.file.getStdFile();
        const reader = file.reader();

        while (true) {
            const bytesRead = reader.read(readBuf) catch stdx.fatal();
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
                lineBuf.appendString(alloc, readBuf[0..end]) catch stdx.fatal();

                // Advance pos.
                obj.file.curPos = @intCast(end);
                obj.file.readBufEnd = @intCast(bytesRead);

                return Value.initPtr(lineBuf.ownObject(alloc));
            } else {
                lineBuf.appendString(alloc, readBuf[0..bytesRead]) catch stdx.fatal();

                // Advance pos.
                obj.file.curPos = @intCast(bytesRead);
                obj.file.readBufEnd = @intCast(bytesRead);
            }
        }
    } else {
        return Value.None;
    }
}

pub fn booleanCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    defer vm.release(args[0]);
    return Value.initBool(args[0].toBool());
}

pub fn integerCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => {
            return Value.initI32(@intFromFloat(@trunc(val.asF64())));
        },
        .string => {
            var str = vm.valueToTempString(val);
            if (std.mem.indexOfScalar(u8, str, '.')) |idx| {
                str = str[0..idx];
            }
            const res = std.fmt.parseInt(i32, str, 10) catch {
                return Value.initI32(0);
            };
            return Value.initI32(res);
        },
        else => {
            return Value.initI32(0);
        }
    }
}

pub fn numberCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getUserTag()) {
        .float => return val,
        .string => {
            defer vm.release(val);
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

pub fn pointerCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isPointerT()) {
        return val;
    } else if (val.isFloat()) {
        const i: usize = @intFromFloat(val.asF64());
        return cy.heap.allocPointer(vm.internal(), @ptrFromInt(i)) catch fatal();
    } else {
        vm.release(val);
        return vm.returnPanic("Not a `pointer`.");
    }
}

pub fn rawstringCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const str = vm.valueToTempRawString(args[0]);
    defer vm.release(args[0]);
    return vm.allocRawString(str) catch stdx.fatal();
}

pub fn stringCall(vm: *cy.UserVM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isString()) {
        return val; 
    } else {
        defer vm.release(val);
        const str = vm.valueToTempString(val);
        return vm.allocStringInfer(str) catch stdx.fatal();
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

pub fn nop0(vm: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return vm.returnPanic("Unsupported.");
}

pub fn nop1(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    vm.release(args[0]);
    return vm.returnPanic("Unsupported.");
}

pub fn nop2(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    vm.release(args[0]);
    vm.release(args[1]);
    return vm.returnPanic("Unsupported.");
}

pub fn nop3(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    vm.release(args[0]);
    vm.release(args[1]);
    vm.release(args[2]);
    return vm.returnPanic("Unsupported.");
}

pub fn objNop0(vm: *cy.UserVM, recv: Value, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    vm.releaseObject(obj);
    return vm.returnPanic("Unsupported.");
}

pub fn objNop1(vm: *cy.UserVM, recv: Value, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = recv.asHeapObject();
    vm.releaseObject(obj);
    vm.release(args[0]);
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

    pub fn setVar(self: *const ModuleBuilder, name: []const u8, typeSymId: sema.ResolvedSymId, val: Value) !void {
        try self.mod().setTypedVar(self.compiler, name, typeSymId, val);
    }

    pub fn setFunc(self: *const ModuleBuilder, name: []const u8, params: []const sema.ResolvedSymId, ret: sema.ResolvedSymId, ptr: cy.NativeFuncPtr) !void {
        try self.mod().setNativeTypedFunc(self.compiler, name, params, ret, ptr);
    }

    pub fn ensureMethodGroup(self: *const ModuleBuilder, name: []const u8) !vmc.MethodGroupId {
        return self.vm.ensureMethodGroup(name);
    }

    pub fn addMethod(self: *const ModuleBuilder, typeId: rt.TypeId, mgId: vmc.MethodGroupId, params: []const sema.ResolvedSymId, ret: sema.ResolvedSymId, ptr: cy.NativeObjFuncPtr) !void {
        const funcSigId = try sema.ensureResolvedFuncSig(self.compiler, params, ret);
        const funcSig = self.compiler.sema.getResolvedFuncSig(funcSigId);
        if (funcSig.isTyped) {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initTypedNativeFunc(funcSigId, ptr, @intCast(params.len)));
        } else {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initUntypedNativeFunc1(funcSigId, ptr, @intCast(params.len)));
        }
    }

    pub fn addMethod2(self: *const ModuleBuilder, typeId: rt.TypeId, mgId: vmc.MethodGroupId, params: []const sema.ResolvedSymId, ret: sema.ResolvedSymId, ptr: cy.NativeObjFunc2Ptr) !void {
        const funcSigId = try sema.ensureResolvedFuncSig(self.compiler, params, ret);
        const funcSig = self.compiler.sema.getResolvedFuncSig(funcSigId);
        if (funcSig.isTyped) {
            return error.Unsupported;
        } else {
            try self.vm.addMethod(typeId, mgId, rt.MethodInit.initUntypedNativeFunc2(funcSigId, ptr, @intCast(params.len)));
        }
    }

    pub fn mod(self: *const ModuleBuilder) *cy.Module {
        return self.compiler.sema.getModulePtr(self.modId);
    }

    pub fn createAndSetTypeObject(self: *const ModuleBuilder, name: []const u8, fields: []const []const u8) !rt.TypeId {
        const nameId = try cy.sema.ensureNameSym(self.compiler, name);
        const key = sema.AbsResolvedSymKey{
            .absResolvedSymKey = .{
                .rParentSymId = self.mod().resolvedRootSymId,
                .nameId = nameId,
            },
        };
        const modId = try cy.module.declareTypeObject(self.compiler, self.modId, name, cy.NullId, cy.NullId);
        const symId = try cy.sema.resolveObjectSym(self.compiler, key, modId);
        const sym = self.compiler.sema.getResolvedSym(symId);
        const typeId = sym.inner.object.typeId;

        for (fields, 0..) |field, i| {
            const id = try self.vm.ensureFieldSym(field);
            try self.vm.addFieldSym(typeId, id, @intCast(i), bt.Any);
        }
        self.vm.types.buf[typeId].numFields = @intCast(fields.len);
        return typeId;
    }
};
