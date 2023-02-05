// Copyright (c) 2023 Cyber (See LICENSE)

const std = @import("std");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const builtin = @import("builtin");

const cy = @import("../cyber.zig");
const Value = cy.Value;
const vm_ = @import("../vm.zig");
const TrackGlobalRC = vm_.TrackGlobalRC;
const gvm = &vm_.gvm;
const fmt = @import("../fmt.zig");

const debug = builtin.mode == .Debug;
const log = stdx.log.scoped(.bindings);

const NullId = std.math.maxInt(u32);

pub const TagLit = enum {
    int,
    bool,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    usize,
    f32,
    f64,
    float,
    double,
    charPtrZ,
    ptr,
    void,

    little,
    big,

    AssertError,
    FileNotFound,
    MissingSymbol,
    EndOfStream,
    OutOfBounds,
    InvalidArgument,
    InvalidSignature,
    InvalidChar,
    StreamTooLong,
    NotAllowed,
    UnknownError,

    running,
    paused,
    done,

    err,
    number,
    object,
    map,
    list,
    function,
    fiber,
    string,
    rawstring,
    none,
    symbol,

    // Open modes.
    read,
    write,
    readWrite,

    // File types.
    file,
    dir,

    unknown,
};

const StdSection = cy.StdSection;
const Section = cy.Section;

pub var CFuncT: cy.TypeId = undefined;
pub var CStructT: cy.TypeId = undefined;

pub fn bindCore(self: *cy.VM) linksection(cy.InitSection) !void {
    @setCold(true);

    self.iteratorObjSym = try self.ensureMethodSymKey("iterator", 0);
    self.nextObjSym = try self.ensureMethodSymKey("next", 0);
    self.pairIteratorObjSym = try self.ensureMethodSymKey("pairIterator", 0);
    self.nextPairObjSym = try self.ensureMethodSymKey("nextPair", 0);
    const add = try self.ensureMethodSymKey("add", 1);
    const append = try self.ensureMethodSymKey("append", 1);
    const byteAt = try self.ensureMethodSymKey("byteAt", 1);
    const charAt = try self.ensureMethodSymKey("charAt", 1);
    const codeAt = try self.ensureMethodSymKey("codeAt", 1);
    const concat = try self.ensureMethodSymKey("concat", 1);
    const endsWith = try self.ensureMethodSymKey("endsWith", 1);
    const index = try self.ensureMethodSymKey("index", 1);
    const indexChar = try self.ensureMethodSymKey("indexChar", 1);
    const indexCharSet = try self.ensureMethodSymKey("indexCharSet", 1);
    const indexCode = try self.ensureMethodSymKey("indexCode", 1);
    const insert = try self.ensureMethodSymKey("insert", 2);
    const insertByte = try self.ensureMethodSymKey("insertByte", 2);
    const isAscii = try self.ensureMethodSymKey("isAscii", 0);
    const joinString = try self.ensureMethodSymKey("joinString", 1);
    const len = try self.ensureMethodSymKey("len", 0);
    const less = try self.ensureMethodSymKey("less", 1);
    const lower = try self.ensureMethodSymKey("lower", 0);
    const read = try self.ensureMethodSymKey("read", 1);
    const readToEnd = try self.ensureMethodSymKey("readToEnd", 0);
    const remove = try self.ensureMethodSymKey("remove", 1);
    const repeat = try self.ensureMethodSymKey("repeat", 1);
    const replace = try self.ensureMethodSymKey("replace", 2);
    const resize = try self.ensureMethodSymKey("resize", 1);
    const seek = try self.ensureMethodSymKey("seek", 1);
    const seekFromCur = try self.ensureMethodSymKey("seekFromCur", 1);
    const seekFromEnd = try self.ensureMethodSymKey("seekFromEnd", 1);
    const size = try self.ensureMethodSymKey("size", 0);
    const slice = try self.ensureMethodSymKey("slice", 2);
    const sort = try self.ensureMethodSymKey("sort", 1);
    const startsWith = try self.ensureMethodSymKey("startsWith", 1);
    const stat = try self.ensureMethodSymKey("stat", 0);
    const status = try self.ensureMethodSymKey("status", 0);
    const streamLines = try self.ensureMethodSymKey("streamLines", 0);
    const streamLines1 = try self.ensureMethodSymKey("streamLines", 1);
    const toString = try self.ensureMethodSymKey("toString", 0);
    const upper = try self.ensureMethodSymKey("upper", 0);
    const utf8 = try self.ensureMethodSymKey("utf8", 0);
    const walk = try self.ensureMethodSymKey("walk", 0);
    const write = try self.ensureMethodSymKey("write", 1);
    
    // Init compile time builtins.

    // Primitive types.
    var id = try self.addStruct("none");
    std.debug.assert(id == cy.NoneT);
    id = try self.addStruct("boolean");
    std.debug.assert(id == cy.BooleanT);
    id = try self.addStruct("error");
    std.debug.assert(id == cy.ErrorT);

    id = try self.addStruct("string");
    std.debug.assert(id == cy.StaticAstringT);
    try self.addMethodSym(cy.StaticAstringT, append, cy.MethodSym.initNativeFunc1(stringAppend(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, index, cy.MethodSym.initNativeFunc1(stringIndex(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, len, cy.MethodSym.initNativeFunc1(stringLen(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, less, cy.MethodSym.initNativeFunc1(stringLess(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, lower, cy.MethodSym.initNativeFunc1(stringLower(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.staticAstring)));
    try self.addMethodSym(cy.StaticAstringT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.staticAstring)));

    id = try self.addStruct("string"); // Astring and Ustring share the same string user type.
    std.debug.assert(id == cy.StaticUstringT);
    try self.addMethodSym(cy.StaticUstringT, append, cy.MethodSym.initNativeFunc1(stringAppend(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, index, cy.MethodSym.initNativeFunc1(stringIndex(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, len, cy.MethodSym.initNativeFunc1(stringLen(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, less, cy.MethodSym.initNativeFunc1(stringLess(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, lower, cy.MethodSym.initNativeFunc1(stringLower(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.staticUstring)));
    try self.addMethodSym(cy.StaticUstringT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.staticUstring)));

    id = try self.addStruct("tag");
    std.debug.assert(id == cy.UserTagT);
    id = try self.addStruct("tagliteral");
    std.debug.assert(id == cy.UserTagLiteralT);
    id = try self.addStruct("integer");
    std.debug.assert(id == cy.IntegerT);
    id = try self.addStruct("number");
    std.debug.assert(id == cy.NumberT);

    id = try self.addStruct("List");
    std.debug.assert(id == cy.ListS);
    try self.addMethodSym(cy.ListS, add, cy.MethodSym.initNativeFunc1(listAdd));
    try self.addMethodSym(cy.ListS, append, cy.MethodSym.initNativeFunc1(listAppend));
    try self.addMethodSym(cy.ListS, concat, cy.MethodSym.initNativeFunc1(listConcat));
    try self.addMethodSym(cy.ListS, insert, cy.MethodSym.initNativeFunc1(listInsert));
    try self.addMethodSym(cy.ListS, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(listIterator));
    try self.addMethodSym(cy.ListS, joinString, cy.MethodSym.initNativeFunc1(listJoinString));
    try self.addMethodSym(cy.ListS, len, cy.MethodSym.initNativeFunc1(listLen));
    try self.addMethodSym(cy.ListS, self.pairIteratorObjSym, cy.MethodSym.initNativeFunc1(listIterator));
    try self.addMethodSym(cy.ListS, remove, cy.MethodSym.initNativeFunc1(listRemove));
    try self.addMethodSym(cy.ListS, resize, cy.MethodSym.initNativeFunc1(listResize));
    try self.addMethodSym(cy.ListS, sort, cy.MethodSym.initNativeFunc1(listSort));

    id = try self.addStruct("ListIterator");
    std.debug.assert(id == cy.ListIteratorT);
    try self.addMethodSym(cy.ListIteratorT, self.nextObjSym, cy.MethodSym.initNativeFunc1(listIteratorNext));
    try self.addMethodSym(cy.ListIteratorT, self.nextPairObjSym, cy.MethodSym.initNativeFunc2(listIteratorNextPair));

    id = try self.addStruct("Map");
    std.debug.assert(id == cy.MapS);
    try self.addMethodSym(cy.MapS, remove, cy.MethodSym.initNativeFunc1(mapRemove));
    try self.addMethodSym(cy.MapS, size, cy.MethodSym.initNativeFunc1(mapSize));
    try self.addMethodSym(cy.MapS, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(mapIterator));
    try self.addMethodSym(cy.MapS, self.pairIteratorObjSym, cy.MethodSym.initNativeFunc1(mapIterator));

    id = try self.addStruct("MapIterator");
    std.debug.assert(id == cy.MapIteratorT);
    try self.addMethodSym(cy.MapIteratorT, self.nextObjSym, cy.MethodSym.initNativeFunc1(mapIteratorNext));
    try self.addMethodSym(cy.MapIteratorT, self.nextPairObjSym, cy.MethodSym.initNativeFunc2(mapIteratorNextPair));

    id = try self.addStruct("Closure");
    std.debug.assert(id == cy.ClosureS);

    id = try self.addStruct("Lambda");
    std.debug.assert(id == cy.LambdaS);

    id = try self.addStruct("string");
    std.debug.assert(id == cy.AstringT);
    try self.addMethodSym(cy.AstringT, append, cy.MethodSym.initNativeFunc1(stringAppend(.astring)));
    try self.addMethodSym(cy.AstringT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.astring)));
    try self.addMethodSym(cy.AstringT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.astring)));
    try self.addMethodSym(cy.AstringT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.astring)));
    try self.addMethodSym(cy.AstringT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.astring)));
    try self.addMethodSym(cy.AstringT, index, cy.MethodSym.initNativeFunc1(stringIndex(.astring)));
    try self.addMethodSym(cy.AstringT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.astring)));
    try self.addMethodSym(cy.AstringT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.astring)));
    try self.addMethodSym(cy.AstringT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.astring)));
    try self.addMethodSym(cy.AstringT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.astring)));
    try self.addMethodSym(cy.AstringT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.astring)));
    try self.addMethodSym(cy.AstringT, len, cy.MethodSym.initNativeFunc1(stringLen(.astring)));
    try self.addMethodSym(cy.AstringT, less, cy.MethodSym.initNativeFunc1(stringLess(.astring)));
    try self.addMethodSym(cy.AstringT, lower, cy.MethodSym.initNativeFunc1(stringLower(.astring)));
    try self.addMethodSym(cy.AstringT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.astring)));
    try self.addMethodSym(cy.AstringT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.astring)));
    try self.addMethodSym(cy.AstringT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.astring)));
    try self.addMethodSym(cy.AstringT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.astring)));
    try self.addMethodSym(cy.AstringT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.astring)));

    id = try self.addStruct("string");
    std.debug.assert(id == cy.UstringT);
    try self.addMethodSym(cy.UstringT, append, cy.MethodSym.initNativeFunc1(stringAppend(.ustring)));
    try self.addMethodSym(cy.UstringT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.ustring)));
    try self.addMethodSym(cy.UstringT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.ustring)));
    try self.addMethodSym(cy.UstringT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.ustring)));
    try self.addMethodSym(cy.UstringT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.ustring)));
    try self.addMethodSym(cy.UstringT, index, cy.MethodSym.initNativeFunc1(stringIndex(.ustring)));
    try self.addMethodSym(cy.UstringT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.ustring)));
    try self.addMethodSym(cy.UstringT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.ustring)));
    try self.addMethodSym(cy.UstringT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.ustring)));
    try self.addMethodSym(cy.UstringT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.ustring)));
    try self.addMethodSym(cy.UstringT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.ustring)));
    try self.addMethodSym(cy.UstringT, len, cy.MethodSym.initNativeFunc1(stringLen(.ustring)));
    try self.addMethodSym(cy.UstringT, less, cy.MethodSym.initNativeFunc1(stringLess(.ustring)));
    try self.addMethodSym(cy.UstringT, lower, cy.MethodSym.initNativeFunc1(stringLower(.ustring)));
    try self.addMethodSym(cy.UstringT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.ustring)));
    try self.addMethodSym(cy.UstringT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.ustring)));
    try self.addMethodSym(cy.UstringT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.ustring)));
    try self.addMethodSym(cy.UstringT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.ustring)));
    try self.addMethodSym(cy.UstringT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.ustring)));

    id = try self.addStruct("string");
    std.debug.assert(id == cy.StringSliceT);
    try self.addMethodSym(cy.StringSliceT, append, cy.MethodSym.initNativeFunc1(stringAppend(.slice)));
    try self.addMethodSym(cy.StringSliceT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.slice)));
    try self.addMethodSym(cy.StringSliceT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.slice)));
    try self.addMethodSym(cy.StringSliceT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.slice)));
    try self.addMethodSym(cy.StringSliceT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.slice)));
    try self.addMethodSym(cy.StringSliceT, index, cy.MethodSym.initNativeFunc1(stringIndex(.slice)));
    try self.addMethodSym(cy.StringSliceT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.slice)));
    try self.addMethodSym(cy.StringSliceT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.slice)));
    try self.addMethodSym(cy.StringSliceT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.slice)));
    try self.addMethodSym(cy.StringSliceT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.slice)));
    try self.addMethodSym(cy.StringSliceT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.slice)));
    try self.addMethodSym(cy.StringSliceT, len, cy.MethodSym.initNativeFunc1(stringLen(.slice)));
    try self.addMethodSym(cy.StringSliceT, less, cy.MethodSym.initNativeFunc1(stringLess(.slice)));
    try self.addMethodSym(cy.StringSliceT, lower, cy.MethodSym.initNativeFunc1(stringLower(.slice)));
    try self.addMethodSym(cy.StringSliceT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.slice)));
    try self.addMethodSym(cy.StringSliceT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.slice)));
    try self.addMethodSym(cy.StringSliceT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.slice)));
    try self.addMethodSym(cy.StringSliceT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.slice)));
    try self.addMethodSym(cy.StringSliceT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.slice)));

    id = try self.addStruct("rawstring");
    std.debug.assert(id == cy.RawStringT);
    try self.addMethodSym(cy.RawStringT, append, cy.MethodSym.initNativeFunc1(stringAppend(.rawstring)));
    try self.addMethodSym(cy.RawStringT, byteAt, cy.MethodSym.initNativeFunc1(rawStringByteAt));
    try self.addMethodSym(cy.RawStringT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.rawstring)));
    try self.addMethodSym(cy.RawStringT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.rawstring)));
    try self.addMethodSym(cy.RawStringT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.rawstring)));
    try self.addMethodSym(cy.RawStringT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.rawstring)));
    try self.addMethodSym(cy.RawStringT, index, cy.MethodSym.initNativeFunc1(stringIndex(.rawstring)));
    try self.addMethodSym(cy.RawStringT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.rawstring)));
    try self.addMethodSym(cy.RawStringT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.rawstring)));
    try self.addMethodSym(cy.RawStringT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.rawstring)));
    try self.addMethodSym(cy.RawStringT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.rawstring)));
    try self.addMethodSym(cy.RawStringT, insertByte, cy.MethodSym.initNativeFunc1(rawStringInsertByte));
    try self.addMethodSym(cy.RawStringT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.rawstring)));
    try self.addMethodSym(cy.RawStringT, len, cy.MethodSym.initNativeFunc1(stringLen(.rawstring)));
    try self.addMethodSym(cy.RawStringT, less, cy.MethodSym.initNativeFunc1(stringLess(.rawstring)));
    try self.addMethodSym(cy.RawStringT, lower, cy.MethodSym.initNativeFunc1(stringLower(.rawstring)));
    try self.addMethodSym(cy.RawStringT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.rawstring)));
    try self.addMethodSym(cy.RawStringT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.rawstring)));
    try self.addMethodSym(cy.RawStringT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.rawstring)));
    try self.addMethodSym(cy.RawStringT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.rawstring)));
    try self.addMethodSym(cy.RawStringT, toString, cy.MethodSym.initNativeFunc1(rawStringToString));
    try self.addMethodSym(cy.RawStringT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.rawstring)));
    try self.addMethodSym(cy.RawStringT, utf8, cy.MethodSym.initNativeFunc1(rawStringUtf8));

    id = try self.addStruct("rawstring");
    std.debug.assert(id == cy.RawStringSliceT);
    try self.addMethodSym(cy.RawStringSliceT, append, cy.MethodSym.initNativeFunc1(stringAppend(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, byteAt, cy.MethodSym.initNativeFunc1(rawStringSliceByteAt));
    try self.addMethodSym(cy.RawStringSliceT, charAt, cy.MethodSym.initNativeFunc1(stringCharAt(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, codeAt, cy.MethodSym.initNativeFunc1(stringCodeAt(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, concat, cy.MethodSym.initNativeFunc1(stringConcat(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, endsWith, cy.MethodSym.initNativeFunc1(stringEndsWith(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, index, cy.MethodSym.initNativeFunc1(stringIndex(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, indexChar, cy.MethodSym.initNativeFunc1(stringIndexChar(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, indexCharSet, cy.MethodSym.initNativeFunc1(stringIndexCharSet(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, indexCode, cy.MethodSym.initNativeFunc1(stringIndexCode(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, insert, cy.MethodSym.initNativeFunc1(stringInsert(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, insertByte, cy.MethodSym.initNativeFunc1(rawStringSliceInsertByte));
    try self.addMethodSym(cy.RawStringSliceT, isAscii, cy.MethodSym.initNativeFunc1(stringIsAscii(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, len, cy.MethodSym.initNativeFunc1(stringLen(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, less, cy.MethodSym.initNativeFunc1(stringLess(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, lower, cy.MethodSym.initNativeFunc1(stringLower(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, repeat, cy.MethodSym.initNativeFunc1(stringRepeat(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, replace, cy.MethodSym.initNativeFunc1(stringReplace(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, slice, cy.MethodSym.initNativeFunc1(stringSlice(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, startsWith, cy.MethodSym.initNativeFunc1(stringStartsWith(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, toString, cy.MethodSym.initNativeFunc1(rawStringSliceToString));
    try self.addMethodSym(cy.RawStringSliceT, upper, cy.MethodSym.initNativeFunc1(stringUpper(.rawSlice)));
    try self.addMethodSym(cy.RawStringSliceT, utf8, cy.MethodSym.initNativeFunc1(rawStringSliceUtf8));

    id = try self.addStruct("Fiber");
    std.debug.assert(id == cy.FiberS);
    try self.addMethodSym(cy.FiberS, status, cy.MethodSym.initNativeFunc1(fiberStatus));

    id = try self.addStruct("Box");
    std.debug.assert(id == cy.BoxS);

    id = try self.addStruct("NativeFunc1");
    std.debug.assert(id == cy.NativeFunc1S);

    id = try self.addStruct("TccState");
    std.debug.assert(id == cy.TccStateS);

    id = try self.addStruct("OpaquePtr");
    std.debug.assert(id == cy.OpaquePtrS);

    id = try self.addStruct("File");
    std.debug.assert(id == cy.FileT);
    if (cy.hasStdFiles) {
        try self.addMethodSym(cy.FileT, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(fileIterator));
        try self.addMethodSym(cy.FileT, self.nextObjSym, cy.MethodSym.initNativeFunc1(fileNext));
        try self.addMethodSym(cy.FileT, read, cy.MethodSym.initNativeFunc1(fileRead));
        try self.addMethodSym(cy.FileT, readToEnd, cy.MethodSym.initNativeFunc1(fileReadToEnd));
        try self.addMethodSym(cy.FileT, seek, cy.MethodSym.initNativeFunc1(fileSeek));
        try self.addMethodSym(cy.FileT, seekFromCur, cy.MethodSym.initNativeFunc1(fileSeekFromCur));
        try self.addMethodSym(cy.FileT, seekFromEnd, cy.MethodSym.initNativeFunc1(fileSeekFromEnd));
        try self.addMethodSym(cy.FileT, stat, cy.MethodSym.initNativeFunc1(fileStat));
        try self.addMethodSym(cy.FileT, streamLines, cy.MethodSym.initNativeFunc1(fileStreamLines));
        try self.addMethodSym(cy.FileT, streamLines1, cy.MethodSym.initNativeFunc1(fileStreamLines1));
        try self.addMethodSym(cy.FileT, write, cy.MethodSym.initNativeFunc1(fileWrite));
    } else {
        try self.addMethodSym(cy.FileT, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.FileT, self.nextObjSym, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.FileT, read, cy.MethodSym.initNativeFunc1(objNop1));
        try self.addMethodSym(cy.FileT, readToEnd, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.FileT, seek, cy.MethodSym.initNativeFunc1(objNop1));
        try self.addMethodSym(cy.FileT, seekFromCur, cy.MethodSym.initNativeFunc1(objNop1));
        try self.addMethodSym(cy.FileT, seekFromEnd, cy.MethodSym.initNativeFunc1(objNop1));
        try self.addMethodSym(cy.FileT, stat, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.FileT, streamLines, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.FileT, streamLines1, cy.MethodSym.initNativeFunc1(objNop1));
        try self.addMethodSym(cy.FileT, write, cy.MethodSym.initNativeFunc1(objNop1));
    }

    id = try self.addStruct("Dir");
    std.debug.assert(id == cy.DirT);
    if (cy.hasStdFiles) {
        try self.addMethodSym(cy.DirT, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(dirIterator));
        try self.addMethodSym(cy.DirT, stat, cy.MethodSym.initNativeFunc1(fileStat));
        try self.addMethodSym(cy.DirT, walk, cy.MethodSym.initNativeFunc1(dirWalk));
    } else {
        try self.addMethodSym(cy.DirT, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.DirT, stat, cy.MethodSym.initNativeFunc1(objNop0));
        try self.addMethodSym(cy.DirT, walk, cy.MethodSym.initNativeFunc1(objNop0));
    }

    id = try self.addStruct("DirIterator");
    std.debug.assert(id == cy.DirIteratorT);
    if (cy.hasStdFiles) {
        try self.addMethodSym(cy.DirIteratorT, self.nextObjSym, cy.MethodSym.initNativeFunc1(dirIteratorNext));
    } else {
        try self.addMethodSym(cy.DirIteratorT, self.nextObjSym, cy.MethodSym.initNativeFunc1(objNop0));
    }

    id = try self.addStruct("Symbol");
    std.debug.assert(id == cy.SymbolT);

    CFuncT = try self.addStruct("CFunc");
    self.structs.buf[CFuncT].numFields = 3;
    id = try self.ensureFieldSym("sym");
    try self.addFieldSym(CFuncT, id, 0);
    id = try self.ensureFieldSym("args");
    try self.addFieldSym(CFuncT, id, 1);
    id = try self.ensureFieldSym("ret");
    try self.addFieldSym(CFuncT, id, 2);

    CStructT = try self.addStruct("CStruct");
    self.structs.buf[CStructT].numFields = 2;
    id = try self.ensureFieldSym("fields");
    try self.addFieldSym(CStructT, id, 0);
    id = try self.ensureFieldSym("type");
    try self.addFieldSym(CStructT, id, 1);

    try ensureTagLitSym(self, "int", .int);
    try ensureTagLitSym(self, "bool", .bool);
    try ensureTagLitSym(self, "i8", .i8);
    try ensureTagLitSym(self, "u8", .u8);
    try ensureTagLitSym(self, "i16", .i16);
    try ensureTagLitSym(self, "u16", .u16);
    try ensureTagLitSym(self, "i32", .i32);
    try ensureTagLitSym(self, "u32", .u32);
    try ensureTagLitSym(self, "i64", .i64);
    try ensureTagLitSym(self, "u64", .u64);
    try ensureTagLitSym(self, "usize", .usize);
    try ensureTagLitSym(self, "f32", .f32);
    try ensureTagLitSym(self, "f64", .f64);
    try ensureTagLitSym(self, "float", .float);
    try ensureTagLitSym(self, "double", .double);
    try ensureTagLitSym(self, "charPtrZ", .charPtrZ);
    try ensureTagLitSym(self, "ptr", .ptr);
    try ensureTagLitSym(self, "void", .void);

    try ensureTagLitSym(self, "little", .little);
    try ensureTagLitSym(self, "big", .big);

    try ensureTagLitSym(self, "AssertError", .AssertError);
    try ensureTagLitSym(self, "FileNotFound", .FileNotFound);
    try ensureTagLitSym(self, "MissingSymbol", .MissingSymbol);
    try ensureTagLitSym(self, "EndOfStream", .EndOfStream);
    try ensureTagLitSym(self, "OutOfBounds", .OutOfBounds);
    try ensureTagLitSym(self, "InvalidArgument", .InvalidArgument);
    try ensureTagLitSym(self, "InvalidSignature", .InvalidSignature);
    try ensureTagLitSym(self, "InvalidChar", .InvalidChar);
    try ensureTagLitSym(self, "SteamTooLong", .StreamTooLong);
    try ensureTagLitSym(self, "NotAllowed", .NotAllowed);
    try ensureTagLitSym(self, "UnknownError", .UnknownError);

    try ensureTagLitSym(self, "running", .running);
    try ensureTagLitSym(self, "paused", .paused);
    try ensureTagLitSym(self, "done", .done);

    try ensureTagLitSym(self, "error", .err);
    try ensureTagLitSym(self, "number", .number);
    try ensureTagLitSym(self, "object", .object);
    try ensureTagLitSym(self, "map", .map);
    try ensureTagLitSym(self, "list", .list);
    try ensureTagLitSym(self, "function", .function);
    try ensureTagLitSym(self, "fiber", .fiber);
    try ensureTagLitSym(self, "string", .string);
    try ensureTagLitSym(self, "rawstring", .rawstring);
    try ensureTagLitSym(self, "none", .none);
    try ensureTagLitSym(self, "symbol", .symbol);

    try ensureTagLitSym(self, "read", .read);
    try ensureTagLitSym(self, "write", .write);
    try ensureTagLitSym(self, "readWrite", .readWrite);

    try ensureTagLitSym(self, "file", .file);
    try ensureTagLitSym(self, "dir", .dir);

    try ensureTagLitSym(self, "unknown", .unknown);
}

fn ensureTagLitSym(vm: *cy.VM, name: []const u8, tag: TagLit) !void {
    const id = try vm.ensureTagLitSym(name);
    std.debug.assert(id == @enumToInt(tag));
}

fn listSort(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
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
        .newFramePtr = vm.getNewFramePtrOffset(args),
    };

    const S = struct {
        fn less(ctx_: *LessContext, a: Value, b: Value) bool {
            const res = ctx_.vm.callFunc(ctx_.newFramePtr, ctx_.lessFn, &.{a, b}) catch stdx.fatal();
            return res.toBool();
        }
    };
    std.sort.sort(Value, list.items(), &lessCtx, S.less);
    return Value.None;
}

fn listRemove(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index = @floatToInt(i64, args[0].toF64());
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(list);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index >= inner.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    vm.release(inner.buf[@intCast(usize, index)]);
    inner.remove(@intCast(usize, index));
    return Value.None;
}

fn listInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index = @floatToInt(i64, args[0].toF64());
    const value = args[1];
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(list);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (index < 0 or index > inner.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch stdx.fatal();
    inner.insertAssumeCapacity(@intCast(usize, index), value);
    return Value.None;
}

fn listAdd(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(cy.Section) Value {
    fmt.printDeprecated("list.add()", "0.1", "Use list.append() instead.", &.{});
    return listAppend(vm, ptr, args, nargs);
}

fn listAppend(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    obj.list.append(vm.allocator(), args[0]);
    vm.releaseObject(obj);
    return Value.None;
}

fn listJoinString(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const items = obj.list.items();
    if (items.len > 0) {
        var sepCharLen: u32 = undefined;
        const sep = vm.valueToTempString2(args[0], &sepCharLen);

        const alloc = vm.allocator();
        const tempSlices = &@ptrCast(*cy.VM, vm).u8Buf2;
        tempSlices.clearRetainingCapacity();
        const tempBuf = &@ptrCast(*cy.VM, vm).u8Buf;
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
        byteLen += @intCast(u32, str.len);

        // Record other string parts.
        for (items[1..]) |item| {
            str = vm.getOrWriteValueString(tempBufWriter, item, &charLen);
            tempSlices.appendSlice(alloc, std.mem.asBytes(&str)) catch fatal();
            charLenSum += charLen;
            byteLen += @intCast(u32, str.len);
        }
        charLenSum += @intCast(u32, sepCharLen * (items.len-1));
        byteLen += @intCast(u32, sep.len * (items.len-1));

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
        const slices = @ptrCast([*][]const u8, tempSlices.buf.ptr)[0..items.len];
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

fn listConcat(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    if (args[0].isList()) {
        const list = args[0].asHeapObject(*cy.HeapObject);
        for (list.list.items()) |it| {
            vm.retain(it);
            obj.list.append(vm.allocator(), it);
        }
    }
    return Value.None;
}

fn listIteratorNextPair(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) cy.ValuePair {
    _ = args;
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return .{
            .left = Value.initF64(@intToFloat(f64, obj.listIter.nextIdx)),
            .right = val,
        };
    } else return .{
        .left = Value.None,
        .right = Value.None,
    };
}

fn listIteratorNext(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    _ = args;
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const list = obj.listIter.list;
    if (obj.listIter.nextIdx < list.list.len) {
        defer obj.listIter.nextIdx += 1;
        const val = list.list.ptr[obj.listIter.nextIdx];
        vm.retain(val);
        return val;
    } else return Value.None;
}

fn listIterator(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    // Don't need to release recv since it's retained by the iterator.
    return vm.allocListIterator(&obj.list) catch fatal();
}

fn listResize(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    const size = @floatToInt(u32, args[0].toF64());
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

fn mapIterator(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    // Don't need to release recv since it's retained by the iterator.
    return vm.allocMapIterator(&obj.map) catch fatal();
}

fn mapIteratorNextPair(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(Section) cy.ValuePair {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const map = @ptrCast(*cy.ValueMap, &obj.mapIter.map.inner);
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

fn mapIteratorNext(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const map = @ptrCast(*cy.ValueMap, &obj.mapIter.map.inner);
    if (map.next(&obj.mapIter.nextIdx)) |entry| {
        vm.retain(entry.value);
        return entry.value;
    } else return Value.None;
}

fn mapSize(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    vm.releaseObject(obj);
    return Value.initF64(@intToFloat(f64, inner.size));
}

fn mapRemove(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.MapInner, &obj.map.inner);
    _ = inner.remove(@ptrCast(*cy.VM, vm), args[0]);
    vm.releaseObject(obj);
    return Value.None;
}

fn listLen(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    vm.releaseObject(list);
    return Value.initF64(@intToFloat(f64, inner.len));
}

// Keep as reference in case resume should be a function call.
// Although it works, it requires native func calls to perform additional copies of pc and framePtr back to the eval loop,
// which is a bad tradeoff for every other function call that doesn't need to.
// One solution is to add another bytecode to call nativeFunc1 with control over execution context.
// fn fiberResume(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.HotSection) Value {
//     const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
//     if (&obj.fiber != @ptrCast(*cy.VM, vm).curFiber) {
//         // Only resume fiber if it's not done.
//         if (obj.fiber.pc != NullId) {
//             // Obtain the startLocal from looking at previous inst operand.
//             const startLocal = (@ptrCast(*cy.VM, vm).pc - 14 + 1)[0].arg;
//             // Obtain previous framePtr by subtracting from args pointer.
//             const prevFramePtr = @intToPtr([*]Value, @ptrToInt(args - startLocal - 4));

//             const pcOffset = @intCast(u32, @ptrToInt(@ptrCast(*cy.VM, vm).pc) - @ptrToInt(@ptrCast(*cy.VM, vm).ops.ptr));
//             const res = cy.pushFiber(@ptrCast(*cy.VM, vm), pcOffset, prevFramePtr, &obj.fiber, startLocal);
//             @ptrCast(*cy.VM, vm).pc = res.pc;
//             @ptrCast(*cy.VM, vm).framePtr = res.framePtr;
//             return Value.None;
//         }
//     }
//     vm.releaseObject(obj);
//     return Value.None;
// }

fn fiberStatus(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    if (gvm.curFiber == @ptrCast(*cy.Fiber, obj)) {
        return Value.initTagLiteral(@enumToInt(TagLit.running));
    } else {
        // Check if done.
        if (obj.fiber.pc == NullId) {
            return Value.initTagLiteral(@enumToInt(TagLit.done));
        } else {
            return Value.initTagLiteral(@enumToInt(TagLit.paused));
        }
    }
}

pub fn stringUpper(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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

pub fn stringLower(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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

pub fn stringLess(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            if (isRawStringObject(T)) {
                var right: []const u8 = undefined;
                if (args[0].isRawString()) {
                    right = args[0].asHeapObject(*cy.HeapObject).rawstring.getConstSlice();
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

pub fn stringLen(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
            }
            if (isAstringObject(T, obj)) {
                if (T == .astring) {
                    return Value.initF64(@intToFloat(f64, obj.astring.len));
                } else if (T == .staticAstring) {
                    return Value.initF64(@intToFloat(f64, obj.len()));
                } else if (T == .slice) {
                    return Value.initF64(@intToFloat(f64, obj.stringSlice.len));
                } else fatal();
            } else if (isUstringObject(T, obj)) {
                return Value.initF64(@intToFloat(f64, getStringCharLen(T, vm, obj)));
            } else if (T == .rawstring) {
                return Value.initF64(@intToFloat(f64, obj.rawstring.len));
            } else if (T == .rawSlice) {
                return Value.initF64(@intToFloat(f64, obj.rawstringSlice.len));
            } else fatal();
        }
    };
    return S.inner;
}

pub fn stringCharAt(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const idx = @floatToInt(i32, args[0].toF64());

            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx >= str.len) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const uidx = @intCast(u32, idx);
                if (T == .staticAstring) {
                    return Value.initStaticAstring(obj.start + uidx, 1);
                } else {
                    // TODO: return slice.
                    return vm.allocAstring(str[uidx..uidx + 1]) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                if (idx < 0 or idx >= getStringCharLen(T, vm, obj)) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const uidx = @intCast(u32, idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start = @intCast(u32, cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
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
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const uidx = @intCast(u32, idx);
                if (cy.utf8CharSliceAt(str, uidx)) |slice| {
                    if (slice.len == 1) {
                        return vm.allocAstring(slice) catch fatal();
                    } else {
                        return vm.allocUstring(slice, 1) catch fatal();
                    }
                } else {
                    return Value.initErrorTagLit(@enumToInt(TagLit.InvalidChar));
                }
            } else fatal();
        }
    };
    return S.inner;
}

fn stringCodeAt(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const idx = @floatToInt(i32, args[0].toF64());
            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx >= str.len) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                return Value.initF64(@intToFloat(f64, str[@intCast(u32, idx)]));
            } else if (isUstringObject(T, obj)) {
                if (idx < 0 or idx >= getStringCharLen(T, vm, obj)) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const uidx = @intCast(u32, idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start = @intCast(u32, cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));
                const slice = cy.string.utf8CharSliceAtNoCheck(str, start);

                const cp = std.unicode.utf8Decode(slice) catch stdx.fatal();
                setUstringMruChar(T, vm, obj, uidx, start);
                return Value.initF64(@intToFloat(f64, cp));
            } else if (isRawStringObject(T)) {
                if (idx < 0 or idx >= str.len) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const uidx = @intCast(u32, idx);
                if (cy.utf8CharSliceAt(str, uidx)) |slice| {
                    const cp = std.unicode.utf8Decode(slice) catch stdx.fatal();
                    return Value.initF64(@intToFloat(f64, cp));
                } else {
                    return Value.initErrorTagLit(@enumToInt(TagLit.InvalidChar));
                }
            } else fatal();
        }
    };
    return S.inner;
}

fn rawStringInsertByteCommon(vm: *cy.UserVM, str: []const u8, indexv: Value, val: Value) Value {
    const index = @floatToInt(i64, indexv.toF64());
    if (index < 0 or index > str.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    const byte = @floatToInt(u8, val.toF64());
    const new = vm.allocUnsetRawStringObject(str.len + 1) catch stdx.fatal();
    const buf = new.rawstring.getSlice();
    const uidx = @intCast(u32, index);
    std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
    buf[uidx] = byte;
    std.mem.copy(u8, buf[uidx+1..], str[uidx..]);
    return Value.initPtr(new);
}

fn rawStringInsertByte(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const str = obj.rawstring.getConstSlice();
    return rawStringInsertByteCommon(vm, str, args[0], args[1]);
}

fn rawStringSliceInsertByte(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const str = obj.rawstringSlice.getConstSlice();
    return rawStringInsertByteCommon(vm, str, args[0], args[1]);
}

fn ustringReplaceCommon(vm: *cy.UserVM, str: []const u8, needlev: Value, replacev: Value) linksection(cy.StdSection) ?Value {
    var ncharLen: u32 = undefined;
    const needle = vm.valueToTempString2(needlev, &ncharLen);
    var rcharLen: u32 = undefined;
    const replacement = vm.valueToNextTempString2(replacev, &rcharLen);

    const idxBuf = &@ptrCast(*cy.VM, vm).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        const new = vm.allocUnsetUstringObject(newLen, @intCast(u32, str.len + idxBuf.len * rcharLen - idxBuf.len * ncharLen)) catch fatal();
        const newBuf = new.ustring.getSlice();
        const idxes = @ptrCast([*]const u32, idxBuf.buf.ptr)[0..numIdxes];
        cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxes);
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

const NativeFunc = fn (vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) Value;

fn stringRepeat(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                if (isHeapString(T)) {
                    vm.releaseObject(obj);
                }
            }
            const str = getStringSlice(T, vm, obj);

            const n = @floatToInt(i32, args[0].toF64());
            if (n < 0) {
                return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
            }

            var un = @intCast(u32, n);
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
                    dst += @intCast(u32, str.len);
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
                        return @intToPtr(*Value, @ptrToInt(ptr)).*;
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

inline fn getStringObject(comptime T: cy.StringType, ptr: *anyopaque) StringObject(T) {
    switch (StringObject(T)) {
        *cy.HeapObject => {
            return stdx.ptrAlignCast(*cy.HeapObject, ptr);
        },
        stdx.IndexSlice(u32) => {
            const val = @intToPtr(*Value, @ptrToInt(ptr)).*;
            return val.asStaticStringSlice();
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

fn stringReplace(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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
                        return @intToPtr(*Value, @ptrToInt(ptr)).*;
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
                        return @intToPtr(*Value, @ptrToInt(ptr)).*;
                    }
                }
            } else if (isRawStringObject(T)) {
                const needle = vm.valueToTempString(args[0]);
                const replacement = vm.valueToNextTempString(args[1]);

                const idxBuf = &@ptrCast(*cy.VM, vm).u8Buf;
                idxBuf.clearRetainingCapacity();
                defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
                const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
                const numIdxes = @divExact(idxBuf.len, 4);
                if (numIdxes > 0) {
                    const new = vm.allocUnsetRawStringObject(newLen) catch fatal();
                    const newBuf = new.rawstring.getSlice();
                    const idxes = @ptrCast([*]const u32, idxBuf.buf.ptr)[0..numIdxes];
                    cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxes);
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
    const idxBuf = &@ptrCast(*cy.VM, vm).u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.allocator(), 4096) catch fatal();
    const newLen = cy.prepReplacement(str, needle, replacement, idxBuf.writer(vm.allocator())) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        if (rcharLen == replacement.len) {
            const new = vm.allocUnsetAstringObject(newLen) catch fatal();
            const newBuf = new.astring.getSlice();
            const idxes = @ptrCast([*]const u32, idxBuf.buf.ptr)[0..numIdxes];
            cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxes);
            return vm.allocOwnedAstring(new) catch fatal();
        } else {
            const new = vm.allocUnsetUstringObject(newLen, @intCast(u32, str.len + idxBuf.len * rcharLen - idxBuf.len * needle.len)) catch fatal();
            const newBuf = new.ustring.getSlice();
            const idxes = @ptrCast([*]const u32, idxBuf.buf.ptr)[0..numIdxes];
            cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxes);
            return vm.allocOwnedUstring(new) catch fatal();
        }
    } else {
        return null;
    }
}

pub fn stringSlice(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
                vm.release(args[1]);
            }
            const str = getStringSlice(T, vm, obj);

            if (isAstringObject(T, obj)) {
                var start = @floatToInt(i32, args[0].toF64());
                if (start < 0) {
                    start = @intCast(i32, str.len) + start;
                }
                var end = if (args[1].isNone()) @intCast(i32, str.len) else @floatToInt(i32, args[1].toF64());
                if (end < 0) {
                    end = @intCast(i32, str.len) + end;
                }
                if (start < 0 or end > str.len or end < start) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const ustart = @intCast(u32, start);
                const uend = @intCast(u32, end);

                if (T == .staticAstring) {
                    return Value.initStaticAstring(obj.start + ustart, @intCast(u15, uend - ustart));
                } else {
                    vm.retainObject(obj);
                    return vm.allocAstringSlice(str[ustart..uend], obj) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                const charLen = getStringCharLen(T, vm, obj);
                var start = @floatToInt(i32, args[0].toF64());
                if (start < 0) {
                    start = @intCast(i32, charLen) + start;
                }
                var end = if (args[1].isNone()) @intCast(i32, charLen) else @floatToInt(i32, args[1].toF64());
                if (end < 0) {
                    end = @intCast(i32, charLen) + end;
                }
                if (start < 0 or end > charLen or end < start) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                const ustart = @intCast(u32, start);
                const uend = @intCast(u32, end);

                const mru = getUstringMruChar(T, vm, obj);
                const startByteIdx = @intCast(u32, cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, ustart));
                setUstringMruChar(T, vm, obj, ustart, startByteIdx);
                const endByteIdx = @intCast(u32, cy.string.ustringSeekByCharIndex(str, startByteIdx, ustart, uend));

                if (T == .staticUstring) {
                    return vm.allocUstringSlice(str[startByteIdx..endByteIdx], uend - ustart, null) catch fatal();
                } else {
                    vm.retainObject(obj);
                    return vm.allocUstringSlice(str[startByteIdx..endByteIdx], uend - ustart, obj) catch fatal();
                }
            } else if (isRawStringObject(T)) {
                var start = @floatToInt(i32, args[0].toF64());
                if (start < 0) {
                    start = @intCast(i32, str.len) + start;
                }
                var end = if (args[1].isNone()) @intCast(i32, str.len) else @floatToInt(i32, args[1].toF64());
                if (end < 0) {
                    end = @intCast(i32, str.len) + end;
                }
                if (start < 0 or end > str.len or end < start) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }

                vm.retainObject(obj);
                return vm.allocRawStringSlice(str[@intCast(u32, start)..@intCast(u32, end)], obj) catch fatal();
            } else fatal();
        }
    };
    return S.inner;
}

fn stringAppend(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
            fmt.printDeprecated("string.append()", "0.1", "Use string.concat() instead.", &.{});
            return stringConcat(T)(vm, ptr, args, nargs);
        }
    };
    return S.inner;
}

fn stringConcat(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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
                    return vm.allocUstringConcat(str, rstr, @intCast(u32, str.len + rcharLen)) catch fatal();
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

fn stringInsert(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
                vm.release(args[1]);
            }
            const str = getStringSlice(T, vm, obj);
            const idx = @floatToInt(i32, args[0].toF64());
            if (isAstringObject(T, obj)) {
                if (idx < 0 or idx > str.len) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                var insertCharLen: u32 = undefined;
                const insert = vm.valueToTempString2(args[1], &insertCharLen);
                const uidx = @intCast(u32, idx);
                if (insertCharLen == insert.len) {
                    return vm.allocAstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
                } else {
                    return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..], @intCast(u32, str.len + insertCharLen)) catch fatal();
                }
            } else if (isUstringObject(T, obj)) {
                const charLen = getStringCharLen(T, vm, obj);
                if (idx < 0 or idx > charLen) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                }
                var insertCharLen: u32 = undefined;
                const insert = vm.valueToTempString2(args[1], &insertCharLen);
                const uidx = @intCast(u32, idx);
                const mru = getUstringMruChar(T, vm, obj);
                const start = @intCast(u32, cy.string.ustringSeekByCharIndex(str, mru.byteIdx, mru.charIdx, uidx));

                setUstringMruChar(T, vm, obj, uidx, start);
                return vm.allocUstringConcat3(str[0..start], insert, str[start..], @intCast(u32, charLen + insertCharLen)) catch fatal();
            } else if (isRawStringObject(T)) {
                if (idx < 0 or idx > str.len) {
                    return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
                } 
                const insert = vm.valueToTempString(args[1]);
                const new = vm.allocUnsetRawStringObject(str.len + insert.len) catch stdx.fatal();
                const buf = new.rawstring.getSlice();
                const uidx = @intCast(u32, idx);
                std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
                std.mem.copy(u8, buf[uidx..uidx+insert.len], insert);
                std.mem.copy(u8, buf[uidx+insert.len..], str[uidx..]);
                return Value.initPtr(new);
            } else fatal();
        }
    };
    return S.inner;
}

fn stringIndex(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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
                            return Value.initF64(@intToFloat(f64, charIdx));
                        } else {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    }
                }
                if (cy.string.indexOf(str, needle)) |idx| {
                    if (isUstringObject(T, obj)) {
                        const charIdx = cy.toUtf8CharIdx(str, idx);
                        return Value.initF64(@intToFloat(f64, charIdx));
                    } else {
                        return Value.initF64(@intToFloat(f64, idx));
                    }
                }
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringStartsWith(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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

fn stringEndsWith(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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

fn rawStringSliceToString(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("rawstring.toString()", "0.1", "Use rawstring.utf8() instead.", &.{});
    return rawStringSliceUtf8(vm, ptr, args, nargs);
}

fn rawStringSliceUtf8(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const str = obj.rawstringSlice.getConstSlice();
    if (cy.validateUtf8(str)) |size| {
        if (size == str.len) {
            return vm.allocAstring(str) catch fatal();
        } else {
            return vm.allocUstring(str, @intCast(u32, size)) catch fatal();
        }
    } else {
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidChar));
    }
}

fn rawStringToString(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, nargs: u8) linksection(cy.StdSection) Value {
    fmt.printDeprecated("rawstring.toString()", "0.1", "Use rawstring.utf8() instead.", &.{});
    return rawStringUtf8(vm, ptr, args, nargs);
}

fn rawStringUtf8(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const str = obj.rawstring.getConstSlice();
    if (cy.validateUtf8(str)) |size| {
        if (size == str.len) {
            return vm.allocAstring(str) catch fatal();
        } else {
            return vm.allocUstring(str, @intCast(u32, size)) catch fatal();
        }
    } else {
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidChar));
    }
}

fn rawStringSliceByteAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= obj.rawstringSlice.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const str = obj.rawstringSlice.getConstSlice();
    const uidx = @intCast(u32, idx);
    return Value.initF64(@intToFloat(f64, str[uidx]));
}

fn rawStringByteAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= obj.rawstring.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const str = obj.rawstring.getConstSlice();
    const uidx = @intCast(u32, idx);
    return Value.initF64(@intToFloat(f64, str[uidx]));
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
       return @floatToInt(u8, val.toF64());
    }
}

fn stringIsAscii(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            if (T == .staticAstring) {
                return Value.True;
            } else if (T == .staticUstring) {
                return Value.False;
            } else {
                const obj = getStringObject(T, ptr);
                defer {
                    releaseStringObject(T, vm, obj);
                }
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

fn stringIndexCharSet(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
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
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    } else {
                        // Filter ascii codepoints.
                        const alloc = vm.allocator();
                        const tempBuf = &@ptrCast(*cy.VM, vm).u8Buf;
                        tempBuf.clearRetainingCapacity();
                        defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();
                        var iter = std.unicode.Utf8Iterator{
                            .bytes = set,
                            .i = 0,
                        };
                        while (iter.nextCodepoint()) |cp| {
                            if (cp < 128) {
                                tempBuf.append(alloc, @intCast(u8, cp)) catch fatal();
                            }
                        }
                        if (tempBuf.len > 0) {
                            if (cy.indexOfAsciiSet(str, tempBuf.items())) |idx| {
                                return Value.initF64(@intToFloat(f64, idx));
                            }
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (setIsAscii) {
                        if (cy.indexOfAsciiSet(str, set)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@intToFloat(f64, charIdx));
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
                                    minIndex = @intCast(u32, idx);
                                }
                            }
                        }
                        if (minIndex != NullId) {
                            return Value.initF64(@intToFloat(f64, minIndex));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (setIsAscii) {
                        if (cy.indexOfAsciiSet(str, set)) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
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
                                    minIndex = @intCast(u32, idx);
                                }
                            }
                        }
                        if (minIndex != NullId) {
                            return Value.initF64(@intToFloat(f64, minIndex));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringIndexCode(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const needle = @floatToInt(i32, args[0].toF64());

            if (needle > 0) {
                const code = @intCast(u21, needle);
                const needleIsAscii = code < 128;
                if (isAstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, @intCast(u8, code))) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, @intCast(u8, code))) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@intToFloat(f64, charIdx));
                        }
                    } else {
                        var slice: [4]u8 = undefined;
                        _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                        if (cy.string.indexOf(str, &slice)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@intToFloat(f64, charIdx));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (needleIsAscii) {
                        if (cy.indexOfChar(str, @intCast(u8, code))) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    } else {
                        var slice: [4]u8 = undefined;
                        _ = std.unicode.utf8Encode(code, &slice) catch fatal();
                        if (cy.string.indexOf(str, &slice)) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

fn stringIndexChar(comptime T: cy.StringType) NativeFunc {
    const S = struct {
        fn inner(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
            const obj = getStringObject(T, ptr);
            defer {
                releaseStringObject(T, vm, obj);
                vm.release(args[0]);
            }
            const str = getStringSlice(T, vm, obj);
            const needle = vm.valueToTempString(args[0]);
            const needleIsAscii = needle[0] & 0x80 == 0;

            if (needle.len > 0) {
                if (isAstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, needle[0])) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    }
                } else if (isUstringObject(T, obj)) {
                    if (needleIsAscii) {
                        if (cy.string.indexOfChar(str, needle[0])) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@intToFloat(f64, charIdx));
                        }
                    } else {
                        const slice = cy.string.utf8CharSliceAtNoCheck(needle, 0);
                        if (cy.string.indexOf(str, slice)) |idx| {
                            const charIdx = cy.toUtf8CharIdx(str, idx);
                            return Value.initF64(@intToFloat(f64, charIdx));
                        }
                    }
                } else if (isRawStringObject(T)) {
                    if (needleIsAscii) {
                        if (cy.indexOfChar(str, needle[0])) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    } else {
                        const slice = cy.string.utf8CharSliceAtNoCheck(needle, 0);
                        if (cy.string.indexOf(str, slice)) |idx| {
                            return Value.initF64(@intToFloat(f64, idx));
                        }
                    }
                } else fatal();
            }
            return Value.None;
        }
    };
    return S.inner;
}

pub fn fileStreamLines(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, nargs: u8) linksection(StdSection) Value {
    return fileStreamLines1(vm, ptr, &[_]Value{ Value.initF64(@intToFloat(f64, 4096)) }, nargs);
}

pub fn fileStreamLines1(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const bufSize = @floatToInt(u32, args[0].toF64());
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
        obj.file.readBufCap = @intCast(u32, readBuf.len);
        obj.file.hasReadBuf = true;
    }
    return Value.initPtr(ptr);
}

pub fn dirWalk(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    if (obj.dir.iterable) {
        vm.retainObject(obj);
        return vm.allocDirIterator(@ptrCast(*cy.Dir, obj), true) catch fatal();
    } else {
        return Value.initErrorTagLit(@enumToInt(TagLit.NotAllowed));
    }
}

pub fn dirIterator(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    if (obj.dir.iterable) {
        vm.retainObject(obj);
        return vm.allocDirIterator(@ptrCast(*cy.Dir, obj), false) catch fatal();
    } else {
        return Value.initErrorTagLit(@enumToInt(TagLit.NotAllowed));
    }
}

pub fn fileIterator(_: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    obj.file.curPos = 0;
    obj.file.readBufEnd = 0;
    return Value.initPtr(ptr);
}

pub fn fileSeekFromEnd(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    const numBytes = @floatToInt(i32, args[0].toF64());
    if (numBytes > 0) {
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
    }

    const file = obj.file.getStdFile();
    file.seekFromEnd(numBytes) catch |err| {
        fmt.printStderr("seekFromEnd {}", &.{fmt.v(err)});
        return Value.None;
    };
    return Value.None;
}

pub fn fileSeekFromCur(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    const numBytes = @floatToInt(i32, args[0].toF64());

    const file = obj.file.getStdFile();
    file.seekBy(numBytes) catch |err| {
        fmt.printStderr("seekFromCur {}", &.{fmt.v(err)});
        return Value.None;
    };
    return Value.None;
}

pub fn fileSeek(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    const numBytes = @floatToInt(i32, args[0].toF64());
    if (numBytes < 0) {
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
    }

    const file = obj.file.getStdFile();
    const unumBytes = @intCast(u32, numBytes);
    file.seekTo(unumBytes) catch |err| {
        fmt.printStderr("seek {}", &.{fmt.v(err)});
        return Value.None;
    };
    return Value.None;
}

pub fn fileWrite(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }

    var buf: []const u8 = undefined;
    if (args[0].isRawString()) {
        buf = args[0].asHeapObject(*cy.HeapObject).rawstring.getConstSlice();
    } else {
        buf = vm.valueToTempString(args[0]);
    }

    const file = obj.file.getStdFile();
    const numWritten = file.write(buf) catch |err| {
        fmt.printStderr("read {}", &.{fmt.v(err)});
        return Value.None;
    };

    return Value.initF64(@intToFloat(f64, numWritten));
}

pub fn fileRead(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    const numBytes = @floatToInt(i32, args[0].toF64());
    if (numBytes <= 0) {
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidArgument));
    }
    const unumBytes = @intCast(u32, numBytes);
    const file = obj.file.getStdFile();

    const alloc = vm.allocator();
    const tempBuf = &@ptrCast(*cy.VM, vm).u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();
    tempBuf.ensureTotalCapacityPrecise(alloc, unumBytes) catch fatal();

    const numRead = file.read(tempBuf.buf[0..unumBytes]) catch |err| {
        fmt.printStderr("read {}", &.{fmt.v(err)});
        return Value.None;
    };
    // Can return empty string when numRead == 0.
    return vm.allocRawString(tempBuf.buf[0..numRead]) catch fatal();
}

pub fn fileReadToEnd(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const file = obj.file.getStdFile();

    const alloc = vm.allocator();
    const tempBuf = &@ptrCast(*cy.VM, vm).u8Buf;
    tempBuf.clearRetainingCapacity();
    defer tempBuf.ensureMaxCapOrClear(alloc, 4096) catch fatal();

    const MinReadBufSize = 4096;
    tempBuf.ensureTotalCapacity(alloc, MinReadBufSize) catch fatal();

    while (true) {
        const buf = tempBuf.buf[tempBuf.len .. tempBuf.buf.len];
        const numRead = file.readAll(buf) catch |err| {
            fmt.printStderr("readToEnd {}", &.{fmt.v(err)});
            return Value.None;
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

pub fn fileStat(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const file = obj.file.getStdFile();
    const stat = file.stat() catch |err| {
        fmt.printStderr("stat {}", &.{fmt.v(err)});
        return Value.None;
    };

    const map = vm.allocEmptyMap() catch fatal();
    const sizeKey = vm.allocAstring("size") catch fatal();
    gvm.setIndex(map, sizeKey, Value.initF64(@intToFloat(f64, stat.size))) catch fatal();
    const modeKey = vm.allocAstring("mode") catch fatal();
    gvm.setIndex(map, modeKey, Value.initF64(@intToFloat(f64, stat.mode))) catch fatal();
    const typeKey = vm.allocAstring("type") catch fatal();
    const typeTag: TagLit = switch (stat.kind) {
        .File => .file,
        .Directory => .dir,
        else => .unknown,
    };
    gvm.setIndex(map, typeKey, Value.initTagLiteral(@enumToInt(typeTag))) catch fatal();
    const atimeKey = vm.allocAstring("atime") catch fatal();
    gvm.setIndex(map, atimeKey, Value.initF64(@intToFloat(f64, @divTrunc(stat.atime, 1000000)))) catch fatal();
    const ctimeKey = vm.allocAstring("ctime") catch fatal();
    gvm.setIndex(map, ctimeKey, Value.initF64(@intToFloat(f64, @divTrunc(stat.ctime, 1000000)))) catch fatal();
    const mtimeKey = vm.allocAstring("mtime") catch fatal();
    gvm.setIndex(map, mtimeKey, Value.initF64(@intToFloat(f64, @divTrunc(stat.mtime, 1000000)))) catch fatal();
    return map;
}

pub fn dirIteratorNext(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);

    const iter = @ptrCast(*cy.DirIterator, obj);
    if (iter.recursive) {
        const walker = stdx.ptrAlignCast(*std.fs.IterableDir.Walker, &iter.inner.walker);
        const entryOpt = walker.next() catch |err| {
            fmt.printStderr("next {}", &.{fmt.v(err)});
            return Value.initErrorTagLit(@enumToInt(TagLit.UnknownError));
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch fatal();
            const pathKey = vm.allocAstring("path") catch fatal();
            gvm.setIndex(map, pathKey, vm.allocRawString(entry.path) catch fatal()) catch fatal();
            const nameKey = vm.allocAstring("name") catch fatal();
            gvm.setIndex(map, nameKey, vm.allocRawString(entry.basename) catch fatal()) catch fatal();
            const typeKey = vm.allocAstring("type") catch fatal();
            const typeTag: TagLit = switch (entry.kind) {
                .File => .file,
                .Directory => .dir,
                else => .unknown,
            };
            gvm.setIndex(map, typeKey, Value.initTagLiteral(@enumToInt(typeTag))) catch fatal();
            return map;
        } else {
            return Value.None;
        }
    } else {
        const stdIter = stdx.ptrAlignCast(*std.fs.IterableDir.Iterator, &iter.inner.iter);
        const entryOpt = stdIter.next() catch |err| {
            fmt.printStderr("next {}", &.{fmt.v(err)});
            return Value.initErrorTagLit(@enumToInt(TagLit.UnknownError));
        };
        if (entryOpt) |entry| {
            const map = vm.allocEmptyMap() catch fatal();
            const nameKey = vm.allocAstring("name") catch fatal();
            gvm.setIndex(map, nameKey, vm.allocRawString(entry.name) catch fatal()) catch fatal();
            const typeKey = vm.allocAstring("type") catch fatal();
            const typeTag: TagLit = switch (entry.kind) {
                .File => .file,
                .Directory => .dir,
                else => .unknown,
            };
            gvm.setIndex(map, typeKey, Value.initTagLiteral(@enumToInt(typeTag))) catch fatal();
            return map;
        } else {
            return Value.None;
        }
    }
}

pub fn fileNext(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    if (obj.file.iterLines) {
        const alloc = vm.allocator();
        const readBuf = obj.file.readBuf[0..obj.file.readBufCap];
        if (cy.getLineEnd(readBuf[obj.file.curPos..obj.file.readBufEnd])) |end| {
            // Found new line.
            const line = vm.allocRawString(readBuf[obj.file.curPos..obj.file.curPos+end]) catch stdx.fatal();

            // Advance pos.
            obj.file.curPos += @intCast(u32, end);

            return line;
        }

        var lineBuf = cy.HeapRawStringBuilder.init(@ptrCast(*cy.VM, vm)) catch fatal();
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
                obj.file.curPos = @intCast(u32, end);
                obj.file.readBufEnd = @intCast(u32, bytesRead);

                return Value.initPtr(lineBuf.ownObject(alloc));
            } else {
                lineBuf.appendString(alloc, readBuf[0..bytesRead]) catch stdx.fatal();

                // Advance pos.
                obj.file.curPos = @intCast(u32, bytesRead);
                obj.file.readBufEnd = @intCast(u32, bytesRead);
            }
        }
    } else {
        return Value.None;
    }
}

pub fn nop0(_: *cy.UserVM, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.None;
}

pub fn nop1(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    vm.release(args[0]);
    return Value.None;
}

pub fn nop2(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    vm.release(args[0]);
    vm.release(args[1]);
    return Value.None;
}

pub fn nop3(vm: *cy.UserVM, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    vm.release(args[0]);
    vm.release(args[1]);
    vm.release(args[2]);
    return Value.None;
}

pub fn objNop0(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    vm.releaseObject(obj);
    return Value.None;
}

pub fn objNop1(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    vm.releaseObject(obj);
    vm.release(args[0]);
    return Value.None;
}