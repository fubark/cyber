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
    InvalidChar,
    StreamTooLong,

    running,
    paused,
    done,

    err,
    number,
    object,
    map,
};

const StdSection = cy.StdSection;
const Section = cy.Section;

pub export fn stdSection() linksection(cy.StdSection) callconv(.C) void {}
pub export fn section() linksection(cy.Section) callconv(.C) void {}
pub export fn hotSection() linksection(cy.HotSection) callconv(.C) void {}

/// Force the compiler to order linksection first on given function.
/// Use exported c function so release builds don't remove them.
pub fn forceSectionDep(_: *const fn() callconv(.C) void) void {} 

pub fn bindCore(self: *cy.VM) !void {
    @setCold(true);
    forceSectionDep(hotSection);
    forceSectionDep(section);
    forceSectionDep(stdSection);

    self.iteratorObjSym = try self.ensureMethodSymKey("iterator", 0);
    self.nextObjSym = try self.ensureMethodSymKey("next", 0);
    self.pairIteratorObjSym = try self.ensureMethodSymKey("pairIterator", 0);
    self.nextPairObjSym = try self.ensureMethodSymKey("nextPair", 0);
    const add = try self.ensureMethodSymKey("add", 1);
    const append = try self.ensureMethodSymKey("append", 1);
    const charAt = try self.ensureMethodSymKey("charAt", 1);
    const codeAt = try self.ensureMethodSymKey("codeAt", 1);
    const endsWith = try self.ensureMethodSymKey("endsWith", 1);
    const index = try self.ensureMethodSymKey("index", 1);
    const indexChar = try self.ensureMethodSymKey("indexChar", 1);
    const indexCode = try self.ensureMethodSymKey("indexCode", 1);
    const insert = try self.ensureMethodSymKey("insert", 2);
    const insertByte = try self.ensureMethodSymKey("insertByte", 2);
    const isAscii = try self.ensureMethodSymKey("isAscii", 0);
    const len = try self.ensureMethodSymKey("len", 0);
    const lower = try self.ensureMethodSymKey("lower", 0);
    const remove = try self.ensureMethodSymKey("remove", 1);
    const replace = try self.ensureMethodSymKey("replace", 2);
    const resize = try self.ensureMethodSymKey("resize", 1);
    const size = try self.ensureMethodSymKey("size", 0);
    const sort = try self.ensureMethodSymKey("sort", 1);
    const startsWith = try self.ensureMethodSymKey("startsWith", 1);
    const status = try self.ensureMethodSymKey("status", 0);
    const streamLines = try self.ensureMethodSymKey("streamLines", 0);
    const streamLines1 = try self.ensureMethodSymKey("streamLines", 1);
    const upper = try self.ensureMethodSymKey("upper", 0);

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
    try self.addMethodSym(cy.StaticAstringT, append, cy.MethodSym.initNativeFunc1(staticAstringAppend));
    try self.addMethodSym(cy.StaticAstringT, charAt, cy.MethodSym.initNativeFunc1(staticAstringCharAt));
    try self.addMethodSym(cy.StaticAstringT, codeAt, cy.MethodSym.initNativeFunc1(staticAstringCodeAt));
    try self.addMethodSym(cy.StaticAstringT, endsWith, cy.MethodSym.initNativeFunc1(staticStringEndsWith));
    try self.addMethodSym(cy.StaticAstringT, index, cy.MethodSym.initNativeFunc1(staticAstringIndex));
    try self.addMethodSym(cy.StaticAstringT, indexChar, cy.MethodSym.initNativeFunc1(staticAstringIndexChar));
    try self.addMethodSym(cy.StaticAstringT, indexCode, cy.MethodSym.initNativeFunc1(staticAstringIndexCode));
    try self.addMethodSym(cy.StaticAstringT, insert, cy.MethodSym.initNativeFunc1(staticAstringInsert));
    try self.addMethodSym(cy.StaticAstringT, isAscii, cy.MethodSym.initNativeFunc1(staticAstringIsAscii));
    try self.addMethodSym(cy.StaticAstringT, len, cy.MethodSym.initNativeFunc1(staticAstringLen));
    try self.addMethodSym(cy.StaticAstringT, lower, cy.MethodSym.initNativeFunc1(staticAstringLower));
    try self.addMethodSym(cy.StaticAstringT, replace, cy.MethodSym.initNativeFunc1(staticAstringReplace));
    try self.addMethodSym(cy.StaticAstringT, startsWith, cy.MethodSym.initNativeFunc1(staticStringStartsWith));
    try self.addMethodSym(cy.StaticAstringT, upper, cy.MethodSym.initNativeFunc1(staticAstringUpper));

    id = try self.addStruct("string"); // Astring and Ustring share the same string user type.
    std.debug.assert(id == cy.StaticUstringT);
    try self.addMethodSym(cy.StaticUstringT, append, cy.MethodSym.initNativeFunc1(staticUstringAppend));
    try self.addMethodSym(cy.StaticUstringT, charAt, cy.MethodSym.initNativeFunc1(staticUstringCharAt));
    try self.addMethodSym(cy.StaticUstringT, codeAt, cy.MethodSym.initNativeFunc1(staticUstringCodeAt));
    try self.addMethodSym(cy.StaticUstringT, endsWith, cy.MethodSym.initNativeFunc1(staticStringEndsWith));
    try self.addMethodSym(cy.StaticUstringT, index, cy.MethodSym.initNativeFunc1(staticUstringIndex));
    try self.addMethodSym(cy.StaticUstringT, indexChar, cy.MethodSym.initNativeFunc1(staticUstringIndexChar));
    try self.addMethodSym(cy.StaticUstringT, indexCode, cy.MethodSym.initNativeFunc1(staticUstringIndexCode));
    try self.addMethodSym(cy.StaticUstringT, insert, cy.MethodSym.initNativeFunc1(staticUstringInsert));
    try self.addMethodSym(cy.StaticUstringT, isAscii, cy.MethodSym.initNativeFunc1(staticUstringIsAscii));
    try self.addMethodSym(cy.StaticUstringT, len, cy.MethodSym.initNativeFunc1(staticUstringLen));
    try self.addMethodSym(cy.StaticUstringT, lower, cy.MethodSym.initNativeFunc1(staticUstringLower));
    try self.addMethodSym(cy.StaticUstringT, replace, cy.MethodSym.initNativeFunc1(staticUstringReplace));
    try self.addMethodSym(cy.StaticUstringT, startsWith, cy.MethodSym.initNativeFunc1(staticStringStartsWith));
    try self.addMethodSym(cy.StaticUstringT, upper, cy.MethodSym.initNativeFunc1(staticUstringUpper));

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
    try self.addMethodSym(cy.ListS, resize, cy.MethodSym.initNativeFunc1(listResize));
    try self.addMethodSym(cy.ListS, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(listIterator));
    try self.addMethodSym(cy.ListS, self.pairIteratorObjSym, cy.MethodSym.initNativeFunc1(listIterator));
    try self.addMethodSym(cy.ListS, add, cy.MethodSym.initNativeFunc1(listAdd));
    try self.addMethodSym(cy.ListS, insert, cy.MethodSym.initNativeFunc1(listInsert));
    try self.addMethodSym(cy.ListS, remove, cy.MethodSym.initNativeFunc1(listRemove));
    try self.addMethodSym(cy.ListS, sort, cy.MethodSym.initNativeFunc1(listSort));
    try self.addMethodSym(cy.ListS, len, cy.MethodSym.initNativeFunc1(listLen));

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
    try self.addMethodSym(cy.AstringT, append, cy.MethodSym.initNativeFunc1(astringAppend));
    try self.addMethodSym(cy.AstringT, charAt, cy.MethodSym.initNativeFunc1(astringCharAt));
    try self.addMethodSym(cy.AstringT, codeAt, cy.MethodSym.initNativeFunc1(astringCodeAt));
    try self.addMethodSym(cy.AstringT, endsWith, cy.MethodSym.initNativeFunc1(rawOrAstringEndsWith));
    try self.addMethodSym(cy.AstringT, index, cy.MethodSym.initNativeFunc1(rawOrAstringIndex));
    try self.addMethodSym(cy.AstringT, indexChar, cy.MethodSym.initNativeFunc1(astringIndexChar));
    try self.addMethodSym(cy.AstringT, indexCode, cy.MethodSym.initNativeFunc1(astringIndexCode));
    try self.addMethodSym(cy.AstringT, insert, cy.MethodSym.initNativeFunc1(astringInsert));
    try self.addMethodSym(cy.AstringT, isAscii, cy.MethodSym.initNativeFunc1(astringIsAscii));
    try self.addMethodSym(cy.AstringT, len, cy.MethodSym.initNativeFunc1(astringLen));
    try self.addMethodSym(cy.AstringT, lower, cy.MethodSym.initNativeFunc1(astringLower));
    try self.addMethodSym(cy.AstringT, replace, cy.MethodSym.initNativeFunc1(astringReplace));
    try self.addMethodSym(cy.AstringT, startsWith, cy.MethodSym.initNativeFunc1(rawOrAstringStartsWith));
    try self.addMethodSym(cy.AstringT, upper, cy.MethodSym.initNativeFunc1(astringUpper));

    id = try self.addStruct("string");
    std.debug.assert(id == cy.UstringT);
    try self.addMethodSym(cy.UstringT, append, cy.MethodSym.initNativeFunc1(ustringAppend));
    try self.addMethodSym(cy.UstringT, charAt, cy.MethodSym.initNativeFunc1(ustringCharAt));
    try self.addMethodSym(cy.UstringT, codeAt, cy.MethodSym.initNativeFunc1(ustringCodeAt));
    try self.addMethodSym(cy.UstringT, endsWith, cy.MethodSym.initNativeFunc1(ustringEndsWith));
    try self.addMethodSym(cy.UstringT, index, cy.MethodSym.initNativeFunc1(ustringIndex));
    try self.addMethodSym(cy.UstringT, indexChar, cy.MethodSym.initNativeFunc1(ustringIndexChar));
    try self.addMethodSym(cy.UstringT, indexCode, cy.MethodSym.initNativeFunc1(ustringIndexCode));
    try self.addMethodSym(cy.UstringT, insert, cy.MethodSym.initNativeFunc1(ustringInsert));
    try self.addMethodSym(cy.UstringT, isAscii, cy.MethodSym.initNativeFunc1(ustringIsAscii));
    try self.addMethodSym(cy.UstringT, len, cy.MethodSym.initNativeFunc1(ustringLen));
    try self.addMethodSym(cy.UstringT, lower, cy.MethodSym.initNativeFunc1(ustringLower));
    try self.addMethodSym(cy.UstringT, replace, cy.MethodSym.initNativeFunc1(ustringReplace));
    try self.addMethodSym(cy.UstringT, startsWith, cy.MethodSym.initNativeFunc1(ustringStartsWith));
    try self.addMethodSym(cy.UstringT, upper, cy.MethodSym.initNativeFunc1(ustringUpper));

    id = try self.addStruct("rawstring");
    std.debug.assert(id == cy.RawStringT);
    try self.addMethodSym(cy.RawStringT, append, cy.MethodSym.initNativeFunc1(rawStringAppend));
    try self.addMethodSym(cy.RawStringT, charAt, cy.MethodSym.initNativeFunc1(rawStringCharAt));
    try self.addMethodSym(cy.RawStringT, codeAt, cy.MethodSym.initNativeFunc1(rawStringCodeAt));
    try self.addMethodSym(cy.RawStringT, endsWith, cy.MethodSym.initNativeFunc1(rawOrAstringEndsWith));
    try self.addMethodSym(cy.RawStringT, index, cy.MethodSym.initNativeFunc1(rawOrAstringIndex));
    try self.addMethodSym(cy.RawStringT, indexChar, cy.MethodSym.initNativeFunc1(rawStringIndexChar));
    try self.addMethodSym(cy.RawStringT, indexCode, cy.MethodSym.initNativeFunc1(rawStringIndexCode));
    try self.addMethodSym(cy.RawStringT, insert, cy.MethodSym.initNativeFunc1(rawStringInsert));
    try self.addMethodSym(cy.RawStringT, insertByte, cy.MethodSym.initNativeFunc1(rawStringInsertByte));
    try self.addMethodSym(cy.RawStringT, isAscii, cy.MethodSym.initNativeFunc1(rawStringIsAscii));
    try self.addMethodSym(cy.RawStringT, len, cy.MethodSym.initNativeFunc1(rawStringLen));
    try self.addMethodSym(cy.RawStringT, lower, cy.MethodSym.initNativeFunc1(rawStringLower));
    try self.addMethodSym(cy.RawStringT, replace, cy.MethodSym.initNativeFunc1(rawStringReplace));
    try self.addMethodSym(cy.RawStringT, startsWith, cy.MethodSym.initNativeFunc1(rawOrAstringStartsWith));
    try self.addMethodSym(cy.RawStringT, upper, cy.MethodSym.initNativeFunc1(rawStringUpper));

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
    try self.addMethodSym(cy.FileT, streamLines, cy.MethodSym.initNativeFunc1(fileStreamLines));
    try self.addMethodSym(cy.FileT, streamLines1, cy.MethodSym.initNativeFunc1(fileStreamLines1));
    try self.addMethodSym(cy.FileT, self.iteratorObjSym, cy.MethodSym.initNativeFunc1(fileIterator));
    try self.addMethodSym(cy.FileT, self.nextObjSym, cy.MethodSym.initNativeFunc1(fileNext));

    const sid = try self.ensureStruct("CFunc");
    self.structs.buf[sid].numFields = 3;
    id = try self.ensureFieldSym("sym");
    try self.addFieldSym(sid, id, 0);
    id = try self.ensureFieldSym("args");
    try self.addFieldSym(sid, id, 1);
    id = try self.ensureFieldSym("ret");
    try self.addFieldSym(sid, id, 2);

    try ensureTagLitSym(self, "int", .int);
    try ensureTagLitSym(self, "bool", .bool);
    try ensureTagLitSym(self, "i8", .i8);
    try ensureTagLitSym(self, "u8", .u8);
    try ensureTagLitSym(self, "i16", .i16);
    try ensureTagLitSym(self, "u16", .u16);
    try ensureTagLitSym(self, "i32", .i32);
    try ensureTagLitSym(self, "u32", .u32);
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
    try ensureTagLitSym(self, "InvalidChar", .InvalidChar);
    try ensureTagLitSym(self, "SteamTooLong", .StreamTooLong);

    try ensureTagLitSym(self, "running", .running);
    try ensureTagLitSym(self, "paused", .paused);
    try ensureTagLitSym(self, "done", .done);

    try ensureTagLitSym(self, "error", .err);
    try ensureTagLitSym(self, "number", .number);
    try ensureTagLitSym(self, "object", .object);
    try ensureTagLitSym(self, "map", .map);
}

fn ensureTagLitSym(vm: *cy.VM, name: []const u8, tag: TagLit) !void {
    const id = try vm.ensureTagLitSym(name);
    std.debug.assert(id == @enumToInt(tag));
}

fn listSort(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
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
    vm.releaseObject(obj);
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

fn listAdd(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const list = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const inner = stdx.ptrAlignCast(*cy.List(Value), &list.list.list);
    if (inner.len == inner.buf.len) {
        // After reaching a certain size, use power of two ceil.
        // This reduces allocations for big lists while not over allocating for smaller lists.
        if (inner.len > 512) {
            const newCap = std.math.ceilPowerOfTwo(u32, @intCast(u32, inner.len) + 1) catch stdx.fatal();
            inner.growTotalCapacityPrecise(vm.allocator(), newCap) catch stdx.fatal();
        } else {
            inner.growTotalCapacity(vm.allocator(), inner.len + 1) catch stdx.fatal();
        }
    }
    inner.appendAssumeCapacity(args[0]);
    vm.releaseObject(list);
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
    inner.resize(vm.allocator(), size) catch stdx.fatal();
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

fn ustringLen(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.initF64(@intToFloat(f64, obj.ustring.charLen));
}

fn ustringUpper(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const new = vm.allocUnsetUstringObject(obj.ustring.len, obj.ustring.charLen) catch fatal();
    const newBuf = new.ustring.getSlice();
    _ = std.ascii.upperString(newBuf, obj.ustring.getConstSlice());
    return vm.allocOwnedUstring(new) catch fatal();
}

fn ustringLower(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const new = vm.allocUnsetUstringObject(obj.ustring.len, obj.ustring.charLen) catch fatal();
    const newBuf = new.ustring.getSlice();
    _ = std.ascii.lowerString(newBuf, obj.ustring.getConstSlice());
    return vm.allocOwnedUstring(new) catch fatal();
}

fn astringUpper(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const new = vm.allocUnsetAstringObject(obj.astring.len) catch fatal();
    const newBuf = new.astring.getSlice();
    _ = std.ascii.upperString(newBuf, obj.astring.getConstSlice());
    return vm.allocOwnedAstring(new) catch fatal();
}

fn astringLower(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const new = vm.allocUnsetAstringObject(obj.astring.len) catch fatal();
    const newBuf = new.astring.getSlice();
    _ = std.ascii.lowerString(newBuf, obj.astring.getConstSlice());
    return vm.allocOwnedAstring(new) catch fatal();
}

fn astringLen(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.initF64(@intToFloat(f64, obj.astring.len));
}

pub fn astringCharAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const str = obj.astring.getConstSlice();
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= str.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const uidx = @intCast(u32, idx);
    // TODO: return slice.
    return vm.allocAstring(str[uidx..uidx + 1]) catch fatal();
}

fn astringCodeAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const str = obj.astring.getConstSlice();
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= str.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    return Value.initF64(@intToFloat(f64, str[@floatToInt(u32, args[0].toF64())]));
}

fn rawStringInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index = @floatToInt(i64, args[0].toF64());
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[1]);
    }
    const str = obj.rawstring.getConstSlice();
    if (index < 0 or index > str.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    const insert = vm.valueToTempString(args[1]);
    const new = vm.allocUnsetRawStringObject(obj.rawstring.len + insert.len) catch stdx.fatal();
    const buf = new.rawstring.getSlice();
    const uidx = @intCast(u32, index);
    std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
    std.mem.copy(u8, buf[uidx..uidx+insert.len], insert);
    std.mem.copy(u8, buf[uidx+insert.len..], str[uidx..]);
    return Value.initPtr(new);
}

fn rawStringInsertByte(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.Section) Value {
    const index = @floatToInt(i64, args[0].toF64());
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[1]);
    }
    const str = obj.rawstring.getConstSlice();
    if (index < 0 or index > str.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    } 
    const byte = @floatToInt(u8, args[1].toF64());
    const new = vm.allocUnsetRawStringObject(obj.rawstring.len + 1) catch stdx.fatal();
    const buf = new.rawstring.getSlice();
    const uidx = @intCast(u32, index);
    std.mem.copy(u8, buf[0..uidx], str[0..uidx]);
    buf[uidx] = byte;
    std.mem.copy(u8, buf[uidx+1..], str[uidx..]);
    return Value.initPtr(new);
}

fn rawStringIsAscii(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.initBool(cy.isAstring(obj.rawstring.getConstSlice()));
}

fn rawStringUpper(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const new = vm.allocUnsetRawStringObject(obj.rawstring.len) catch fatal();
    const newBuf = new.rawstring.getSlice();
    _ = std.ascii.upperString(newBuf, obj.rawstring.getConstSlice());
    return Value.initPtr(new);
}

fn rawStringLower(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const new = vm.allocUnsetRawStringObject(obj.rawstring.len) catch fatal();
    const newBuf = new.rawstring.getSlice();
    _ = std.ascii.lowerString(newBuf, obj.rawstring.getConstSlice());
    return Value.initPtr(new);
}

fn rawStringLen(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.initF64(@intToFloat(f64, obj.rawstring.len));
}

fn staticUstringLen(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const str = val.asStaticStringSlice();
    const header = vm.getStaticUstringHeader(str.start);
    return Value.initF64(@intToFloat(f64, header.charLen));
}

fn staticAstringUpper(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const new = vm.allocUnsetAstringObject(str.len) catch fatal();
    const newBuf = new.astring.getSlice();
    _ = std.ascii.upperString(newBuf, str);
    return vm.allocOwnedAstring(new) catch fatal();
}

fn staticUstringUpper(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const header = vm.getStaticUstringHeader(slice.start);
    const new = vm.allocUnsetUstringObject(str.len, header.charLen) catch fatal();
    const newBuf = new.ustring.getSlice();
    _ = std.ascii.upperString(newBuf, str);
    return vm.allocOwnedUstring(new) catch fatal();
}

fn staticUstringReplace(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    if (ustringReplaceCommon(vm, str, args[0], args[1])) |res| {
        return res;
    } else {
        return val;
    }
}

fn ustringReplaceCommon(vm: *cy.UserVM, str: []const u8, needlev: Value, replacev: Value) linksection(cy.StdSection) ?Value {
    var ncharLen: u32 = undefined;
    const needle = vm.valueToTempString2(needlev, &ncharLen);
    var rcharLen: u32 = undefined;
    const replacement = vm.valueToNextTempString2(replacev, &rcharLen);

    const idxBuf = &@ptrCast(*cy.VM, vm).u32Buf;
    idxBuf.clearRetainingCapacity();
    const newLen = cy.prepReplacement(vm.allocator(), str, needle, replacement, idxBuf) catch fatal();
    if (idxBuf.len > 0) {
        const new = vm.allocUnsetUstringObject(newLen, @intCast(u32, str.len + idxBuf.len * rcharLen - idxBuf.len * ncharLen)) catch fatal();
        const newBuf = new.ustring.getSlice();
        cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxBuf.items());
        return vm.allocOwnedUstring(new) catch fatal();
    } else {
        return null;
    }
}

fn staticAstringReplace(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    defer {
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    if (astringReplaceCommon(vm, str, args[0], args[1])) |res| {
        return res;
    } else {
        return val;
    }
}

fn rawStringReplace(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const str = obj.rawstring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    const replacement = vm.valueToNextTempString(args[1]);

    const idxBuf = &@ptrCast(*cy.VM, vm).u32Buf;
    idxBuf.clearRetainingCapacity();
    const newLen = cy.prepReplacement(vm.allocator(), str, needle, replacement, idxBuf) catch fatal();
    if (idxBuf.len > 0) {
        const new = vm.allocUnsetRawStringObject(newLen) catch fatal();
        const newBuf = new.rawstring.getSlice();
        cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxBuf.items());
        return Value.initPtr(new);
    } else {
        vm.retainObject(obj);
        return Value.initPtr(obj);
    }
}

fn ustringReplace(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const str = obj.ustring.getConstSlice();
    if (ustringReplaceCommon(vm, str, args[0], args[1])) |val| {
        return val;
    } else {
        vm.retainObject(obj);
        return Value.initPtr(obj);
    }
}

fn astringReplace(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
        vm.release(args[1]);
    }
    const str = obj.astring.getConstSlice();
    if (astringReplaceCommon(vm, str, args[0], args[1])) |val| {
        return val;
    } else {
        vm.retainObject(obj);
        return Value.initPtr(obj);
    }
}

fn astringReplaceCommon(vm: *cy.UserVM, str: []const u8, needlev: Value, replacev: Value) linksection(cy.StdSection) ?Value {
    const needle = vm.valueToTempString(needlev);
    var rcharLen: u32 = undefined;
    const replacement = vm.valueToNextTempString2(replacev, &rcharLen);
    const idxBuf = &@ptrCast(*cy.VM, vm).u32Buf;
    idxBuf.clearRetainingCapacity();
    const newLen = cy.prepReplacement(vm.allocator(), str, needle, replacement, idxBuf) catch fatal();
    if (idxBuf.len > 0) {
        if (rcharLen == replacement.len) {
            const new = vm.allocUnsetAstringObject(newLen) catch fatal();
            const newBuf = new.astring.getSlice();
            cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxBuf.items());
            return vm.allocOwnedAstring(new) catch fatal();
        } else {
            const new = vm.allocUnsetUstringObject(newLen, @intCast(u32, str.len + idxBuf.len * rcharLen - idxBuf.len * needle.len)) catch fatal();
            const newBuf = new.ustring.getSlice();
            cy.replaceAtIdxes(newBuf, str, @intCast(u32, needle.len), replacement, idxBuf.items());
            return vm.allocOwnedUstring(new) catch fatal();
        }
    } else {
        return null;
    }
}

fn staticAstringLower(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const new = vm.allocUnsetAstringObject(str.len) catch fatal();
    const newBuf = new.astring.getSlice();
    _ = std.ascii.lowerString(newBuf, str);
    return vm.allocOwnedAstring(new) catch fatal();
}

fn staticUstringLower(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const header = vm.getStaticUstringHeader(slice.start);
    const new = vm.allocUnsetUstringObject(str.len, header.charLen) catch fatal();
    const newBuf = new.ustring.getSlice();
    _ = std.ascii.lowerString(newBuf, str);
    return vm.allocOwnedUstring(new) catch fatal();
}

fn staticAstringLen(_: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.Section) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const str = val.asStaticStringSlice();
    return Value.initF64(@intToFloat(f64, str.len()));
}

fn astringAppend(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.astring.getConstSlice();
    var rcharLen: u32 = undefined;
    const rstr = vm.valueToTempString2(args[0], &rcharLen);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    if (rcharLen == rstr.len) {
        return vm.allocAstringConcat(str, rstr) catch fatal();
    } else {
        return vm.allocUstringConcat(str, rstr, @intCast(u32, str.len + rcharLen)) catch fatal();
    }
}

fn ustringAppend(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.ustring.getConstSlice();
    var rcharLen: u32 = undefined;
    const rstr = vm.valueToTempString2(args[0], &rcharLen);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return vm.allocUstringConcat(str, rstr, obj.ustring.charLen + rcharLen) catch fatal();
}

fn rawStringAppend(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.rawstring.getConstSlice();
    const rstr = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return vm.allocRawStringConcat(str, rstr) catch fatal();
}

fn staticUstringAppend(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const header = vm.getStaticUstringHeader(slice.start);
    const str = vm.getStaticString(slice.start, slice.end);
    var rcharLen: u32 = undefined;
    const rstr = vm.valueToTempString2(args[0], &rcharLen);
    defer {
        vm.release(args[0]);
    }
    return vm.allocUstringConcat(str, rstr, header.charLen + rcharLen) catch fatal();
}

fn staticAstringAppend(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    var rcharLen: u32 = undefined;
    const rstr = vm.valueToTempString2(args[0], &rcharLen);
    defer vm.release(args[0]);
    if (rcharLen == rstr.len) {
        return vm.allocAstringConcat(str, rstr) catch fatal();
    } else {
        return vm.allocUstringConcat(str, rstr, @intCast(u32, str.len + rcharLen)) catch fatal();
    }
}

pub fn ustringCharAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.ustring.getConstSlice();
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= obj.ustring.charLen) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const uidx = @intCast(u32, idx);
    const res = cy.ustringSeekCharIndexSliceAt(str, obj.ustring.mruIdx, obj.ustring.mruCharIdx, uidx);
    obj.ustring.mruIdx = res.start;
    obj.ustring.mruCharIdx = uidx;
    if (res.start + 1 == res.end) {
        return vm.allocAstring(str[res.start..res.end]) catch fatal();
    } else {
        return vm.allocUstring(str[res.start..res.end], 1) catch fatal();
    }
}

fn ustringCodeAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.ustring.getConstSlice();
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= obj.ustring.charLen) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const uidx = @intCast(u32, idx);
    const res = cy.ustringSeekCharIndexSliceAt(str, obj.ustring.mruIdx, obj.ustring.mruCharIdx, uidx);
    const cp = std.unicode.utf8Decode(str[res.start..res.end]) catch stdx.fatal();
    obj.ustring.mruIdx = res.start;
    obj.ustring.mruCharIdx = uidx;
    return Value.initF64(@intToFloat(f64, cp));
}

pub fn staticUstringCharAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const header = vm.getStaticUstringHeader(slice.start);
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= header.charLen) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const uidx = @intCast(u32, idx);
    const str = vm.getStaticString(slice.start, slice.end);
    const res = cy.ustringSeekCharIndexSliceAt(str, header.mruIdx, header.mruCharIdx, uidx);
    header.mruIdx = res.start;
    header.mruCharIdx = uidx;
    if (res.start + 1 == res.end) {
        return Value.initStaticAstring(slice.start + res.start, 1);
    } else {
        return vm.allocUstring(str[res.start..res.end], 1) catch fatal();
    }
}

fn staticUstringCodeAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const header = vm.getStaticUstringHeader(slice.start);
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= header.charLen) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const uidx = @intCast(u32, idx);
    const str = vm.getStaticString(slice.start, slice.end);
    const res = cy.ustringSeekCharIndexSliceAt(str, header.mruIdx, header.mruCharIdx, uidx);
    const cp = std.unicode.utf8Decode(str[res.start..res.end]) catch stdx.fatal();
    header.mruIdx = res.start;
    header.mruCharIdx = uidx;
    return Value.initF64(@intToFloat(f64, cp));
}

pub fn staticAstringCharAt(_: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= slice.len()) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const uidx = @intCast(u32, idx);
    return Value.initStaticAstring(slice.start + uidx, 1);
}

fn ustringInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[1]);
    }
    const str = obj.ustring.getConstSlice();
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx > obj.ustring.charLen) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    var insertCharLen: u32 = undefined;
    const insert = vm.valueToTempString2(args[1], &insertCharLen);
    const uidx = @intCast(u32, idx);
    const res = cy.ustringSeekCharIndexSliceAt(str, obj.ustring.mruIdx, obj.ustring.mruCharIdx, uidx);
    obj.ustring.mruIdx = res.start;
    obj.ustring.mruCharIdx = uidx;
    return vm.allocUstringConcat3(str[0..res.start], insert, str[res.start..], @intCast(u32, obj.ustring.charLen + insertCharLen)) catch fatal();
}

fn staticUstringInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const header = vm.getStaticUstringHeader(slice.start);
    defer {
        vm.release(args[1]);
    }
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx > header.charLen) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    var insertCharLen: u32 = undefined;
    const insert = vm.valueToTempString2(args[1], &insertCharLen);
    const uidx = @intCast(u32, idx);
    const res = cy.ustringSeekCharIndexSliceAt(str, header.mruIdx, header.mruCharIdx, uidx);
    header.mruIdx = res.start;
    header.mruCharIdx = uidx;
    return vm.allocUstringConcat3(str[0..res.start], insert, str[res.start..], @intCast(u32, header.charLen + insertCharLen)) catch fatal();
}

fn astringInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[1]);
    }
    const str = obj.astring.getConstSlice();
    return astringInsertCommon(vm, str, args[0], args[1]);
}

fn astringInsertCommon(vm: *cy.UserVM, str: []const u8, idxv: Value, insertv: Value) linksection(cy.StdSection) Value {
    const idx = @floatToInt(i32, idxv.toF64());
    if (idx < 0 or idx > str.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    var insertCharLen: u32 = undefined;
    const insert = vm.valueToTempString2(insertv, &insertCharLen);
    const uidx = @intCast(u32, idx);
    if (insertCharLen == insert.len) {
        return vm.allocAstringConcat3(str[0..uidx], insert, str[uidx..]) catch fatal();
    } else {
        return vm.allocUstringConcat3(str[0..uidx], insert, str[uidx..], @intCast(u32, str.len + insertCharLen)) catch fatal();
    }
}

fn staticAstringInsert(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    defer {
        vm.release(args[1]);
    }
    return astringInsertCommon(vm, str, args[0], args[1]);
}

fn staticAstringCodeAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const str = val.asStaticStringSlice();
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= str.len()) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    return Value.initF64(@intToFloat(f64, vm.getStaticStringChar(str.start + @intCast(u32, idx))));
}

fn staticStringEndsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = vm.valueToTempString(args[0]);
    defer vm.release(args[0]);
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}

fn staticUstringIndex(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = vm.valueToTempString(args[0]);

    if (std.mem.indexOf(u8, str, needle)) |idx| {
        const charIdx = cy.toUtf8CharIdx(str, idx);
        return Value.initF64(@intToFloat(f64, charIdx));
    } else return Value.None;
}

fn staticAstringIndex(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = vm.valueToTempString(args[0]);

    if (std.mem.indexOf(u8, str, needle)) |idx| {
        return Value.initF64(@intToFloat(f64, idx));
    } else return Value.None;
}

fn staticStringStartsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = vm.valueToTempString(args[0]);
    defer vm.release(args[0]);
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}

fn ustringEndsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.ustring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}

fn astringEndsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.astring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}

fn astringStartsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.astring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}

fn ustringStartsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.ustring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}

pub fn rawStringCharAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.rawstring.getConstSlice();
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= obj.rawstring.len) {
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
}

fn rawStringCodeAt(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    const idx = @floatToInt(i32, args[0].toF64());
    if (idx < 0 or idx >= obj.rawstring.len) {
        return Value.initErrorTagLit(@enumToInt(TagLit.OutOfBounds));
    }
    const str = obj.rawstring.getConstSlice();
    const uidx = @intCast(u32, idx);

    if (cy.utf8CharSliceAt(str, uidx)) |slice| {
        const cp = std.unicode.utf8Decode(slice) catch stdx.fatal();
        return Value.initF64(@intToFloat(f64, cp));
    } else {
        return Value.initErrorTagLit(@enumToInt(TagLit.InvalidChar));
    }
}

fn rawOrAstringEndsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.astring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}

fn rawOrAstringStartsWith(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    const str = obj.astring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    return Value.initBool(std.mem.startsWith(u8, str, needle));
}

fn ustringIndex(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.ustring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);

    if (std.mem.indexOf(u8, str, needle)) |idx| {
        const charIdx = cy.toUtf8CharIdx(str, idx);
        return Value.initF64(@intToFloat(f64, charIdx));
    } else return Value.None;
}

fn rawOrAstringIndex(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.astring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);

    if (std.mem.indexOf(u8, str, needle)) |idx| {
        return Value.initF64(@intToFloat(f64, idx));
    } else return Value.None;
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

fn staticUstringIsAscii(_: *cy.UserVM, _: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.False;
}

fn staticAstringIsAscii(_: *cy.UserVM, _: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    return Value.True;
}

fn ustringIsAscii(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.False;
}

fn astringIsAscii(vm: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer vm.releaseObject(obj);
    return Value.True;
}

fn staticUstringIndexChar(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = vm.valueToTempString(args[0]);
    return ustringIndexCharCommon(str, needle);
}

fn ustringIndexCharCommon(str: []const u8, needle: []const u8) linksection(cy.StdSection) Value {
    if (needle.len > 0) {
        if (needle[0] & 0x80 == 0) {
            // Must be an ascii character.
            if (cy.indexOfChar(str, needle[0])) |idx| {
                const charIdx = cy.toUtf8CharIdx(str, idx);
                return Value.initF64(@intToFloat(f64, charIdx));
            }
        } else {
            const cpLen = std.unicode.utf8ByteSequenceLength(needle[0]) catch stdx.fatal();
            const cp = std.unicode.utf8Decode(needle[0..cpLen]) catch stdx.fatal();
            if (cy.charIndexOfCodepoint(str, cp)) |idx| {
                return Value.initF64(@intToFloat(f64, idx));
            }
        }
    }
    return Value.None;
}

fn staticAstringIndexChar(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = vm.valueToTempString(args[0]);
    if (needle.len > 0 and needle[0] & 0x80 == 0) {
        // Must be an ascii character.
        if (cy.indexOfChar(str, needle[0])) |idx| {
            return Value.initF64(@intToFloat(f64, idx));
        } else return Value.None;
    } else return Value.None;
}

fn staticUstringIndexCode(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = @floatToInt(i32, args[0].toF64());
    if (needle >= 0) {
        return ustringIndexCodeCommon(str, @intCast(u21, needle));
    } else return Value.None;
}

fn staticAstringIndexCode(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const val = Value{ .val = @ptrToInt(ptr) };
    const slice = val.asStaticStringSlice();
    const str = vm.getStaticString(slice.start, slice.end);
    const needle = @floatToInt(i32, args[0].toF64());
    if (needle >= 0 and needle < 128) {
        // Must be an ascii character.
        if (cy.indexOfChar(str, @intCast(u8, needle))) |idx| {
            return Value.initF64(@intToFloat(f64, idx));
        } else return Value.None;
    } else return Value.None;
}

fn ustringIndexChar(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.ustring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    return ustringIndexCharCommon(str, needle);
}

fn rawStringIndexCode(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.rawstring.getConstSlice();
    const cp = @floatToInt(i32, args[0].toF64());
    if (cp >= 0) {
        if (cp < 128) {
            if (cy.indexOfChar(str, @intCast(u8, cp))) |idx| {
                return Value.initF64(@intToFloat(f64, idx));
            }
        } else {
            var cpBuf: [4]u8 = undefined;
            const len = std.unicode.utf8Encode(@intCast(u21, cp), &cpBuf) catch stdx.fatal();
            if (std.mem.indexOf(u8, str, cpBuf[0..len])) |idx| {
                return Value.initF64(@intToFloat(f64, idx));
            }
        }
    }
    return Value.None;
}

fn rawStringIndexChar(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.rawstring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);

    if (needle.len > 0) {
        if (needle[0] & 0x80 == 0) {
            if (cy.indexOfChar(str, needle[0])) |idx| {
                return Value.initF64(@intToFloat(f64, idx));
            } else return Value.None;
        } else {
            const cpLen = std.unicode.utf8ByteSequenceLength(needle[0]) catch stdx.fatal();
            if (std.mem.indexOf(u8, str, needle[0..cpLen])) |idx| {
                return Value.initF64(@intToFloat(f64, idx));
            } else return Value.None;
        }
    } else return Value.None;
}

fn ustringIndexCode(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.ustring.getConstSlice();
    const needle = @floatToInt(i32, args[0].toF64());
    if (needle >= 0) {
        return ustringIndexCodeCommon(str, @intCast(u21, needle));
    } else return Value.None;
}

fn ustringIndexCodeCommon(str: []const u8, cp: u21) linksection(cy.StdSection) Value {
    if (cp < 128) {
        // Must be an ascii character.
        if (cy.indexOfChar(str, @intCast(u8, cp))) |idx| {
            const charIdx = cy.toUtf8CharIdx(str, idx);
            return Value.initF64(@intToFloat(f64, charIdx));
        }
    } else {
        if (cy.charIndexOfCodepoint(str, cp)) |idx| {
            return Value.initF64(@intToFloat(f64, idx));
        }
    }
    return Value.None;
}

fn astringIndexCode(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.astring.getConstSlice();
    const cp = @floatToInt(i32, args[0].toF64());
    if (cp >= 0 and cp < 128) {
        if (cy.indexOfChar(str, @intCast(u8, cp))) |idx| {
            return Value.initF64(@intToFloat(f64, idx));
        }
    }
    return Value.None;
}

fn astringIndexChar(vm: *cy.UserVM, ptr: *anyopaque, args: [*]const Value, _: u8) linksection(cy.StdSection) Value {
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    defer {
        vm.releaseObject(obj);
        vm.release(args[0]);
    }
    const str = obj.astring.getConstSlice();
    const needle = vm.valueToTempString(args[0]);
    if (needle.len > 0) {
        if (needle[0] & 0x80 == 0) {
            if (cy.indexOfChar(str, needle[0])) |idx| {
                return Value.initF64(@intToFloat(f64, idx));
            }
        }
    }
    return Value.None;
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

pub fn fileIterator(_: *cy.UserVM, ptr: *anyopaque, _: [*]const Value, _: u8) linksection(StdSection) Value {
    // Don't need to release obj since it's being returned.
    const obj = stdx.ptrAlignCast(*cy.HeapObject, ptr);
    obj.file.curPos = 0;
    obj.file.readBufEnd = 0;
    return Value.initPtr(ptr);
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
        const file = std.fs.File{
            .handle = @bitCast(i32, obj.file.fd),
            .capable_io_mode = .blocking,
            .intended_io_mode = .blocking,
        };
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