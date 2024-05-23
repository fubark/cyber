const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = cy.fatal;
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const Value = cy.Value;
const bindings = @import("bindings.zig");
const cy_mod = @import("cy.zig");
const Symbol = bindings.Symbol;
const fmt = @import("../fmt.zig");
const rt = cy.rt;
const bt = cy.types.BuiltinTypes;
const vmc = cy.vmc;
const string = @import("string.zig");

const log = cy.log.scoped(.core);

pub const VmSrc = @embedFile("builtins_vm.cy");
pub const Src = @embedFile("builtins.cy");

const func = cy.hostFuncEntry;
pub const funcs = [_]C.HostFuncEntry{
    // Utils.
    func("copy",           copy),
    func("dump",           zErrFunc(dump)),
    func("eprint",         eprint),
    func("errorReport",    zErrFunc(errorReport)),
    func("getObjectRc",    zErrFunc(getObjectRc)),
    func("is",             is),
    func("isAlpha",        isAlpha),
    func("isDigit",        isDigit),
    func("isNone",         isNone),
    func("must",           zErrFunc(must)),
    func("panic",          zErrFunc(panic)),
    func("performGC",      zErrFunc(performGC)),
    func("print",          print),
    func("runestr",        zErrFunc(runestr)),
    func("typeof",         typeof),

    // bool
    func("bool.$call",     boolCall),

    // error
    func("error.sym",      errorSym),
    func("error.$call",    errorCall),

    // int
    func("int.$prefix~",   bindings.intNot),
    func("int.$prefix-",   bindings.intNeg),
    func("int.$infix<",    bindings.intLess),
    func("int.$infix<=",   bindings.intLessEq),
    func("int.$infix>",    bindings.intGreater),
    func("int.$infix>=",   bindings.intGreaterEq),
    func("int.$infix+",    bindings.intAdd),
    func("int.$infix-",    bindings.intSub),
    func("int.$infix*",    bindings.intMul),
    func("int.$infix/",    bindings.intDiv),
    func("int.$infix%",    bindings.intMod),
    func("int.$infix^",    bindings.intPow),
    func("int.$infix&",    bindings.intAnd),
    func("int.$infix|",    bindings.intOr),
    func("int.$infix||",   bindings.intXor),
    func("int.$infix<<",   bindings.intLeftShift),
    func("int.$infix>>",   bindings.intRightShift),
    func("int.fmt",        zErrFunc(intFmt)),
    func("int.fmt2",       zErrFunc(intFmt2)),
    func("int.$call",      intCall),

    // float
    func("float.$prefix-", bindings.floatNeg),
    func("float.$infix<",  bindings.floatLess),
    func("float.$infix<=", bindings.floatLessEq),
    func("float.$infix>",  bindings.floatGreater),
    func("float.$infix>=", bindings.floatGreaterEq),
    func("float.$infix+",  bindings.floatAdd),
    func("float.$infix-",  bindings.floatSub),
    func("float.$infix*",  bindings.floatMul),
    func("float.$infix/",  bindings.floatDiv),
    func("float.$infix%",  bindings.floatMod),
    func("float.$infix^",  bindings.floatPow),
    func("float.$call",    floatCall),

    // List
    func("List.$index",      bindings.listIndex),
    func("List.$indexRange", zErrFunc(bindings.listSlice)),
    func("List.$setIndex",   bindings.listSetIndex),
    func("List.append",      zErrFunc(bindings.listAppend)),
    func("List.appendAll",   zErrFunc(bindings.listAppendAll)),
    func("List.insert",      bindings.listInsert),
    func("List.iterator_",   bindings.listIterator),
    func("List.join",        zErrFunc(bindings.listJoin)),
    func("List.len",         bindings.listLen),
    func("List.remove",      bindings.listRemove),
    func("List.resize",      bindings.listResize),
    // .{"sort", bindings.listSort, .standard},
    func("List.fill",        listFill),

    // ListIterator
    func("ListIterator.next",  bindings.listIteratorNext),

    // Tuple
    func("Tuple.$index",       bindings.tupleIndex),

    // Table
    func("Table.$initPair",    zErrFunc(bindings.tableInitPair)),
    func("Table.$get",         bindings.tableGet),
    func("Table.$set",         zErrFunc(bindings.tableSet)),
    func("Table.$index",       bindings.tableIndex),
    func("Table.$setIndex",    zErrFunc(bindings.tableSet)),

    // Map
    func("Map.$initPair",      zErrFunc(bindings.mapSetIndex)),
    func("Map.$index",         bindings.mapIndex),
    func("Map.$setIndex",      zErrFunc(bindings.mapSetIndex)),
    func("Map.contains",       bindings.mapContains),
    func("Map.get",            bindings.mapGet),
    func("Map.remove",         bindings.mapRemove),
    func("Map.size",           bindings.mapSize),
    func("Map.iterator",       bindings.mapIterator),

    // MapIterator
    func("MapIterator.next",   bindings.mapIteratorNext),

    // String
    func("String.$infix+",     string.concat),
    func("String.concat",      string.concat),
    func("String.count",       string.count),
    func("String.endsWith",    string.endsWith),
    func("String.find",        string.find),
    func("String.findAnyRune", string.findAnyRune),
    func("String.findRune",    string.findRune),
    func("String.insert",      zErrFunc(string.insertFn)),
    func("String.isAscii",     string.isAscii),
    func("String.len",         string.lenFn),
    func("String.less",        string.less),
    func("String.lower",       string.lower),
    func("String.replace",     string.stringReplace),
    func("String.repeat",      string.repeat),
    func("String.seek",        zErrFunc(string.seek)),
    func("String.sliceAt",     zErrFunc(string.sliceAt)),
    func("String.$index",      zErrFunc(string.runeAt)),
    func("String.$indexRange", string.sliceFn),
    func("String.split",       zErrFunc(string.split)),
    func("String.startsWith",  string.startsWith),
    func("String.trim",        string.trim),
    func("String.upper",       string.upper),
    func("String.$call",       zErrFunc(string.stringCall)),

    // Array
    func("Array.$infix+",      arrayConcat),
    func("Array.concat",       arrayConcat),
    func("Array.decode",       arrayDecode),
    func("Array.decode2",      arrayDecode2),
    func("Array.endsWith",     arrayEndsWith),
    func("Array.find",         arrayFind),
    func("Array.findAnyByte",  arrayFindAnyByte),
    func("Array.findByte",     arrayFindByte),
    func("Array.fmt",          zErrFunc(arrayFmt)),
    func("Array.getByte",      zErrFunc(arrayGetByte)),
    func("Array.getInt",       zErrFunc(arrayGetInt)),
    func("Array.getInt32",     zErrFunc(arrayGetInt32)),
    func("Array.insert",       arrayInsert),
    func("Array.insertByte",   arrayInsertByte),
    func("Array.len",          arrayLen),
    func("Array.repeat",       zErrFunc(arrayRepeat)),
    func("Array.replace",      arrayReplace),
    func("Array.$index",       zErrFunc(arrayGetByte)),
    func("Array.$indexRange",  arraySlice),
    func("Array.split",        zErrFunc(arraySplit)),
    func("Array.startsWith",   arrayStartsWith),
    func("Array.trim",         zErrFunc(arrayTrim)),
    func("Array.$call",        zErrFunc(arrayCall)),

    // pointer
    func("pointer.addr",       pointerAddr),
    func("pointer.asObject",   pointerAsObject),
    func("pointer.fromCstr",   zErrFunc(pointerFromCstr)),
    func("pointer.get",        zErrFunc(pointerGet)),
    func("pointer.set",        zErrFunc(pointerSet)),
    func("pointer.toArray",    zErrFunc(pointerToArray)),
    func("pointer.$call",      pointerCall),

    // ExternFunc
    func("ExternFunc.addr",    externFuncAddr),

    // Fiber
    func("Fiber.status",       fiberStatus),

    // metatype
    func("metatype.id",        metatypeId),
};

pub const types = [_]C.HostTypeEntry{
};

const htype = C.hostTypeEntry;
pub const vm_types = [_]C.HostTypeEntry{
    htype("void",          C.CORE_TYPE(bt.Void)),
    htype("bool",          C.CORE_TYPE_DECL(bt.Boolean)),
    htype("symbol",        C.CORE_TYPE_DECL(bt.Symbol)),
    htype("error",         C.CORE_TYPE_DECL(bt.Error)),
    htype("int",           C.CORE_TYPE_DECL(bt.Integer)),
    htype("float",         C.CORE_TYPE_DECL(bt.Float)), 
    htype("placeholder1",  C.CORE_TYPE(bt.Placeholder1)), 
    htype("placeholder2",  C.CORE_TYPE(bt.Placeholder2)), 
    htype("taglit",        C.CORE_TYPE(bt.TagLit)), 
    htype("dyn",           C.CORE_TYPE(bt.Dyn)),
    htype("any",           C.CORE_TYPE(bt.Any)),
    htype("type",          C.CORE_TYPE(bt.Type)),
    htype("List",          C.CUSTOM_TYPE(null, listGetChildren, listFinalizer)),
    htype("ListDyn",       C.CORE_TYPE_EXT(bt.ListDyn, listGetChildren, listFinalizer)),
    htype("ListIterator",  C.CUSTOM_TYPE(null, listIterGetChildren, null)),
    htype("ListIterDyn",   C.CORE_TYPE_EXT(bt.ListIterDyn, listIterGetChildren, null)),
    htype("Tuple",         C.CORE_TYPE(bt.Tuple)),
    htype("Table",         C.CORE_TYPE_DECL(bt.Table)),
    htype("Map",           C.CORE_TYPE(bt.Map)),
    htype("MapIterator",   C.CORE_TYPE(bt.MapIter)),
    htype("String",        C.CORE_TYPE(bt.String)),
    htype("Array",         C.CORE_TYPE(bt.Array)),
    htype("pointer",       C.CORE_TYPE(bt.Pointer)),
    htype("Closure",       C.CORE_TYPE(bt.Closure)),
    htype("Lambda",        C.CORE_TYPE(bt.Lambda)),
    htype("HostFunc",      C.CORE_TYPE(bt.HostFunc)),
    htype("ExternFunc",    C.CORE_TYPE(bt.ExternFunc)),
    htype("Fiber",         C.CORE_TYPE(bt.Fiber)),
    htype("metatype",      C.CORE_TYPE(bt.MetaType)),
    htype("Range",         C.CORE_TYPE(bt.Range)),
    htype("Box",           C.CORE_TYPE(bt.Box)),
    htype("TccState",      C.CORE_TYPE(bt.TccState)),
};

pub var OptionInt: cy.TypeId = undefined;
pub var OptionAny: cy.TypeId = undefined;
pub var OptionTuple: cy.TypeId = undefined;
pub var OptionMap: cy.TypeId = undefined;
pub var OptionArray: cy.TypeId = undefined;
pub var OptionString: cy.TypeId = undefined;

pub fn onLoad(vm_: ?*C.VM, mod: C.Sym) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const chunk_sym = cy.Sym.fromC(mod).cast(.chunk);
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(chunk_sym));
    if (cy.Trace) {
        b.declareFuncSig("traceRetains", &.{}, bt.Integer, traceRetains) catch cy.fatal();
        b.declareFuncSig("traceReleases", &.{}, bt.Integer, traceRetains) catch cy.fatal();
    }

    const option_tmpl = chunk_sym.getMod().getSym("Option").?.toC();

    const int_t = C.newType(vm_, bt.Integer);
    defer C.release(vm_, int_t);
    OptionInt = C.expandTemplateType(option_tmpl, @constCast(&[_]C.Value{ int_t }), 1);

    const any_t = C.newType(vm_, bt.Any);
    defer C.release(vm_, any_t);
    OptionAny = C.expandTemplateType(option_tmpl, @constCast(&[_]C.Value{ any_t }), 1);

    const tuple_t = C.newType(vm_, bt.Tuple);
    defer C.release(vm_, tuple_t);
    OptionTuple = C.expandTemplateType(option_tmpl, @constCast(&[_]C.Value{ tuple_t }), 1);

    const map_t = C.newType(vm_, bt.Map);
    defer C.release(vm_, map_t);
    OptionMap = C.expandTemplateType(option_tmpl, @constCast(&[_]C.Value{ map_t }), 1);

    const array_t = C.newType(vm_, bt.Array);
    defer C.release(vm_, array_t);
    OptionArray = C.expandTemplateType(option_tmpl, @constCast(&[_]C.Value{ array_t }), 1);

    const string_t = C.newType(vm_, bt.String);
    defer C.release(vm_, string_t);
    OptionString = C.expandTemplateType(option_tmpl, @constCast(&[_]C.Value{ string_t }), 1);

    const list_tmpl = chunk_sym.getMod().getSym("List").?.toC();

    const dynamic_t = C.newType(vm_, bt.Dyn);
    defer C.release(vm_, dynamic_t);
    _ = C.expandTemplateType(list_tmpl, @constCast(&[_]C.Value{ dynamic_t }), 1);

    const list_iter_tmpl = chunk_sym.getMod().getSym("ListIterator").?.toC();
    _ = C.expandTemplateType(list_iter_tmpl, @constCast(&[_]C.Value{ dynamic_t }), 1);

    // Verify all core types have been initialized.
    if (cy.Trace) {
        for (0..cy.types.BuiltinEnd) |i| {
            const type_e = vm.sema.types.items[i];
            if (type_e.kind == .null) {
                cy.panicFmt("Type {} is uninited.", .{i});
            }
        }
    }
}

pub fn cFunc(f: *const fn (vm: cy.Context, args: [*]const Value, nargs: u8) callconv(.C) Value) cy.ZHostFuncFn {
    return @ptrCast(f);
}

pub fn zErrFunc(comptime f: fn (vm: *cy.VM, args: [*]const Value, nargs: u8) anyerror!Value) cy.ZHostFuncFn {
    const S = struct {
        pub fn genFunc(vm: *cy.VM, args: [*]const Value, nargs: u8) callconv(.C) Value {
            return @call(.always_inline, f, .{vm, args, nargs}) catch |err| {
                return @call(.never_inline, prepThrowZError, .{vm, err, @errorReturnTrace()});
            };
        }
    };
    return @ptrCast(&S.genFunc);
}

pub fn prepThrowZError(ctx: cy.Context, err: anyerror, optTrace: ?*std.builtin.StackTrace) Value {
    if (!cy.isFreestanding and C.verbose()) {
        if (optTrace) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
    }
    const sym = errorSymbol(err);
    return rt.prepThrowError(ctx, sym);
}

pub fn prepThrowZError2(ctx: cy.Context, err: anyerror, optTrace: ?*std.builtin.StackTrace) rt.Error {
    if (!cy.isFreestanding and C.verbose()) {
        if (optTrace) |trace| {
            std.debug.dumpStackTrace(trace.*);
        }
    }
    const sym = errorSymbol(err);
    _ = rt.prepThrowError(ctx, sym);
    return rt.Error.init(@tagName(sym));
}

fn errorSymbol(err: anyerror) Symbol {
    switch (err) {
        error.AssertError           => return .AssertError,
        error.EvalError             => return .EvalError,
        error.Unicode               => return .Unicode,
        error.InvalidResult         => return .InvalidResult,
        error.InvalidArgument       => return .InvalidArgument,
        error.InvalidEnumTag        => return .InvalidArgument,
        error.FileNotFound          => return .FileNotFound,
        error.OutOfBounds           => return .OutOfBounds,
        error.PermissionDenied      => return .PermissionDenied,
        error.StdoutStreamTooLong   => return .StreamTooLong,
        error.StderrStreamTooLong   => return .StreamTooLong,
        error.EndOfStream           => return .EndOfStream,
        else                        => return .UnknownError,
    }
}

fn traceRetains(vm: *cy.VM, _: [*]const Value, _: u8) Value {
    return Value.initInt(vm.c.trace.numRetains);
}

fn traceReleases(vm: *cy.VM, _: [*]const Value, _: u8) Value {
    return Value.initInt(vm.c.trace.numReleases);
}

pub fn listFill(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    return vm.allocListFill(args[0], @intCast(args[1].asInteger())) catch cy.fatal();
}

pub fn copy(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    return cy.value.shallowCopy(vm, val);
}

pub fn errorReport(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    const curFrameLen = vm.compactTrace.len;

    // Append frames from current call-site.
    try cy.fiber.recordCurFrames(vm);

    // Remove top frame since it contains the `errorReport` call.
    if (vm.compactTrace.len > curFrameLen) {
        vm.compactTrace.remove(curFrameLen);
    }

    const trace = try cy.debug.allocStackTrace(vm, vm.c.getStack(), vm.compactTrace.items());
    defer vm.alloc.free(trace);

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(vm.alloc);

    const w = buf.writer(vm.alloc);
    try cy.debug.writeStackFrames(vm, w, trace);

    return vm.allocString(buf.items);
}

pub fn must(vm: *cy.VM, args: [*]const Value, nargs: u8) anyerror!Value {
    if (!args[0].isError()) {
        return args[0];
    } else {
        return panic(vm, args, nargs);
    }
}

pub fn panic(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const str = try vm.getOrBufPrintValueStr(&cy.tempBuf, args[0]);
    return vm.prepPanic(str);
}

pub fn is(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].val == args[1].val);
}

pub fn isAlpha(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const num = args[0].asInteger();
    if (num < 0 or num >= 2 << 21) {
        return rt.prepThrowError(vm, .InvalidRune);
    }
    if (num > 255) {
        return Value.False;
    } else {
        return Value.initBool(std.ascii.isAlphabetic(@intCast(num)));
    }
}

pub fn isDigit(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const num = args[0].asInteger();
    if (num < 0 or num >= 2 << 21) {
        return rt.prepThrowError(vm, .InvalidRune);
    }
    if (num > 255) {
        return Value.False;
    } else {
        return Value.initBool(std.ascii.isDigit(@intCast(num)));
    }
}

pub fn isNone(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const type_e = vm.c.types[args[0].getTypeId()];
    if (type_e.kind != .option) {
        return Value.False;
    }
    const is_none = args[0].asHeapObject().object.getValue(0).asInteger() == 0;
    return Value.initBool(is_none);
}

pub fn runestr(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const num = args[0].asInteger();
    if (num < 0 or num >= 2 << 21) {
        return rt.prepThrowError(vm, .InvalidRune);
    }
    const rune: u21 = @intCast(num);
    if (std.unicode.utf8ValidCodepoint(rune)) {
        var buf: [4]u8 = undefined;
        const len = try std.unicode.utf8Encode(rune, &buf);
        return vm.allocString(buf[0..len]);
    } else {
        return rt.prepThrowError(vm, .InvalidRune);
    }
}

pub fn dump(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const res = try cy_mod.allocToCyon(vm, vm.alloc, args[0]);
    defer vm.alloc.free(res);
    rt.print(vm, res);
    rt.print(vm, "\n");
    return Value.Void;
}

pub fn getObjectRc(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (args[0].isPointer()) {
        return Value.initInt(@intCast(args[0].asHeapObject().head.rc));
    } else {
        return Value.initInt(-1);
    }
}

pub fn performGC(vm: *cy.VM, _: [*]const Value, _: u8) anyerror!Value {
    const res = try cy.arc.performGC(vm);
    const map = try vm.allocEmptyMap();
    const cycKey = try vm.retainOrAllocAstring("numCycFreed");
    const objKey = try vm.retainOrAllocAstring("numObjFreed");
    defer {
        vm.release(cycKey);
        vm.release(objKey);
    }
    try map.asHeapObject().map.set(vm, cycKey, Value.initInt(@intCast(res.numCycFreed)));
    try map.asHeapObject().map.set(vm, objKey, Value.initInt(@intCast(res.numObjFreed)));
    return map;
}

pub fn eprint(vm: *cy.VM, args: [*]const cy.Value, _: u8) Value {
    const err = eprint_c(vm, args[0]);
    if (!err.isNull()) {
        return Value.Interrupt;
    }
    return Value.Void;
}

pub fn eprint_c(ctx: cy.Context, arg: rt.Any) callconv(.C) rt.Error {
    if (build_options.rt == .vm) {
        const str = ctx.getOrBufPrintValueStr(&cy.tempBuf, arg) catch |err| {
            return cy.builtins.prepThrowZError2(ctx, err, @errorReturnTrace());
        };
        rt.err(ctx, str);
        rt.err(ctx, "\n");
    } else {
        const str = arg.type.toPrintString(ctx, arg);
        rt.err(ctx, str.slice());
        rt.err(ctx, "\n");
        ctx.release(str.buf);
    }
    return rt.Error.initNull();
}

pub fn print(vm: *cy.VM, args: [*]const cy.Value, _: u8) Value {
    const err = print_c(vm, args[0]);
    if (!err.isNull()) {
        return Value.Interrupt;
    }
    return Value.Void;
}

pub fn print_c(ctx: cy.Context, arg: rt.Any) callconv(.C) rt.Error {
    if (build_options.rt == .vm) {
        const str = ctx.getOrBufPrintValueStr(&cy.tempBuf, arg) catch |err| {
            return cy.builtins.prepThrowZError2(ctx, err, @errorReturnTrace());
        };
        rt.print(ctx, str);
        rt.print(ctx, "\n");
    } else {
        const str = arg.type.toPrintString(ctx, arg);
        rt.print(ctx, str.slice());
        rt.print(ctx, "\n");
        ctx.release(str.buf);
    }
    return rt.Error.initNull();
}

pub fn typeof(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    const typeId = val.getTypeId();
    return cy.heap.allocMetaType(vm, @intFromEnum(cy.heap.MetaTypeKind.object), typeId) catch fatal();
}

pub fn listFinalizer(vm_: ?*C.VM, obj: ?*anyopaque) callconv(.C) void {
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    var list: *cy.heap.ListInner = @ptrCast(@alignCast(obj));
    list.getList().deinit(vm.alloc);
}

pub fn listGetChildren(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) C.ValueSlice {
    var list: *cy.heap.ListInner = @ptrCast(@alignCast(obj));
    const items = list.items();
    return .{
        .ptr = @ptrCast(items.ptr),
        .len = items.len,
    };
}

pub fn listIterGetChildren(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) C.ValueSlice {
    var iter: *cy.heap.ListIterInner = @ptrCast(@alignCast(obj));
    return .{
        .ptr = @ptrCast(&iter.list),
        .len = 1,
    };
}

fn arrayConcat(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const rslice = args[1].asArray();
    return vm.allocArrayConcat(slice, rslice) catch fatal();
}

fn arraySlice(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();

    const range = args[1].asHeapObject();

    var start: i48 = undefined;
    if (!range.range.has_start) {
        start = 0;
    } else {
        start = @intCast(range.range.start);
    }
    if (start < 0) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }

    var end: i48 = undefined;
    if (!range.range.has_end) {
        end = @intCast(slice.len);
    } else {
        end = @intCast(range.range.end);
    }
    if (end > slice.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }
    if (end < start) {
        return rt.prepThrowError(vm, .OutOfBounds);
    }

    const parent = obj.array.getParent();
    vm.retainObject(parent);
    return vm.allocArraySlice(slice[@intCast(start)..@intCast(end)], parent) catch fatal();
}

fn arrayInsert(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const idx = args[1].asInteger();
    if (idx < 0 or idx > slice.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    } 
    const insert = args[2].asArray();
    const new = vm.allocUnsetArrayObject(slice.len + insert.len) catch cy.fatal();
    const buf = new.array.getMutSlice();
    const uidx: u32 = @intCast(idx);
    @memcpy(buf[0..uidx], slice[0..uidx]);
    @memcpy(buf[uidx..uidx+insert.len], insert);
    @memcpy(buf[uidx+insert.len..], slice[uidx..]);
    return Value.initNoCycPtr(new);
}

fn arrayFind(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const needle = args[1].asArray();
    if (needle.len > 0 and needle.len <= slice.len) {
        if (needle.len == 1) {
            // One byte special case. Perform indexOfChar.
            if (cy.string.indexOfChar(slice, needle[0])) |idx| {
                return intSome(vm, @intCast(idx)) catch cy.fatal();
            }
        }
        if (cy.string.indexOf(slice, needle)) |idx| {
            return intSome(vm, @intCast(idx)) catch cy.fatal();
        }
    }
    return intNone(vm) catch cy.fatal();
}

fn arrayStartsWith(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const needle = args[1].asArray();
    return Value.initBool(std.mem.startsWith(u8, slice, needle));
}

fn arrayEndsWith(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const str = args[0].asHeapObject().array.getSlice();
    const needle = args[1].asArray();
    return Value.initBool(std.mem.endsWith(u8, str, needle));
}

fn arrayDecode(vm: *cy.VM, args: [*]const Value, nargs: u8) Value {
    const encoding = Value.initSymbol(@intFromEnum(Symbol.utf8));
    return arrayDecode2(vm, &[_]Value{args[0], encoding}, nargs);
}

fn arrayDecode2(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();

    const encoding = bindings.getBuiltinSymbol(args[1].asSymbolId()) orelse {
        return rt.prepThrowError(vm, .InvalidArgument);
    };
    if (encoding != Symbol.utf8) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    const parent = obj.array.getParent();
    const slice = obj.array.getSlice();
    if (cy.string.validateUtf8(slice)) |size| {
        // Since the bytes are validated, just return a slice view of the bytes.
        if (size == slice.len) {
            vm.retainObject(parent);
            return vm.allocAstringSlice(slice, parent) catch fatal();
        } else {
            vm.retainObject(parent);
            return vm.allocUstringSlice(slice, parent) catch fatal();
        }
    } else {
        return rt.prepThrowError(vm, .Unicode);
    }
}

fn arrayGetByte(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const idx = args[1].asInteger();

    if (idx < 0 or idx >= slice.len) return error.OutOfBounds;
    return Value.initInt(@intCast(slice[@intCast(idx)]));
}

fn arrayGetInt(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();

    const slice = obj.array.getSlice();
    const idx = args[1].asInteger();
    const sym = try std.meta.intToEnum(Symbol, args[2].asSymbolId());
    const endian: std.builtin.Endian = switch (sym) {
        .little => .little,
        .big => .big,
        else => return error.InvalidArgument,
    };

    if (idx < 0 or idx + 6 > slice.len) return error.OutOfBounds;
    const uidx: usize = @intCast(idx);
    const val = std.mem.readVarInt(u48, slice[uidx..uidx+6], endian);
    return Value.initInt(@bitCast(val));
}

fn arrayGetInt32(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();

    const slice = obj.array.getSlice();
    const idx = args[1].asInteger();
    const sym = try std.meta.intToEnum(Symbol, args[2].asSymbolId());
    const endian: std.builtin.Endian = switch (sym) {
        .little => .little,
        .big => .big,
        else => return error.InvalidArgument,
    };

    if (idx < 0 or idx + 4 > slice.len) return error.OutOfBounds;
    const uidx: usize = @intCast(idx);
    const val = std.mem.readVarInt(u48, slice[uidx..uidx+4], endian);
    return Value.initInt(@intCast(val));
}

fn arrayFindAnyByte(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const set = args[1].asArray();
    const setIsAscii = cy.string.isAstring(set);
    if (setIsAscii) {
        if (cy.string.indexOfAsciiSet(slice, set)) |idx| {
            return intSome(vm, @intCast(idx)) catch cy.fatal();
        }
    } else {
        // Slow check against every byte.
        var minIndex: u32 = cy.NullId;
        for (set) |byte| {
            if (cy.string.indexOfChar(slice, byte)) |idx| {
                if (idx < minIndex) {
                    minIndex = @intCast(idx);
                }
            }
        }
        if (minIndex != cy.NullId) {
            return intSome(vm, @intCast(minIndex)) catch cy.fatal();
        }
    }
    return intNone(vm) catch cy.fatal();
}

fn arrayFindByte(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const byte = args[1].asInteger();

    if (cy.string.indexOfChar(slice, @intCast(byte))) |idx| {
        return intSome(vm, @intCast(idx)) catch cy.fatal();
    }
    return intNone(vm) catch cy.fatal();
}

fn arrayFmt(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const arr = args[0].asArray();
    const kind = try std.meta.intToEnum(Symbol, args[1].asSymbolId());
    if (kind == .c) {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(vm.alloc);

        for (arr) |byte| {
            if (byte < 0 or byte > 127) {
                return error.InvalidArgument;
            }
        }
        return vm.retainOrAllocAstring(arr);
    } else {
        var base: u8 = undefined;
        var width: u8 = undefined;
        switch (kind) {
            .b => {
                base = 2;
                width = 8;
            },
            .o => {
                base = 8;
                width = 3;
            },
            .d => {
                base = 10;
                width = 3;
            },
            .x => {
                base = 16;
                width = 2;
            },
            else => return error.InvalidArgument,
        }

        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(vm.alloc);
        const w = buf.writer(vm.alloc);
        for (arr) |byte| {
            try std.fmt.formatInt(byte, base, .lower, .{ .width = width, .fill = '0' }, w);
        }
        return vm.retainOrAllocAstring(buf.items);
    }
}

fn arrayLen(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(@intCast(obj.array.getSlice().len));
}

fn arrayTrim(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();

    const trimRunes = args[2].asArray();

    var res: []const u8 = undefined;
    const mode = bindings.getBuiltinSymbol(args[1].asSymbolId()) orelse {
        return rt.prepThrowError(vm, .InvalidArgument);
    };
    switch (mode) {
        .left => res = std.mem.trimLeft(u8, slice, trimRunes),
        .right => res = std.mem.trimRight(u8, slice, trimRunes),
        .ends => res = std.mem.trim(u8, slice, trimRunes),
        else => {
            return rt.prepThrowError(vm, .InvalidArgument);
        }
    }

    return vm.allocArray(res);
}

fn arrayReplace(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const needle = args[1].asArray();
    const replacement = args[2].asArray();

    const idxBuf = &vm.u8Buf;
    idxBuf.clearRetainingCapacity();
    defer idxBuf.ensureMaxCapOrClear(vm.alloc, 4096) catch fatal();
    const newLen = cy.string.prepReplacement(slice, needle, replacement, idxBuf.writer(vm.alloc)) catch fatal();
    const numIdxes = @divExact(idxBuf.len, 4);
    if (numIdxes > 0) {
        const new = vm.allocUnsetArrayObject(newLen) catch fatal();
        const newBuf = new.array.getMutSlice();
        const idxes = @as([*]const u32, @ptrCast(idxBuf.buf.ptr))[0..numIdxes];
        cy.string.replaceAtIdxes(newBuf, slice, @intCast(needle.len), replacement, idxes);
        return Value.initNoCycPtr(new);
    } else {
        vm.retainObject(obj);
        return Value.initNoCycPtr(obj);
    }
}

fn arrayInsertByte(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    const str = obj.array.getSlice();

    const index: i48 = args[1].asInteger();
    if (index < 0 or index > str.len) {
        return rt.prepThrowError(vm, .OutOfBounds);
    } 
    const byte: u8 = @intCast(args[2].asInteger());
    const new = vm.allocUnsetArrayObject(str.len + 1) catch cy.fatal();
    const buf = new.array.getMutSlice();
    const uidx: usize = @intCast(index);
    @memcpy(buf[0..uidx], str[0..uidx]);
    buf[uidx] = byte;
    @memcpy(buf[uidx+1..], str[uidx..]);
    return Value.initNoCycPtr(new);
}

fn arrayRepeat(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();

    const n = args[1].asInteger();
    if (n < 0) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    const un: u32 = @intCast(n);
    const len = un * slice.len;
    if (un > 1 and len > 0) {
        const new = try vm.allocUnsetArrayObject(len);
        const buf = new.array.getMutSlice();

        // This is already quite fast since it has good cache locality.
        // Might be faster if the front of the buffer up to a certain size was used to memcpy instead of just 1 `str`.
        var i: u32 = 0;
        var dst: u32 = 0;
        while (i < un) : (i += 1) {
            @memcpy(buf[dst..dst + slice.len], slice);
            dst += @intCast(slice.len);
        }

        return Value.initNoCycPtr(new);
    } else {
        if (un == 0) {
            return vm.allocArray("");
        } else {
            vm.retainObject(obj);
            return Value.initNoCycPtr(obj);
        }
    }
}

fn arraySplit(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const slice = obj.array.getSlice();
    const delim = args[1].asArray();

    const res = try vm.allocEmptyListDyn();
    if (delim.len == 0) {
        return res;
    }
    const list = res.asHeapObject();

    const parent = obj.array.getParent();
    var iter = std.mem.split(u8, slice, delim);
    while (iter.next()) |part| {
        vm.retainObject(parent);
        const new = try vm.allocArraySlice(part, parent);
        try list.list.append(vm.alloc, new);
    }
    return res;
}

fn arrayCall(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const str = try vm.getOrBufPrintValueRawStr(&cy.tempBuf, args[0]);
    return vm.allocArray(str);
}

fn fiberStatus(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const fiber = args[0].castHeapObject(*vmc.Fiber);

    if (vm.c.curFiber == fiber) {
        return Value.initSymbol(@intFromEnum(Symbol.running));
    } else {
        // Check if done.
        if (fiber.pcOffset == cy.NullId) {
            return Value.initSymbol(@intFromEnum(Symbol.done));
        } else {
            return Value.initSymbol(@intFromEnum(Symbol.paused));
        }
    }
}

fn metatypeId(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(obj.metatype.type);
}

fn pointerAsObject(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const ptr = args[0].asHeapObject().pointer.ptr;
    vm.retainObject(@ptrCast(@alignCast(ptr)));
    return Value.initPtr(ptr);
}

fn pointerAddr(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(@bitCast(@as(u48, (@intCast(@intFromPtr(obj.pointer.ptr))))));
}

fn pointerFromCstr(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const obj = args[0].asHeapObject();
    const raw: [*]const u8 = @ptrCast(obj.pointer.ptr);
    const off: u48 = @bitCast(args[1].asInteger());
    const bytes = std.mem.span(@as([*:0]const u8, @ptrCast(raw + off)));
    return vm.allocArray(bytes);
}

fn pointerGet(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const off = args[1].asInteger();
    const ctype = try std.meta.intToEnum(Symbol, args[2].asSymbolId());

    const raw = obj.pointer.ptr;
    const uoff: u48 = @bitCast(off);
    switch (ctype) {
        .voidPtr => {
            const addr: usize = @intFromPtr(raw) + @as(usize, @intCast(uoff));
            const val = @as(*?*anyopaque, @ptrFromInt(addr)).*;
            return vm.allocPointer(val);
        },
        else => {
            return error.InvalidArgument;
        }
    }
}

fn pointerSet(_: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const idx = args[1].asInteger();
    const ctype = try std.meta.intToEnum(Symbol, args[2].asSymbolId());
    const val = args[3];
    const rawPtr = obj.pointer.ptr;
    const valT = val.getTypeId();
    const uidx: u48 = @bitCast(idx);
    switch (ctype) {
        .int => {
            switch (valT) {
                bt.Integer => {
                    const addr: usize = @intFromPtr(rawPtr) + @as(usize, @intCast(uidx));
                    @as(*i32, @ptrFromInt(addr)).* = @intCast(val.asInteger());
                    return Value.Void;
                },
                else => {
                    return error.InvalidArgument;
                }
            }
        },
        .voidPtr => {
            switch (valT) {
                bt.Pointer => {
                    const addr: usize = @intFromPtr(rawPtr) + @as(usize, @intCast(uidx));
                    @as(*?*anyopaque, @ptrFromInt(addr)).* = val.asHeapObject().pointer.ptr;
                    return Value.Void;
                },
                bt.ExternFunc => {
                    const addr: usize = @intFromPtr(rawPtr) + @as(usize, @intCast(uidx));
                    @as(*?*anyopaque, @ptrFromInt(addr)).* = val.asHeapObject().externFunc.ptr;
                    return Value.Void;
                },
                else => {
                    return error.InvalidArgument;
                }
            }
        },
        else => {
            return error.InvalidArgument;
        }
    }
}

fn pointerToArray(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const obj = args[0].asHeapObject();
    const off: u48 = @bitCast(args[1].asInteger());
    const len: u48 = @bitCast(args[2].asInteger());
    const raw: [*]const u8 = @ptrCast(obj.pointer.ptr);
    const uoff: usize = @intCast(off);
    return vm.allocArray(raw[uoff..@intCast(uoff+len)]);
}

fn pointerCall(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isPointerT()) {
        vm.retain(val);
        return val;
    } else if (val.isInteger()) {
        const i: usize = @intCast(val.asInteger());
        return cy.heap.allocPointer(vm, @ptrFromInt(i)) catch fatal();
    } else {
        return vm.prepPanic("Not a `pointer`.");
    }
}

fn externFuncAddr(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const obj = args[0].asHeapObject();
    return Value.initInt(@bitCast(@as(u48, (@intCast(@intFromPtr(obj.externFunc.ptr))))));
}

fn errorSym(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const recv = args[0];
    return Value.initSymbol(recv.asErrorSymbol());
}

fn errorCall(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    if (val.isPointer()) {
        return rt.prepThrowError(vm, .InvalidArgument);
    } else {
        if (val.isSymbol()) {
            return Value.initErrorSymbol(@intCast(val.asSymbolId()));
        } else if (val.isEnum()) {
            const enumT = val.getEnumType();
            const enumv = val.getEnumValue();
            const name = vm.c.types[enumT].sym.cast(.enum_t).getValueSym(enumv).head.name();
            const symId = vm.ensureSymbol(name) catch cy.unexpected();
            return Value.initErrorSymbol(symId);
        } else {
            return rt.prepThrowError(vm, .InvalidArgument);
        }
    }
}

fn intFmt(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const val = args[0].asInteger();
    const kind = try std.meta.intToEnum(Symbol, args[1].asSymbolId());
    return intFmtExt(vm, val, kind, .{});
}

fn intFmt2(vm: *cy.VM, args: [*]const Value, _: u8) anyerror!Value {
    const val = args[0].asInteger();
    const kind = try std.meta.intToEnum(Symbol, args[1].asSymbolId());
    const optsv = args[2].castHeapObject(*cy.heap.Table);
    var opts: IntFmtOptions = .{};
    if (optsv.map().getByString("pad")) |pad| {
        if (!pad.isInteger()) return error.InvalidArgument;
        const padv = pad.asInteger();
        if (padv < 0 or padv > 127) return error.InvalidArgument;
        opts.pad = @intCast(padv);
    }
    if (optsv.map().getByString("width")) |width| {
        if (!width.isInteger()) return error.InvalidArgument;
        const widthv = width.asInteger();
        if (widthv < 0) return error.InvalidArgument;
        opts.width = @intCast(widthv);
    }
    return intFmtExt(vm, val, kind, opts);
}

const IntFmtOptions = struct {
    pad: ?u8 = null,
    width: ?usize = null,
};

fn intFmtExt(vm: *cy.VM, val: i48, kind: Symbol, opts: IntFmtOptions) !Value {
    if (kind == .c) {
        if (val < 0 or val > 127) {
            return error.InvalidArgument;
        }
        const uchar: u8 = @intCast(val);
        return vm.retainOrAllocAstring(&.{uchar});
    } else {
        const base: u8 = switch (kind) {
            .b => 2,
            .o => 8,
            .d => 10,
            .x => 16,
            else => return error.InvalidArgument,
        };
        var buf: [48]u8 = undefined;
        var fb = std.io.fixedBufferStream(&buf);
        if (val < 0) {
            try std.fmt.formatInt(val, base, .lower, .{ .fill = opts.pad orelse ' ', .width = opts.width }, fb.writer());
        } else {
            try std.fmt.formatInt(@as(u48, @bitCast(val)), base, .lower, .{ .fill = opts.pad orelse ' ', .width = opts.width }, fb.writer());
        }
        return vm.retainOrAllocAstring(fb.getWritten());
    }
}

fn intCall(_: *cy.VM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getTypeId()) {
        bt.Float => {
            return Value.initInt(@intFromFloat(@trunc(val.asF64())));
        },
        bt.String => {
            var str = val.asString();
            if (std.mem.indexOfScalar(u8, str, '.')) |idx| {
                str = str[0..idx];
            }
            const res = std.fmt.parseInt(i32, str, 10) catch {
                return Value.initInt(0);
            };
            return Value.initInt(res);
        },
        bt.Symbol => return Value.initInt(@intCast(val.val & @as(u64, 0xFF))),
        bt.Integer => {
            return val;
        },
        else => {
            if (val.isEnum()) {
                return Value.initInt(val.getEnumValue());
            }
            return Value.initInt(0);
        }
    }
}

fn boolCall(_: *cy.VM, args: [*]const Value, _: u8) Value {
    return Value.initBool(args[0].toBool());
}

fn floatCall(vm: *cy.VM, args: [*]const Value, _: u8) Value {
    const val = args[0];
    switch (val.getTypeId()) {
        bt.Float => return val,
        bt.String => {
            const res = std.fmt.parseFloat(f64, val.asString()) catch {
                return Value.initF64(0);
            };
            return Value.initF64(res);
        },
        bt.Symbol => return Value.initF64(@floatFromInt(val.val & @as(u64, 0xFF))),
        bt.Integer => return Value.initF64(@floatFromInt(val.asInteger())),
        bt.Void => return Value.initF64(0),
        bt.Boolean => return Value.initF64(if (val.asBool()) 1 else 0),
        else => {
            if (val.isEnum()) {
                return Value.initF64(@floatFromInt(val.getEnumValue()));
            }
            vm.release(val);
            return vm.prepPanic("Not a type that can be converted to `float`.");
        }
    }
}

pub fn intNone(vm: *cy.VM) !Value {
    return vm.allocObjectSmall(OptionInt, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn intSome(vm: *cy.VM, v: i48) !Value {
    return vm.allocObjectSmall(OptionInt, &.{ Value.initInt(1), Value.initInt(v) });
}

pub fn anyNone(vm: *cy.VM) !Value {
    return vm.allocObjectSmall(OptionAny, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn anySome(vm: *cy.VM, v: Value) !Value {
    return vm.allocObjectSmall(OptionAny, &.{ Value.initInt(1), v });
}

pub fn TupleNone(vm: *cy.VM) !Value {
    return vm.allocObjectSmall(OptionTuple, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn TupleSome(vm: *cy.VM, v: Value) !Value {
    return vm.allocObjectSmall(OptionTuple, &.{ Value.initInt(1), v });
}

pub fn MapNone(vm: *cy.VM) !Value {
    return vm.allocObjectSmall(OptionMap, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn MapSome(vm: *cy.VM, v: Value) !Value {
    return vm.allocObjectSmall(OptionMap, &.{ Value.initInt(1), v });
}

pub fn ArrayNone(vm: *cy.VM) !Value {
    return vm.allocObjectSmall(OptionArray, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn ArraySome(vm: *cy.VM, v: Value) !Value {
    return vm.allocObjectSmall(OptionArray, &.{ Value.initInt(1), v });
}

pub fn StringNone(vm: *cy.VM) !Value {
    return vm.allocObjectSmall(OptionString, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn StringSome(vm: *cy.VM, v: Value) !Value {
    return vm.allocObjectSmall(OptionString, &.{ Value.initInt(1), v });
}