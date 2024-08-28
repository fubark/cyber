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

const VmSrc = @embedFile("builtins_vm.cy");
const Src = @embedFile("builtins.cy");

pub const CoreData = struct {
    ValueT: cy.TypeId,
    VMT: cy.TypeId,
    EvalResultT: cy.TypeId,
    EvalConfigT: cy.TypeId,
};

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    // Utils.
    func("bitcast_",       zErrFunc(bitcast)),
    func("copy",           zErrFunc(copy)),
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
    func("ptrcast_",       zErrFunc(ptrcast)),
    func("print",          print),
    func("refcast",        zErrFunc(refcast)),
    func("queueTask",      zErrFunc(queueTask)),
    func("runestr",        zErrFunc(runestr)),
    func("sizeof_",        sizeof),

    // Compile-time funcs.

    // bool
    func("bool.$call",     boolCall),

    // error
    func("error.sym",      errorSym),
    func("error.$call",    errorCall),

    // byte
    func("byte.$prefix~",   bindings.byteNot),
    func("byte.$infix<",    bindings.byteLess),
    func("byte.$infix<=",   bindings.byteLessEq),
    func("byte.$infix>",    bindings.byteGreater),
    func("byte.$infix>=",   bindings.byteGreaterEq),
    func("byte.$infix+",    bindings.byteAdd),
    func("byte.$infix-",    bindings.byteSub),
    func("byte.$infix*",    bindings.byteMul),
    func("byte.$infix/",    bindings.byteDiv),
    func("byte.$infix%",    bindings.byteMod),
    func("byte.$infix^",    bindings.bytePow),
    func("byte.$infix&",    bindings.byteAnd),
    func("byte.$infix|",    bindings.byteOr),
    func("byte.$infix||",   bindings.byteXor),
    func("byte.$infix<<",   bindings.byteLeftShift),
    func("byte.$infix>>",   bindings.byteRightShift),
    func("byte.fmt",        zErrFunc(bindings.byteFmt)),
    func("byte.fmt2",       zErrFunc(bindings.byteFmt2)),
    func("byte.$call",      bindings.byteCall),

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
    func("List.$indexRange", zErrFunc(bindings.List_slice)),
    func("List.$setIndex",   bindings.listSetIndex),
    func("List.append",      zErrFunc(bindings.listAppend)),
    func("List.appendAll",   zErrFunc(bindings.listAppendAll)),
    func("List.insert",      bindings.listInsert),
    func("List.iterator_",   bindings.listIterator),
    func("List.join",        zErrFunc(bindings.listJoin)),
    func("List.len",         bindings.listLen),
    func("List.remove",      bindings.listRemove),
    func("List.resize_",     zErrFunc(bindings.listResize)),
    // .{"sort", bindings.listSort, .standard},
    func("List.fill",        listFill),

    // ListIterator
    func("ListIterator.next_", zErrFunc(bindings.listIteratorNext)),

    // Slice
    // func("Slice.endsWith",         Slice_endsWith),
    // func("Slice.find",         Slice_find),
    // func("Slice.split",        zErrFunc(sliceSplit)),
    // func("Slice.startsWith",         Slice_startsWith),
    // func("Slice.trim", sliceTrim),

    // RefSlice
    // func("RefSlice.endsWith",         RefSlice_endsWith),
    // func("RefSlice.find",         RefSlice_find),
    // func("RefSlice.split",        zErrFunc(sliceSplit)),
    // func("RefSlice.startsWith",         RefSlice_startsWith),
    // func("RefSlice.trim", sliceTrim),

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
    // func("String.decode",       String_decode),
    // func("String.decode2",      String_decode2),
    func("String.endsWith",    string.endsWith),
    func("String.find",        string.find),
    func("String.findAnyByte", stringFindAnyByte),
    func("String.findAnyRune", zErrFunc(string.findAnyRune)),
    func("String.findByte",    stringFindByte),
    func("String.findRune",    string.findRune),
    func("String.fmtBytes",    zErrFunc(stringFmtBytes)),
    func("String.getByte",     zErrFunc(stringGetByte)),
    func("String.getInt",      zErrFunc(String_getInt)),
    func("String.getInt32",    zErrFunc(String_getInt32)),
    func("String.insert",      zErrFunc(string.insertFn)),
    func("String.insertByte",  zErrFunc(string.insertByte)),
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
    // func("Array.$infix+",      arrayConcat),
    // func("Array.concat",       arrayConcat),
    // func("Array.insert",       zErrFunc(arrayInsert)),
    // func("Array.repeat",       zErrFunc(arrayRepeat)),
    func("Array.index",        zErrFunc(arrayIndex)),
    // func("Array.indexRange",   zErrFunc(arraySlice)),

    // pointer
    func("pointer.index",      zErrFunc(pointerIndex)),
    func("pointer.indexRange", zErrFunc(pointerIndexRange)),
    func("pointer.setIndex",   zErrFunc(pointerSetIndex)),
    func("pointer.addr",       pointerAddr),
    func("pointer.asObject",   pointerAsObject),
    func("pointer.fromCstr",   zErrFunc(pointerFromCstr)),
    func("pointer.get",        zErrFunc(pointerGet)),
    func("pointer.getString",  zErrFunc(pointerGetString)),
    func("pointer.set",        zErrFunc(pointerSet)),
    func("pointer.$call",      zErrFunc(pointerCall)),

    // ExternFunc
    func("ExternFunc.addr",    externFuncAddr),
    func("ExternFunc.ptr",     externFuncPtr),

    // ExprType
    func("ExprType.getType",   zErrFunc(ExprType_getType)),

    // Fiber
    func("Fiber.status",       fiberStatus),

    // type
    func("type.id",            type_id_),
    func("type.$call",         zErrFunc(type_call)),

    // Future
    func("Future.complete_",   zErrFunc(Future_complete)),
    func("Future.new_",        zErrFunc(Future_new)),

    // FutureResolver
    func("FutureResolver.complete", zErrFunc(futureResolverComplete)),
    func("FutureResolver.future",   zErrFunc(futureResolverFuture)),
    func("FutureResolver.new_",     zErrFunc(futureResolverNew)),

    // DefaultMemory
    func("DefaultMemory.alloc",     zErrFunc(DefaultMemory_alloc)),
    func("DefaultMemory.free",      zErrFunc(DefaultMemory_free)), 
};

const types = [_]C.HostTypeEntry{
};

const htype = C.hostTypeEntry;
const vm_types = [_]C.HostTypeEntry{
    htype("void",           C.CORE_TYPE(bt.Void)),
    htype("bool",           C.DECL_TYPE(bt.Boolean)),
    htype("symbol",         C.DECL_TYPE(bt.Symbol)),
    htype("error",          C.DECL_TYPE(bt.Error)),
    htype("int",            C.DECL_TYPE(bt.Integer)),
    htype("float",          C.DECL_TYPE(bt.Float)), 
    htype("placeholder1",   C.CORE_TYPE(bt.Placeholder1)), 
    htype("placeholder2",   C.CORE_TYPE(bt.Placeholder2)), 
    htype("placeholder3",   C.CORE_TYPE(bt.Placeholder3)), 
    htype("byte",           C.DECL_TYPE(bt.Byte)), 
    htype("taglit",         C.CORE_TYPE(bt.TagLit)), 
    htype("dyn",            C.CORE_TYPE(bt.Dyn)),
    htype("any",            C.CORE_TYPE(bt.Any)),
    htype("type",           C.CORE_TYPE(bt.Type)),
    htype("ExprType",       C.DECL_TYPE(bt.ExprType)),
    htype("List",           C.CREATE_TYPE(createListType)),
    htype("ListIterator",   C.CREATE_TYPE(createListIterType)),
    htype("Tuple",          C.CORE_TYPE(bt.Tuple)),
    htype("Table",          C.DECL_TYPE(bt.Table)),
    htype("Map",            C.CORE_TYPE(bt.Map)),
    htype("MapIterator",    C.CORE_TYPE(bt.MapIter)),
    htype("String",         C.CORE_TYPE(bt.String)),
    htype("ExternFunc",     C.CORE_TYPE(bt.ExternFunc)),
    htype("Fiber",          C.CORE_TYPE(bt.Fiber)),
    htype("Range",          C.DECL_TYPE(bt.Range)),
    htype("TccState",       C.CORE_TYPE(bt.TccState)),
    htype("Future",         C.HOST_OBJECT(null, futureGetChildren, null)),
    htype("FutureResolver", C.HOST_OBJECT(null, futureResolverGetChildren, null)),
    htype("Memory",         C.DECL_TYPE(bt.Memory)),
    htype("array_t",        C.CREATE_TYPE(createArrayType)),
    htype("FuncSig",        C.DECL_TYPE(bt.FuncSig)), 
    htype("funcptr_t",      C.CREATE_TYPE(createFuncPtrType)),
    htype("funcunion_t",    C.CREATE_TYPE(createFuncUnionType)),
    htype("Func",           C.CORE_TYPE(bt.Func)),
    htype("funcsym_t",      C.CREATE_TYPE(createFuncSymType)),
};

fn createFuncSymType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) C.Sym {
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const sig: cy.sema.FuncSigId = @intCast(ctx.ct_params.get("SIG").?.asBoxInt());
    const type_id = c.sema.pushType() catch @panic("error");
    const sym = c.createFuncSymType(@ptrCast(chunk_sym), "funcsym", type_id, sig, C.fromNode(decl)) catch @panic("error");
    return @as(*cy.Sym, @ptrCast(sym)).toC();
}

fn createFuncUnionType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) C.Sym {
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const sig: cy.sema.FuncSigId = @intCast(ctx.ct_params.get("SIG").?.asBoxInt());
    const type_id = c.sema.pushType() catch @panic("error");
    const sym = c.createFuncUnionType(@ptrCast(chunk_sym), "Func", type_id, sig, C.fromNode(decl)) catch @panic("error");
    return @as(*cy.Sym, @ptrCast(sym)).toC();
}

fn createFuncPtrType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) C.Sym {
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const sig: cy.sema.FuncSigId = @intCast(ctx.ct_params.get("SIG").?.asBoxInt());
    const type_id = c.sema.pushType() catch @panic("error");
    const sym = c.createFuncPtrType(@ptrCast(chunk_sym), "funcptr", type_id, sig, C.fromNode(decl)) catch @panic("error");
    return @as(*cy.Sym, @ptrCast(sym)).toC();
}

fn createArrayType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) C.Sym {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const n: usize = @intCast(ctx.ct_params.get("N").?.asBoxInt());
    const elem_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;
    const type_id = c.sema.pushType() catch @panic("error");
    const sym = c.createArrayType(@ptrCast(chunk_sym), "array_t", type_id, n, elem_t) catch @panic("error");
    return @as(*cy.Sym, @ptrCast(sym)).toC();
}

fn createListType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) C.Sym {
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;
    if (child_t == bt.Dyn) {
        const sym = c.createHostObjectType(@ptrCast(chunk_sym), "List", @ptrCast(C.fromNode(decl))) catch @panic("error");
        cy.sema.resolveHostObjectType(c, sym,
            listGetChildren,
            listFinalizer,
            bt.ListDyn,
            false,
            false,
        ) catch @panic("error");
        return @as(*cy.Sym, @ptrCast(sym)).toC();
    } else {
        const sym = c.createHostObjectType(@ptrCast(chunk_sym), "List", @ptrCast(C.fromNode(decl))) catch @panic("error");
        cy.sema.resolveHostObjectType(c, sym,
            listGetChildren,
            listFinalizer,
            null,
            false,
            false,
        ) catch @panic("error");
        return @as(*cy.Sym, @ptrCast(sym)).toC();
    }
}

fn createListIterType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) C.Sym {
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;
    if (child_t == bt.Dyn) {
        const sym = c.createHostObjectType(@ptrCast(chunk_sym), "ListIterator", @ptrCast(C.fromNode(decl))) catch @panic("error");
        cy.sema.resolveHostObjectType(c, sym,
            listIterGetChildren,
            null,
            bt.ListIterDyn,
            false,
            false,
        ) catch @panic("error");
        return @as(*cy.Sym, @ptrCast(sym)).toC();
    } else {
        const sym = c.createHostObjectType(@ptrCast(chunk_sym), "ListIterator", @ptrCast(C.fromNode(decl))) catch @panic("error");
        cy.sema.resolveHostObjectType(c, sym,
            listIterGetChildren,
            null,
            null,
            false,
            false,
        ) catch @panic("error");
        return @as(*cy.Sym, @ptrCast(sym)).toC();
    }
}

pub const BuiltinsData = struct {
    OptionInt: cy.TypeId,
    OptionAny: cy.TypeId,
    OptionTuple: cy.TypeId,
    OptionMap: cy.TypeId,
    OptionString: cy.TypeId,
    PtrVoid: cy.TypeId,
    PtrSliceByte: cy.TypeId,
};

pub fn create(vm: *cy.VM, r_uri: []const u8) C.Module {
    const aot = cy.isAot(vm.compiler.config.backend);
    const src = if (aot) C.toStr(Src) else C.toStr(VmSrc);
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), src);

    var config = C.ModuleConfig{
        .types = if (aot) C.toSlice(C.HostTypeEntry, &types) else C.toSlice(C.HostTypeEntry, &vm_types),
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
        .onLoad = onLoad,
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

fn onLoad(vm_: ?*C.VM, mod: C.Sym) callconv(.C) void {
    log.tracev("builtins: on load", .{});
    const vm: *cy.VM = @ptrCast(@alignCast(vm_));
    const chunk_sym = cy.Sym.fromC(mod).cast(.chunk);
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(chunk_sym));
    if (cy.Trace) {
        b.declareFuncSig("traceRetains", &.{}, bt.Integer, traceRetains) catch cy.fatal();
        b.declareFuncSig("traceReleases", &.{}, bt.Integer, traceRetains) catch cy.fatal();
    }

    const data = vm.getData(*BuiltinsData, "builtins");

    const option_tmpl = chunk_sym.getMod().getSym("Option").?.toC();

    const assert = std.debug.assert;

    const int_t = C.newType(vm_, bt.Integer);
    defer C.release(vm_, int_t);
    assert(C.expandTemplateType(option_tmpl, &int_t, 1, &data.OptionInt));

    const any_t = C.newType(vm_, bt.Any);
    defer C.release(vm_, any_t);
    assert(C.expandTemplateType(option_tmpl, &any_t, 1, &data.OptionAny));

    const tuple_t = C.newType(vm_, bt.Tuple);
    defer C.release(vm_, tuple_t);
    assert(C.expandTemplateType(option_tmpl, &tuple_t, 1, &data.OptionTuple));

    const map_t = C.newType(vm_, bt.Map);
    defer C.release(vm_, map_t);
    assert(C.expandTemplateType(option_tmpl, &map_t, 1, &data.OptionMap));

    const string_t = C.newType(vm_, bt.String);
    defer C.release(vm_, string_t);
    assert(C.expandTemplateType(option_tmpl, &string_t, 1, &data.OptionString));

    const pointer_tmpl = chunk_sym.getMod().getSym("pointer").?.toC();

    const void_t = C.newType(vm_, bt.Void);
    defer C.release(vm_, void_t);
    assert(C.expandTemplateType(pointer_tmpl, &void_t, 1, &data.PtrVoid));

    const list_tmpl = chunk_sym.getMod().getSym("List").?.toC();

    const dynamic_t = C.newType(vm_, bt.Dyn);
    defer C.release(vm_, dynamic_t);
    var temp: cy.TypeId = undefined;
    assert(C.expandTemplateType(list_tmpl, &dynamic_t, 1, &temp));

    const list_iter_tmpl = chunk_sym.getMod().getSym("ListIterator").?.toC();
    assert(C.expandTemplateType(list_iter_tmpl, &dynamic_t, 1, &temp));

    const ptr_slice_tmpl = chunk_sym.getMod().getSym("PtrSlice").?.toC();
    const byte_t = C.newType(vm_, bt.Byte);
    defer C.release(vm_, byte_t);
    assert(C.expandTemplateType(ptr_slice_tmpl, &byte_t, 1, &data.PtrSliceByte));

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

pub fn zErrFunc(comptime f: fn (vm: *cy.VM) anyerror!Value) cy.ZHostFuncFn {
    const S = struct {
        pub fn genFunc(vm: *cy.VM) callconv(.C) Value {
            return @call(.always_inline, f, .{vm}) catch |err| {
                return @call(.never_inline, prepThrowZError, .{vm, err, @errorReturnTrace()});
            };
        }
    };
    return @ptrCast(&S.genFunc);
}

pub fn prepThrowZError(ctx: cy.Context, err: anyerror, optTrace: ?*std.builtin.StackTrace) Value {
    if (!cy.isFreestanding and C.verbose()) {
        log.tracev("{}", .{err});
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
        error.Unsupported           => return .Unsupported,
        else                        => return .UnknownError,
    }
}

fn traceRetains(vm: *cy.VM) Value {
    return Value.initInt(vm.c.trace.numRetains);
}

fn traceReleases(vm: *cy.VM) Value {
    return Value.initInt(vm.c.trace.numReleases);
}

pub fn listFill(vm: *cy.VM) Value {
    const list_t: cy.TypeId = @intCast(vm.getInt(0));
    const val_t: cy.TypeId = @intCast(vm.getInt(1));
    return vm.allocListFill(list_t, val_t, vm.getValue(2), @intCast(vm.getInt(3))) catch cy.fatal();
}

pub fn refcast(vm: *cy.VM) anyerror!Value {
    return vm.getValue(0);
}

pub fn ptrcast(vm: *cy.VM) anyerror!Value {
    return vm.getValue(1);
}

pub fn bitcast(vm: *cy.VM) anyerror!Value {
    const dst_t: cy.TypeId = @intCast(vm.getInt(0));
    const src_t: cy.TypeId = @intCast(vm.getInt(2));
    if (vm.c.types[src_t].kind != .int or vm.c.types[dst_t].kind != .int) {
        return error.InvalidArgument;
    }
    if (vm.c.types[src_t].data.int.bits != vm.c.types[dst_t].data.int.bits) {
        return error.InvalidArgument;
    }
    return vm.getValue(1);
}

pub fn copy(vm: *cy.VM) anyerror!Value {
    const type_id: cy.TypeId = @intCast(vm.getInt(0));
    const val = vm.getValue(1);
    return cy.value.shallowCopy(vm, type_id, val);
}

pub fn errorReport(vm: *cy.VM) anyerror!Value {
    const curFrameLen = vm.compactTrace.len;

    // Append frames from current call-site.
    try cy.fiber.recordCurFrames(vm);

    // Remove top two frames since it contains the `errorReport` call.
    vm.compactTrace.remove(curFrameLen);
    vm.compactTrace.remove(curFrameLen);

    const trace = try cy.debug.allocStackTrace(vm, vm.c.getStack(), vm.compactTrace.items());
    defer vm.alloc.free(trace);

    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(vm.alloc);

    const w = buf.writer(vm.alloc);
    try cy.debug.writeStackFrames(vm, w, trace);

    return vm.allocString(buf.items);
}

pub fn must(vm: *cy.VM) anyerror!Value {
    const val = vm.getValue(0);
    if (!val.isError()) {
        vm.retain(val);
        return val;
    } else {
        return panic(vm);
    }
}

pub fn panic(vm: *cy.VM) anyerror!Value {
    const str = try vm.getOrBufPrintValueStr(&cy.tempBuf, vm.getValue(0));
    return vm.prepPanic(str);
}

pub fn is(vm: *cy.VM) Value {
    return Value.initBool(vm.getValue(0).val == vm.getValue(1).val);
}

pub fn isAlpha(vm: *cy.VM) Value {
    const num = vm.getInt(0);
    if (num < 0 or num >= 2 << 21) {
        return rt.prepThrowError(vm, .InvalidRune);
    }
    if (num > 255) {
        return Value.False;
    } else {
        return Value.initBool(std.ascii.isAlphabetic(@intCast(num)));
    }
}

pub fn isDigit(vm: *cy.VM) Value {
    const num = vm.getInt(0);
    if (num < 0 or num >= 2 << 21) {
        return rt.prepThrowError(vm, .InvalidRune);
    }
    if (num > 255) {
        return Value.False;
    } else {
        return Value.initBool(std.ascii.isDigit(@intCast(num)));
    }
}

pub fn isNone(vm: *cy.VM) Value {
    const val = vm.getValue(0);
    const type_e = vm.c.types[val.getTypeId()];
    if (type_e.kind != .option) {
        return Value.False;
    }
    const is_none = val.asHeapObject().object.getValue(0).asInt() == 0;
    return Value.initBool(is_none);
}

pub fn runestr(vm: *cy.VM) anyerror!Value {
    const num = vm.getInt(0);
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

pub fn dump(vm: *cy.VM) anyerror!Value {
    // const res = try cy_mod.allocToCyon(vm, vm.alloc, vm.getValue(0));
    // defer vm.alloc.free(res);
    // rt.print(vm, res);
    // rt.print(vm, "\n");
    cy.vm.zDumpValue(vm, vm.getValue(0));
    return Value.Void;
}

pub fn getObjectRc(vm: *cy.VM) anyerror!Value {
    const val = vm.getValue(0);
    if (val.isPointer()) {
        return Value.initInt(@intCast(val.asHeapObject().head.rc));
    } else {
        return Value.initInt(-1);
    }
}

pub fn performGC(vm: *cy.VM) anyerror!Value {
    const res = try cy.arc.performGC(vm);
    const map = try vm.allocEmptyMap();
    const cycKey = try vm.retainOrAllocAstring("numCycFreed");
    const objKey = try vm.retainOrAllocAstring("numObjFreed");
    const num_cyc_freed = try vm.allocInt(@intCast(res.numCycFreed));
    try map.asHeapObject().map.setConsume(vm, cycKey, num_cyc_freed);
    const num_obj_freed = try vm.allocInt(@intCast(res.numObjFreed));
    try map.asHeapObject().map.setConsume(vm, objKey, num_obj_freed);
    return map;
}

pub fn eprint(vm: *cy.VM) Value {
    const err = eprint_c(vm, vm.getValue(0));
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

pub fn print(vm: *cy.VM) Value {
    const err = print_c(vm, vm.getValue(0));
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

pub fn queueTask(vm: *cy.VM) anyerror!Value {
    const cb = vm.getValue(0);
    vm.retain(cb);
    const task = cy.heap.AsyncTask{
        .type = .callback,
        .data = .{ .callback = cb },
    };
    try vm.ready_tasks.writeItem(task);
    return Value.Void;
}

pub fn sizeof(vm: *cy.VM) Value {
    const type_id: cy.TypeId = @intCast(vm.getInt(0));
    const type_e = vm.c.types[type_id];
    if (type_e.kind == .struct_t) {
        return Value.initInt(8 * type_e.data.struct_t.nfields);
    } else {
        return Value.initInt(8);
    }
}

pub fn type_call(vm: *cy.VM) !Value {
    const val = vm.getValue(0);
    const typeId = val.getTypeId();
    return cy.heap.allocType(vm, typeId);
}

fn type_id_(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.Type, 0);
    return Value.initInt(obj.type);
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

pub fn futureGetChildren(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) C.ValueSlice {
    var future: *cy.heap.Future = @ptrCast(@alignCast(obj));
    if (future.completed) {
        return .{
            .ptr = @ptrCast(&future.val),
            .len = 1,
        };
    } else {
        return .{
            .ptr = null,
            .len = 0,
        };
    }
}

pub fn futureResolverGetChildren(_: ?*C.VM, obj: ?*anyopaque) callconv(.C) C.ValueSlice {
    var resolver: *cy.heap.FutureResolver = @ptrCast(@alignCast(obj));
    return .{
        .ptr = @ptrCast(&resolver.future),
        .len = 1,
    };
}

/// Assumes `val` retained +1.
pub fn completeFuture(vm: *cy.VM, future: *cy.heap.Future, val: cy.Value) !void {
    future.val = val;
    future.completed = true;

    // Copy continuations to the ready queue.
    var opt_node = future.cont_head;
    while (opt_node) |node| {
        try vm.ready_tasks.writeItem(node.task);
        opt_node = node.next;
        vm.alloc.destroy(node);
    }
}

pub fn futureResolverComplete(vm: *cy.VM) anyerror!Value {
    const resolver = vm.getHostObject(*cy.heap.FutureResolver, 0);
    const future = resolver.future.castHostObject(*cy.heap.Future);
    if (!future.completed) {
        const value = vm.getValue(1);
        vm.retain(value);
        try completeFuture(vm, future, value);
    } 
    return cy.Value.Void;
}

pub fn futureResolverFuture(vm: *cy.VM) anyerror!Value {
    const resolver = vm.getHostObject(*cy.heap.FutureResolver, 0);
    vm.retain(resolver.future);
    return resolver.future;
}

pub fn futureResolverNew(vm: *cy.VM) anyerror!Value {
    const future_t: cy.TypeId = @intCast(vm.getInt(0));
    const resolver_t: cy.TypeId = @intCast(vm.getInt(1));
    const future = try vm.allocFuture(future_t);
    return vm.allocFutureResolver(resolver_t, future);
}

pub fn DefaultMemory_alloc(vm: *cy.VM) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const size: usize = @intCast(vm.getInt(1));
    const ptr = std.c.malloc(size);
    const ptr_v = Value.initRaw(@intCast(@intFromPtr(ptr)));

    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.PtrSliceByte, &.{ ptr_v, Value.initInt(@intCast(size)) });
}

pub fn DefaultMemory_free(vm: *cy.VM) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const slice = vm.getObject(*cy.heap.Object, 1);
    const addr: usize = @intCast(slice.getValue(0).val);
    const ptr: [*]u8 = @ptrFromInt(addr);
    std.c.free(ptr);
    return Value.Void;
}

fn Slice_startsWith(vm: *cy.VM) Value {
    const slice = vm.getObject(*cy.heap.Array, 0).getSlice();
    const needle = vm.getArray(1);
    return Value.initBool(std.mem.startsWith(u8, slice, needle));
}

fn Slice_endsWith(vm: *cy.VM) Value {
    const slice = vm.getObject(*cy.heap.Array, 0).getSlice();
    const needle = vm.getArray(1);
    return Value.initBool(std.mem.endsWith(u8, slice, needle));
}

fn arrayConcat(vm: *cy.VM) Value {
    const slice = vm.getArray(0);
    const rslice = vm.getArray(1);
    return vm.allocArrayConcat(slice, rslice) catch fatal();
}

fn arraySlice(vm: *cy.VM) anyerror!Value {
    _ = vm;
    // const arr = vm.getObject(*cy.heap.Object, 0);
    // const elems = arr.getElemsPtr();
    // const slice_t: cy.TypeId = @intCast(vm.getInt(1));

    // const range = vm.getObject(*cy.heap.Range, 2);
    // if (range.start < 0) {
    //     return error.OutOfBounds;
    // }

    // if (range.end > arr.len) {
    //     return error.OutOfBounds;
    // }
    // if (range.end < range.start) {
    //     return error.OutOfBounds;
    // }
    // const start: usize = @intCast(range.start);
    // return vm.allocRefSlice(slice_t, elems + start, @intCast(range.end - range.start));
}

pub fn intAsIndex(i: i64, len: usize) !usize {
    if (i < 0) {
        return error.OutOfBounds;
    }
    const u: u64 = @bitCast(i);
    if (u >= len) {
        return error.OutOfBounds;
    }
    return @intCast(i);
}

fn arrayInsert(vm: *cy.VM) anyerror!Value {
    const slice = vm.getArray(0);
    const idx = try intAsIndex(vm.getInt(1), slice.len+1);
    const insert = vm.getArray(2);
    const new = vm.allocUnsetArrayObject(slice.len + insert.len) catch cy.fatal();
    const buf = new.array.getMutSlice();
    @memcpy(buf[0..idx], slice[0..idx]);
    @memcpy(buf[idx..idx+insert.len], insert);
    @memcpy(buf[idx+insert.len..], slice[idx..]);
    return Value.initNoCycPtr(new);
}

fn Slice_find(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.Array, 0);
    const slice = obj.getSlice();
    const needle = vm.getArray(1);
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

fn String_decode(vm: *cy.VM) Value {
    vm.setSymbol(1, @intFromEnum(Symbol.utf8));
    return String_decode2(vm);
}

fn String_decode2(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.Array, 0);

    const encoding = bindings.getBuiltinSymbol(vm.getSymbol(1)) orelse {
        return rt.prepThrowError(vm, .InvalidArgument);
    };
    if (encoding != Symbol.utf8) {
        return rt.prepThrowError(vm, .InvalidArgument);
    }

    const parent = obj.getParent();
    const slice = obj.getSlice();
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

fn arrayIndex(vm: *cy.VM) anyerror!Value {
    const arr = vm.getObject(*cy.heap.Object, 0);
    const n: usize = @intCast(vm.getInt(1));
    const elem_t: cy.TypeId = @intCast(vm.getInt(2));
    const idx = try intAsIndex(vm.getInt(3), n);

    const elems = arr.getValuesPtr();
    if (vm.sema.isUnboxedType(elem_t)) {
        // Always an 8 byte stride.
        return Value.initRaw(@intFromPtr(elems + idx));
    } else {
        const type_e = vm.c.types[elem_t];
        const elem_size = type_e.data.struct_t.nfields;
        return Value.initRaw(@intFromPtr(elems + idx * elem_size));
    }
}

fn stringGetByte(vm: *cy.VM) anyerror!Value {
    const slice = vm.getString(0);
    const idx = vm.getInt(1);

    if (idx < 0 or idx >= slice.len) return error.OutOfBounds;
    return Value.initByte(slice[@intCast(idx)]);
}

fn String_getInt(vm: *cy.VM) anyerror!Value {
    const bytes = vm.getString(0);
    const idx = vm.getInt(1);
    const sym = try std.meta.intToEnum(Symbol, vm.getSymbol(2));
    const endian: std.builtin.Endian = switch (sym) {
        .little => .little,
        .big => .big,
        else => return error.InvalidArgument,
    };

    if (idx < 0 or idx + 8 > bytes.len) return error.OutOfBounds;
    const uidx: usize = @intCast(idx);
    const val = std.mem.readVarInt(u64, bytes[uidx..uidx+8], endian);
    return Value.initInt(@bitCast(val));
}

fn String_getInt32(vm: *cy.VM) anyerror!Value {
    const bytes = vm.getString(0);
    const idx = vm.getInt(1);
    const sym = try std.meta.intToEnum(Symbol, vm.getSymbol(2));
    const endian: std.builtin.Endian = switch (sym) {
        .little => .little,
        .big => .big,
        else => return error.InvalidArgument,
    };

    if (idx < 0 or idx + 4 > bytes.len) return error.OutOfBounds;
    const uidx: usize = @intCast(idx);
    const val = std.mem.readVarInt(u32, bytes[uidx..uidx+4], endian);
    return Value.initInt(@intCast(val));
}

fn stringFindAnyByte(vm: *cy.VM) Value {
    const slice = vm.getString(0);
    const set_slice = vm.getObject(*cy.heap.Object, 1);
    const set_ptr: [*]u8 = @ptrFromInt(@as(usize, @intCast(set_slice.getValue(0).val)));
    const set_len: usize = @intCast(set_slice.getValue(1).asInt());
    const set = set_ptr[0..set_len];
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

fn stringFindByte(vm: *cy.VM) Value {
    const slice = vm.getString(0);
    const byte = vm.getByte(1);

    if (cy.string.indexOfChar(slice, @intCast(byte))) |idx| {
        return intSome(vm, @intCast(idx)) catch cy.fatal();
    }
    return intNone(vm) catch cy.fatal();
}

fn stringFmtBytes(vm: *cy.VM) anyerror!Value {
    const str = vm.getString(0);
    const kind = try std.meta.intToEnum(NumberFormat, vm.getEnumValue(1));
    if (kind == .asc) {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(vm.alloc);

        for (str) |byte| {
            if (byte < 0 or byte > 127) {
                return error.InvalidArgument;
            }
        }
        const strv = vm.getValue(0);
        vm.retain(strv);
        return strv;
    } else {
        var base: u8 = undefined;
        var width: u8 = undefined;
        switch (kind) {
            .bin => {
                base = 2;
                width = 8;
            },
            .oct => {
                base = 8;
                width = 3;
            },
            .dec => {
                base = 10;
                width = 3;
            },
            .hex => {
                base = 16;
                width = 2;
            },
            else => return error.InvalidArgument,
        }

        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(vm.alloc);
        const w = buf.writer(vm.alloc);
        for (str) |byte| {
            try std.fmt.formatInt(byte, base, .lower, .{ .width = width, .fill = '0' }, w);
        }
        return vm.retainOrAllocAstring(buf.items);
    }
}

fn sliceTrim(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.Array, 0);
    const slice = obj.getSlice();

    const trimRunes = vm.getArray(2);

    var res: []const u8 = undefined;
    const mode = bindings.getBuiltinSymbol(vm.getSymbol(1)) orelse {
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

fn arrayRepeat(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.Array, 0);
    const slice = obj.getSlice();

    const n = vm.getInt(1);
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
            vm.retainObject(@ptrCast(obj));
            return Value.initNoCycPtr(obj);
        }
    }
}

fn sliceSplit(vm: *cy.VM) anyerror!Value {
    const obj = vm.getObject(*cy.heap.Array, 0);
    const slice = obj.getSlice();
    const delim = vm.getArray(1);

    const res = try vm.allocEmptyListDyn();
    if (delim.len == 0) {
        return res;
    }
    const list = res.asHeapObject();

    const parent = obj.getParent();
    var iter = std.mem.split(u8, slice, delim);
    while (iter.next()) |part| {
        vm.retainObject(parent);
        const new = try vm.allocArraySlice(part, parent);
        try list.list.append(vm.alloc, new);
    }
    return res;
}

fn fiberStatus(vm: *cy.VM) Value {
    const fiber = vm.getObject(*vmc.Fiber, 0);

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

fn Future_new(vm: *cy.VM) anyerror!Value {
    const type_id: cy.TypeId = @intCast(vm.getInt(0));
    return vm.allocFuture(type_id);
}

fn Future_complete(vm: *cy.VM) anyerror!Value {
    const type_id: cy.TypeId = @intCast(vm.getInt(1));
    const future = try vm.allocFuture(type_id);
    const value = vm.getValue(0);
    vm.retain(value);
    future.castHostObject(*cy.heap.Future).val = value;
    future.castHostObject(*cy.heap.Future).completed = true;
    return future;
}

fn pointerAsObject(vm: *cy.VM) Value {
    const ptr = vm.getPointer(0);
    vm.retainObject(@ptrCast(@alignCast(ptr)));
    return Value.initPtr(ptr);
}

fn pointerIndex(vm: *cy.VM) anyerror!Value {
    const ptr: [*]cy.Value = @ptrCast(@alignCast(vm.getPointer(0)));
    const elem_t: cy.TypeId = @intCast(vm.getInt(1));
    const idx: usize = @intCast(vm.getInt(2));

    const elem_te = vm.sema.getType(elem_t);
    if (elem_te.kind == .struct_t) {
        const n = elem_te.data.struct_t.nfields;
        return Value.initRaw(@intCast(@intFromPtr(ptr + idx*n)));
    } else {
        // Always an 8 byte stride.
        return Value.initRaw(@intCast(@intFromPtr(ptr + idx)));
    }
}

fn pointerIndexRange(vm: *cy.VM) anyerror!Value {
    const ptr: [*]cy.Value = @ptrCast(@alignCast(vm.getPointer(0)));
    const slice_t: cy.TypeId = @intCast(vm.getInt(1));
    const range = vm.getObject(*cy.heap.Range, 2);
    if (range.end < range.start) {
        return error.InvalidArgument;
    }
    if (range.start > 0) {
        return vm.allocSlice(slice_t, ptr + @as(usize, @intCast(range.start)), @intCast(range.end - range.start));
    } else {
        return vm.allocSlice(slice_t, ptr - @as(usize, @intCast(range.start)), @intCast(range.end - range.start));
    }
}

fn pointerSetIndex(vm: *cy.VM) anyerror!Value {
    const ptr: [*]cy.Value = @ptrCast(@alignCast(vm.getPointer(0)));
    const elem_t: cy.TypeId = @intCast(vm.getInt(1));
    const idx: usize = @intCast(vm.getInt(2));
    const val = vm.getValue(3);

    const elem_te = vm.sema.getType(elem_t);
    if (elem_te.kind == .struct_t) {
        const n = elem_te.data.struct_t.nfields;
        const dst = ptr[idx*n..idx*n+n];
        const src = val.asHeapObject().object.getValuesPtr()[0..n];
        @memcpy(dst, src);
    } else {
        if (vm.sema.isUnboxedType(elem_t)) {
            vm.release(ptr[idx]);
            vm.retain(val);
        }

        // Always an 8 byte stride.
        ptr[idx] = val;
    }
    return Value.Void;
}

fn pointerAddr(vm: *cy.VM) Value {
    return vm.getValue(0);
}

fn pointerFromCstr(vm: *cy.VM) anyerror!Value {
    if (cy.isWasm) return vm.prepPanic("Unsupported.");
    const ptr: [*]const u8 = @ptrCast(vm.getPointer(0));
    const off: u64 = @bitCast(vm.getInt(1));
    const bytes = std.mem.span(@as([*:0]const u8, @ptrCast(ptr + off)));
    return vm.allocString(bytes);
}

fn pointerGet(vm: *cy.VM) anyerror!Value {
    const ptr = vm.getPointer(0);
    const off = vm.getInt(1);
    const ctype = try std.meta.intToEnum(Symbol, vm.getSymbol(2));

    const uoff: u64 = @bitCast(off);
    switch (ctype) {
        .voidPtr => {
            const addr: usize = @intFromPtr(ptr) + @as(usize, @intCast(uoff));
            const val = @as(*?*anyopaque, @ptrFromInt(addr)).*;
            const data = vm.getData(*BuiltinsData, "builtins");
            return vm.allocPointer(data.PtrVoid, val);
        },
        else => {
            return error.InvalidArgument;
        }
    }
}

fn pointerSet(vm: *cy.VM) anyerror!Value {
    const ptr = vm.getPointer(0);
    const idx: usize = @intCast(vm.getInt(1));
    const ctype = try std.meta.intToEnum(Symbol, vm.getSymbol(2));
    const val = vm.getValue(3);
    const valT = val.getTypeId();
    switch (ctype) {
        .int => {
            switch (valT) {
                bt.Integer => {
                    const addr: usize = @intFromPtr(ptr) + idx;
                    @as(*i32, @ptrFromInt(addr)).* = @intCast(val.asBoxInt());
                    return Value.Void;
                },
                else => {
                    return error.InvalidArgument;
                }
            }
        },
        .voidPtr => {
            switch (valT) {
                bt.ExternFunc => {
                    const addr: usize = @intFromPtr(ptr) + idx;
                    @as(*?*anyopaque, @ptrFromInt(addr)).* = val.asHeapObject().externFunc.ptr;
                    return Value.Void;
                },
                else => {
                    if (vm.c.types[valT].kind == .int) {
                        const addr: usize = @intFromPtr(ptr) + idx;
                        const right_addr: usize = @intCast(val.asBoxInt());
                        @as(*?*anyopaque, @ptrFromInt(addr)).* = @ptrFromInt(right_addr);
                        return Value.Void;
                    }
                    return error.InvalidArgument;
                }
            }
        },
        else => {
            return error.InvalidArgument;
        }
    }
}

fn pointerGetString(vm: *cy.VM) anyerror!Value {
    const ptr: [*]const u8 = @ptrCast(vm.getPointer(0));
    const off: usize = @intCast(vm.getInt(1));
    const len: usize = @intCast(vm.getInt(2));
    return vm.allocString(ptr[off..@intCast(off+len)]);
}

fn pointerCall(vm: *cy.VM) anyerror!Value {
    const val = vm.getInt(0);
    const addr: usize = @intCast(val);
    return Value.initRaw(@intCast(addr));
}

fn externFuncPtr(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.ExternFunc, 0);
    return Value.initRaw(@intFromPtr(obj.ptr));
}

fn externFuncAddr(vm: *cy.VM) Value {
    const obj = vm.getObject(*cy.heap.ExternFunc, 0);
    return Value.initInt(@bitCast(@as(u64, @intFromPtr(obj.ptr))));
}

fn errorSym(vm: *cy.VM) Value {
    const recv = vm.getValue(0);
    return Value.initSymbol(recv.asErrorSymbol());
}

fn errorCall(vm: *cy.VM) Value {
    const val = vm.getValue(0);
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

pub const NumberFormat = enum {
    asc,
    bin,
    dec,
    hex,
    oct,
};

fn intFmt(vm: *cy.VM) anyerror!Value {
    const val = vm.getInt(0);
    const format = try std.meta.intToEnum(NumberFormat, vm.getEnumValue(1));
    return intFmtExt(vm, val, format, .{});
}

fn intFmt2(vm: *cy.VM) anyerror!Value {
    const val = vm.getInt(0);
    const format = try std.meta.intToEnum(NumberFormat, vm.getEnumValue(1));
    const optsv = vm.getObject(*cy.heap.Table, 2);
    const opts = try getIntFmtOptions(optsv);
    return intFmtExt(vm, val, format, opts);
}

pub fn getIntFmtOptions(optsv: *cy.heap.Table) !IntFmtOptions {
    var opts: IntFmtOptions = .{};
    if (optsv.map().getByString("pad")) |pad| {
        if (!pad.isBoxInt()) return error.InvalidArgument;
        const padv = pad.asBoxInt();
        if (padv < 0 or padv > 127) return error.InvalidArgument;
        opts.pad = @intCast(padv);
    }
    if (optsv.map().getByString("width")) |width| {
        if (!width.isBoxInt()) return error.InvalidArgument;
        const widthv = width.asBoxInt();
        if (widthv < 0) return error.InvalidArgument;
        opts.width = @intCast(widthv);
    }
    return opts;
}

const IntFmtOptions = struct {
    pad: ?u8 = null,
    width: ?usize = null,
};

pub fn intFmtExt(vm: *cy.VM, val: i64, format: NumberFormat, opts: IntFmtOptions) !Value {
    if (format == .asc) {
        if (val < 0 or val > 127) {
            return error.InvalidArgument;
        }
        const uchar: u8 = @intCast(val);
        return vm.retainOrAllocAstring(&.{uchar});
    } else {
        const base: u8 = switch (format) {
            .bin => 2,
            .oct => 8,
            .dec => 10,
            .hex => 16,
            else => return error.InvalidArgument,
        };
        var buf: [64]u8 = undefined;
        var fb = std.io.fixedBufferStream(&buf);
        if (val < 0) {
            try std.fmt.formatInt(val, base, .lower, .{ .fill = opts.pad orelse ' ', .width = opts.width }, fb.writer());
        } else {
            try std.fmt.formatInt(@as(u64, @bitCast(val)), base, .lower, .{ .fill = opts.pad orelse ' ', .width = opts.width }, fb.writer());
        }
        return vm.retainOrAllocAstring(fb.getWritten());
    }
}

fn intCall(vm: *cy.VM) Value {
    const val = vm.getValue(0);
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
            return Value.initInt(val.asHeapObject().integer.val);
        },
        else => {
            if (val.isEnum()) {
                return Value.initInt(val.getEnumValue());
            }
            return Value.initInt(0);
        }
    }
}

fn boolCall(vm: *cy.VM) Value {
    return Value.initBool(vm.getValue(0).toBool());
}

fn floatCall(vm: *cy.VM) Value {
    const val = vm.getValue(0);
    switch (val.getTypeId()) {
        bt.Float => return val,
        bt.String => {
            const res = std.fmt.parseFloat(f64, val.asString()) catch {
                return Value.initF64(0);
            };
            return Value.initF64(res);
        },
        bt.Symbol => return Value.initF64(@floatFromInt(val.val & @as(u64, 0xFF))),
        bt.Integer => return Value.initF64(@floatFromInt(val.asBoxInt())),
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
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionInt, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn intSome(vm: *cy.VM, v: i48) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionInt, &.{ Value.initInt(1), Value.initInt(v) });
}

pub fn anyNone(vm: *cy.VM) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionAny, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn anySome(vm: *cy.VM, v: Value) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionAny, &.{ Value.initInt(1), v });
}

pub fn optionNone(vm: *cy.VM, option_t: cy.TypeId) !Value {
    return vm.allocObjectSmall(option_t, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn optionSome(vm: *cy.VM, option_t: cy.TypeId, v: Value) !Value {
    return vm.allocObjectSmall(option_t, &.{ Value.initInt(1), v });
}

pub fn TupleNone(vm: *cy.VM) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionTuple, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn TupleSome(vm: *cy.VM, v: Value) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionTuple, &.{ Value.initInt(1), v });
}

pub fn MapNone(vm: *cy.VM) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionMap, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn MapSome(vm: *cy.VM, v: Value) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionMap, &.{ Value.initInt(1), v });
}

pub fn StringNone(vm: *cy.VM) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionString, &.{ Value.initInt(0), Value.initInt(0) });
}

pub fn StringSome(vm: *cy.VM, v: Value) !Value {
    const data = vm.getData(*BuiltinsData, "builtins");
    return vm.allocObjectSmall(data.OptionString, &.{ Value.initInt(1), v });
}

fn ExprType_getType(vm: *cy.VM) anyerror!Value {
    const val = vm.getObject(*cy.heap.Type, 0);
    return vm.allocType(val.type);
}
