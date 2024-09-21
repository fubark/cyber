const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const fatal = cy.fatal;
const cy = @import("../cyber.zig");
const C = @import("../capi.zig");
const CS = @import("../capi_shim.zig");
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
    ValueT: *cy.Type,
    VMT: *cy.Type,
    EvalResultT: *cy.Type,
    EvalConfigT: *cy.Type,
};

const func = cy.hostFuncEntry;
const funcs = [_]C.HostFuncEntry{
    // Utils.
    func("allTypes",       zErrFunc(allTypes)),
    func("bitcast_",       zErrFunc(bitcast)),
    func("copy_",          zErrFunc(copy)),
    func("choicetag_",     zErrFunc(choicetag)),
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
    func("collectCycles",  zErrFunc(collectCycles)),
    func("ptrcast_",       zErrFunc(ptrcast)),
    func("print",          print),
    func("refcast",        zErrFunc(refcast)),
    func("queueTask",      zErrFunc(queueTask)),
    func("runestr",        zErrFunc(runestr)),
    func("sizeof_",        sizeof),
    func("typeInfo",       zErrFunc(typeInfo)),

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
    func("float.fmt",      zErrFunc(float_fmt)),

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
    func("List.fill_",       listFill),

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
    func("String.$infix+",     zErrFunc(string.concat)),
    func("String.$infix+_any", zErrFunc(string.concatAny)),
    func("String.concat",      zErrFunc(string.concat)),
    func("String.concat_any",  zErrFunc(string.concatAny)),
    func("String.count",       string.count),
    // func("String.decode",       String_decode),
    // func("String.decode2",      String_decode2),
    func("String.endsWith",    string.endsWith),
    func("String.find",        string.find),
    func("String.findAnyByte", stringFindAnyByte),
    func("String.findAnyRune", zErrFunc(string.findAnyRune)),
    func("String.findByte",    stringFindByte),
    func("String.findRune",    string.findRune),
    func("String.fmt",         zErrFunc(string_fmt)),
    func("String.fmt2",        zErrFunc(string_fmt2)),
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
    func("String.interpolate", zErrFunc(string.string_interpolate)),

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
    func("pointer.fromAddr",   zErrFunc(pointer_fromAddr)),
    func("pointer.fromRef",    zErrFunc(pointer_fromRef)),

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

fn createFuncSymType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const sig: cy.sema.FuncSigId = @intCast(ctx.ct_params.get("SIG").?.asBoxInt());

    const new_t = c.sema.createType(.func_sym, .{ .sig = sig }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createFuncUnionType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const sig: cy.sema.FuncSigId = @intCast(ctx.ct_params.get("SIG").?.asBoxInt());

    const new_t = c.sema.createType(.func_union, .{ .sig = sig }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createFuncPtrType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const sig: cy.sema.FuncSigId = @intCast(ctx.ct_params.get("SIG").?.asBoxInt());

    const new_t = c.sema.createType(.func_ptr, .{ .sig = sig }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createRefType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;

    const new_t = c.sema.createType(.pointer, .{ .ref = true, .child_t = c.sema.getType(child_t) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createPointerType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;
    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;

    const new_t = c.sema.createType(.pointer, .{ .ref = false, .child_t = c.sema.getType(child_t) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createIntType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.int, bt.Integer, .{ .bits = 64 }) catch @panic("error");
    c.sema.int_t = new_t;
    return @ptrCast(new_t);
}

fn createFloatType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.float, bt.Float, .{ .bits = 64 }) catch @panic("error");
    c.sema.float_t = new_t;
    return @ptrCast(new_t);
}

fn createByteType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.int, bt.Byte, .{ .bits = 8 }) catch @panic("error");
    c.sema.byte_t = new_t;
    return @ptrCast(new_t);
}

fn createBoolType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = vm;
    _ = decl;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    const new_t = c.sema.createTypeWithId(.bool, bt.Boolean, .{}) catch @panic("error");
    c.sema.bool_t = new_t;
    return @ptrCast(new_t);
}

fn createOptionType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;

    const new_t = c.sema.createType(.option, .{ .child_t = c.sema.getType(child_t) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createArrayType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const n: usize = @intCast(ctx.ct_params.get("N").?.asBoxInt());
    const elem_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;

    const new_t = c.sema.createType(.array, .{ .n = n, .elem_t = c.sema.getType(elem_t) }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createListType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;
    _ = child_t;
    const new_t = c.sema.createType(.hostobj, .{
        .getChildrenFn = listGetChildren,
        .finalizerFn = listFinalizer,
    }) catch @panic("error");
    return @ptrCast(new_t);
}

fn createListIterType(vm: ?*C.VM, c_mod: C.Sym, decl: C.Node) callconv(.C) *C.Type {
    _ = decl;
    _ = vm;
    const chunk_sym = cy.Sym.fromC(c_mod).cast(.chunk);
    const c = chunk_sym.chunk;

    var ctx = cy.sema.getResolveContext(c);
    const child_t = ctx.ct_params.get("T").?.castHeapObject(*cy.heap.Type).type;
    _ = child_t;
    const new_t = c.sema.createType(.hostobj, .{
        .getChildrenFn = listIterGetChildren,
    }) catch @panic("error");
    return @ptrCast(new_t);
}

pub const BuiltinsData = struct {
    OptionInt: *cy.Type,
    OptionAny: *cy.Type,
    OptionTuple: *cy.Type,
    OptionMap: *cy.Type,
    OptionString: *cy.Type,
    PtrVoid: *cy.Type,
    PtrSliceByte: *cy.Type,
};

pub fn create(vm: *cy.VM, r_uri: []const u8) C.Module {
    const aot = cy.isAot(vm.compiler.config.backend);
    const src = if (aot) C.toStr(Src) else C.toStr(VmSrc);
    const mod = C.createModule(@ptrCast(vm), C.toStr(r_uri), src);

    const htype = C.hostTypeEntry;
    const vm_types = [_]C.HostTypeEntry{
        htype("void",           CS.RESERVE_HOBJ_TYPE(bt.Void, &vm.sema.void_t)),
        htype("bool",           C.CREATE_TYPE(createBoolType)),
        htype("symbol",         CS.RESERVE_DECL_TYPE(bt.Symbol, &vm.sema.symbol_t)),
        htype("error",          CS.RESERVE_DECL_TYPE(bt.Error, &vm.sema.error_t)),
        htype("int",            C.CREATE_TYPE(createIntType)),
        htype("float",          C.CREATE_TYPE(createFloatType)), 
        htype("placeholder1",   CS.RESERVE_HOBJ_TYPE(bt.Placeholder1, &vm.sema.placeholder1_t)), 
        htype("placeholder2",   CS.RESERVE_HOBJ_TYPE(bt.Placeholder2, &vm.sema.placeholder2_t)), 
        htype("placeholder3",   CS.RESERVE_HOBJ_TYPE(bt.Placeholder3, &vm.sema.placeholder3_t)), 
        htype("placeholder4",   CS.RESERVE_HOBJ_TYPE(bt.Placeholder4, &vm.sema.placeholder4_t)), 
        htype("placeholder5",   CS.RESERVE_HOBJ_TYPE(bt.Placeholder5, &vm.sema.placeholder5_t)), 
        htype("byte",           C.CREATE_TYPE(createByteType)), 
        htype("taglit",         CS.RESERVE_HOBJ_TYPE(bt.TagLit, &vm.sema.taglit_t)), 
        htype("dyn",            CS.RESERVE_HOBJ_TYPE(bt.Dyn, &vm.sema.dyn_t)),
        htype("any",            CS.RESERVE_HOBJ_TYPE(bt.Any, &vm.sema.any_t)),
        htype("type",           CS.RESERVE_HOBJ_TYPE(bt.Type, &vm.sema.type_t)),
        htype("ExprType",       CS.RESERVE_DECL_TYPE(bt.ExprType, &vm.sema.exprtype_t)),
        htype("List",           C.CREATE_TYPE(createListType)),
        htype("ListIterator",   C.CREATE_TYPE(createListIterType)),
        htype("Tuple",          CS.RESERVE_HOBJ_TYPE(bt.Tuple, &vm.sema.tuple_t)),
        htype("Table",          CS.RESERVE_DECL_TYPE(bt.Table, &vm.sema.table_t)),
        htype("Map",            CS.RESERVE_HOBJ_TYPE(bt.Map, &vm.sema.map_t)),
        htype("MapIterator",    CS.RESERVE_HOBJ_TYPE(bt.MapIter, null)),
        htype("String",         CS.RESERVE_HOBJ_TYPE(bt.String, &vm.sema.string_t)),
        htype("ExternFunc",     CS.RESERVE_HOBJ_TYPE(bt.ExternFunc, null)),
        htype("Fiber",          CS.RESERVE_HOBJ_TYPE(bt.Fiber, &vm.sema.fiber_t)),
        htype("Range",          CS.RESERVE_DECL_TYPE(bt.Range, &vm.sema.range_t)),
        htype("TccState",       CS.RESERVE_HOBJ_TYPE(bt.TccState, null)),
        htype("Future",         CS.HOBJ_TYPE(null, futureGetChildren, null)),
        htype("FutureResolver", CS.HOBJ_TYPE(null, futureResolverGetChildren, null)),
        htype("Memory",         CS.RESERVE_DECL_TYPE(bt.Memory, &vm.sema.memory_t)),
        htype("Array",          C.CREATE_TYPE(createArrayType)),
        htype("pointer",        C.CREATE_TYPE(createPointerType)),
        htype("ref",            C.CREATE_TYPE(createRefType)),
        htype("FuncSig",        CS.RESERVE_DECL_TYPE(bt.FuncSig, null)), 
        htype("funcptr_t",      C.CREATE_TYPE(createFuncPtrType)),
        htype("funcunion_t",    C.CREATE_TYPE(createFuncUnionType)),
        htype("Func",           CS.RESERVE_HOBJ_TYPE(bt.Func, null)),
        htype("funcsym_t",      C.CREATE_TYPE(createFuncSymType)),
        htype("Option",         C.CREATE_TYPE(createOptionType)),
    };

    var config = C.ModuleConfig{
        .types = if (aot) C.toSlice(C.HostTypeEntry, &types) else C.toSlice(C.HostTypeEntry, &vm_types),
        .funcs = C.toSlice(C.HostFuncEntry, &funcs),
        .onLoad = onLoad,
    };
    C.setModuleConfig(@ptrCast(vm), mod, &config);
    return mod;
}

fn onLoad(vm: ?*C.VM, mod: C.Sym) callconv(.C) void {
    onLoadZ(@ptrCast(@alignCast(vm)), cy.Sym.fromC(mod)) catch @panic("error");
}

fn onLoadZ(vm: *cy.VM, mod: *cy.Sym) !void {
    log.tracev("builtins: on load", .{});
    const chunk_sym = mod.cast(.chunk);
    const b = bindings.ModuleBuilder.init(vm.compiler, @ptrCast(chunk_sym));
    if (cy.Trace) {
        try b.declareFuncSig("traceRetains", &.{}, vm.sema.int_t, traceRetains);
        try b.declareFuncSig("traceReleases", &.{}, vm.sema.int_t, traceRetains);
    }

    const data = vm.getData(*BuiltinsData, "builtins");

    const option_tmpl = chunk_sym.getMod().getSym("Option").?.cast(.template);

    const assert = std.debug.assert;
    _ = assert;

    const int_t = try vm.allocType(bt.Integer);
    defer vm.release(int_t);
    data.OptionInt = (try vm.expandTemplateType(option_tmpl, &.{int_t})).?;

    const any_t = try vm.allocType(bt.Any);
    defer vm.release(any_t);
    data.OptionAny = (try vm.expandTemplateType(option_tmpl, &.{any_t})).?;

    const tuple_t = try vm.allocType(bt.Tuple);
    defer vm.release(tuple_t);
    data.OptionTuple = (try vm.expandTemplateType(option_tmpl, &.{tuple_t})).?;

    const map_t = try vm.allocType(bt.Map);
    defer vm.release(map_t);
    data.OptionMap = (try vm.expandTemplateType(option_tmpl, &.{map_t})).?;

    const string_t = try vm.allocType(bt.String);
    defer vm.release(string_t);
    data.OptionString = (try vm.expandTemplateType(option_tmpl, &.{string_t})).?;

    const pointer_tmpl = chunk_sym.getMod().getSym("pointer").?.cast(.template);

    const void_t = try vm.allocType(bt.Void);
    defer vm.release(void_t);
    data.PtrVoid = (try vm.expandTemplateType(pointer_tmpl, &.{void_t})).?;

    const list_tmpl = chunk_sym.getMod().getSym("List").?.cast(.template);

    const dynamic_t = try vm.allocType(bt.Dyn);
    defer vm.release(dynamic_t);
    var temp: *cy.Type = undefined;
    temp = (try vm.expandTemplateType(list_tmpl, &.{dynamic_t})).?;

    const list_iter_tmpl = chunk_sym.getMod().getSym("ListIterator").?.cast(.template);
    temp = (try vm.expandTemplateType(list_iter_tmpl, &.{dynamic_t})).?;

    const ptr_slice_tmpl = chunk_sym.getMod().getSym("PtrSlice").?.cast(.template);
    const byte_t = try vm.allocType(bt.Byte);
    defer vm.release(byte_t);
    data.PtrSliceByte = (try vm.expandTemplateType(ptr_slice_tmpl, &.{byte_t})).?;

    // Verify all core types have been initialized.
    if (cy.Trace) {
        if (vm.sema.types.items[0].kind() != .null) {
            cy.panicFmt("Expected null type.", .{});
        }
        for (1..cy.types.BuiltinEnd) |i| {
            const type_e = vm.sema.types.items[i];
            if (type_e.kind() == .null) {
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
    const dst_tid: cy.TypeId = @intCast(vm.getInt(0));
    const src_tid: cy.TypeId = @intCast(vm.getInt(2));
    const dst_t = vm.getType(dst_tid);
    const src_t = vm.getType(src_tid);
    if (src_t.kind() != .int or dst_t.kind() != .int) {
        return error.InvalidArgument;
    }
    if (src_t.cast(.int).bits != dst_t.cast(.int).bits) {
        return error.InvalidArgument;
    }
    return vm.getValue(1);
}

pub fn copy(vm: *cy.VM) anyerror!Value {
    const type_id: cy.TypeId = @intCast(vm.getInt(0));
    const val = vm.getValue(1);
    return cy.value.shallowCopy(vm, type_id, val);
}

pub fn choicetag(vm: *cy.VM) anyerror!Value {
    const tag_t: cy.TypeId = @intCast(vm.getInt(0));
    const type_ = vm.getType(tag_t);
    if (type_.kind() != .enum_t) {
        return error.InvalidArgument;
    }
    const choice_tag = vm.getValue(1).castHeapObject(*cy.heap.Object).getValue(0).asInt();
    return cy.Value.initInt(choice_tag);
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
    if (type_e.kind() != .option) {
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

pub fn collectCycles(vm: *cy.VM) anyerror!Value {
    const res = try cy.arc.collectCycles(vm);
    const map = try vm.allocEmptyMap();
    const objKey = try vm.retainOrAllocAstring("num_obj_freed");
    const num_obj_freed = try vm.allocInt(@intCast(res.num_obj_freed));
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
    const type_ = vm.getType(type_id);
    if (type_.kind() == .struct_t) {
        return Value.initInt(8 * type_.cast(.struct_t).size);
    } else {
        return Value.initInt(8);
    }
}

pub fn allTypes(ivm: *cy.VM) !Value {
    const vm: *C.ZVM = @ptrCast(ivm);

    const list_tmpl = ivm.sema.list_tmpl.head.toC();
    const type_v = vm.newTypeById(bt.Type);
    defer vm.release(type_v);
    const list_t = vm.expandTemplateType(list_tmpl, &.{type_v}).?;

    const list_v = vm.newList(list_t, &.{});
    for (1..ivm.c.types_len) |id| {
        const type_ = ivm.getType(@intCast(id));
        if (type_.kind() == .bare or type_.kind() == .ct_ref) {
            continue;
        }
        const elem = vm.newTypeById(@intCast(id));
        vm.listAppend(list_v, elem);
        vm.release(elem);
    }
    return @bitCast(list_v);
}

pub fn typeInfo(vm: *cy.VM) !Value {
    const type_id = vm.getObject(*cy.heap.Type, 0).type;

    const info_t = (try vm.findType("TypeInfo")).?;
    switch (type_id) {
        bt.Void => {
            return vm.newChoice(info_t, "void_t", Value.Void);
        },
        bt.Type => {
            return vm.newChoice(info_t, "type_t", Value.Void);
        },
        bt.Error => {
            return vm.newChoice(info_t, "error_t", Value.Void);
        },
        else => {
            const type_ = vm.getType(type_id);
            switch (type_.kind()) {
                .float => {
                    const bits = type_.cast(.float).bits;
                    const float_info_t = (try vm.findType("FloatInfo")).?;
                    const float_info = try vm.newInstance(float_info_t, &.{
                        CS.toFieldInit("bits", Value.initInt(bits)),
                    });
                    return vm.newChoice(info_t, "float_t", float_info);
                },
                .pointer => {
                    const ptr_info_t = (try vm.findType("PointerInfo")).?;
                    const pointer = type_.cast(.pointer);
                    const child_t = try vm.newType(pointer.child_t);
                    const ptr_info = try vm.newInstance(ptr_info_t, &.{
                        CS.toFieldInit("child", child_t),
                        CS.toFieldInit("ref", Value.initBool(pointer.ref)),
                    });
                    return vm.newChoice(info_t, "ptr_t", ptr_info);
                },
                .int => {
                    const bits = type_.cast(.int).bits;
                    const int_info_t = (try vm.findType("IntInfo")).?;
                    const int_info = try vm.newInstance(int_info_t, &.{
                        CS.toFieldInit("sign", Value.initBool(type_id != bt.Byte)),
                        CS.toFieldInit("bits", Value.initInt(bits)),
                    });
                    return vm.newChoice(info_t, "int_t", int_info);
                },
                .bool => {
                    return vm.newChoice(info_t, "bool_t", Value.Void);
                },
                .trait => {
                    const name = type_.name();
                    const trait_info_t = (try vm.findType("TraitInfo")).?;
                    const trait_info = try vm.newInstance(trait_info_t, &.{
                        CS.toFieldInit("name", try vm.newString(name)),
                    });
                    return vm.newChoice(info_t, "trait_t", trait_info);
                },
                .array => {
                    const array_t = type_.cast(.array);
                    const array_info_t = (try vm.findType("ArrayInfo")).?;
                    const array_info = try vm.newInstance(array_info_t, &.{
                        CS.toFieldInit("len", Value.initInt(@intCast(array_t.n))),
                        CS.toFieldInit("elem", try vm.newType(array_t.elem_t)),
                    });
                    return vm.newChoice(info_t, "array_t", array_info);
                },
                .option => {
                    const opt_info_t = (try vm.findType("OptionInfo")).?;
                    const elem_t = type_.sym().variant.?.args[0];
                    vm.retain(@bitCast(elem_t));
                    const opt_info = try vm.newInstance(opt_info_t, &.{
                        CS.toFieldInit("elem", @bitCast(elem_t)),
                    });
                    return vm.newChoice(info_t, "opt_t", opt_info);
                },
                .choice => {
                    const choice_t = type_.cast(.choice);
                    const choice_info_t = (try vm.findType("ChoiceInfo")).?;
                    const choice_case_t = (try vm.findType("ChoiceCase")).?;
                    const list_t = (try vm.findType("List[ChoiceCase]")).?;
                    const list_v = try vm.newList(list_t, &.{});
                    for (choice_t.cases()) |case| {
                        const case_v = try vm.newInstance(choice_case_t, &.{
                            CS.toFieldInit("name", try vm.newString(case.head.name())),
                            CS.toFieldInit("type", try vm.newType(case.payload_t)),
                        });
                        try vm.listAppend(list_v, case_v);
                    }
                    const name = type_.name();
                    const namev = try StringSome(vm, try vm.newString(name));
                    const choice_info = try vm.newInstance(choice_info_t, &.{
                        CS.toFieldInit("name", namev),
                        CS.toFieldInit("cases", list_v),
                    });
                    return vm.newChoice(info_t, "choice_t", choice_info);
                },
                .enum_t => {
                    const enum_t = type_.cast(.enum_t);
                    const enum_info_t = (try vm.findType("EnumInfo")).?;
                    const enum_case_t = (try vm.findType("EnumCase")).?;
                    const list_t = (try vm.findType("List[EnumCase]")).?;
                    const list_v = try vm.newList(list_t, &.{});
                    for (enum_t.cases()) |case| {
                        const case_v = try vm.newInstance(enum_case_t, &.{
                            CS.toFieldInit("name", try vm.newString(case.head.name())),
                        });
                        try vm.listAppend(list_v, case_v);
                    }
                    const name = type_.name();
                    const namev = try StringSome(vm, try vm.newString(name));
                    const enum_info = try vm.newInstance(enum_info_t, &.{
                        CS.toFieldInit("name", namev),
                        CS.toFieldInit("cases", list_v),
                    });
                    return vm.newChoice(info_t, "enum_t", enum_info);
                },
                .hostobj => {
                    const hostobj_info_t = (try vm.findType("HostObjectInfo")).?;
                    const name = type_.name();
                    const hostobj_info = try vm.newInstance(hostobj_info_t, &.{
                        CS.toFieldInit("name", try vm.newString(name)),
                    });
                    return vm.newChoice(info_t, "hostobj_t", hostobj_info);
                },
                .struct_t => {
                    const struct_t = type_.cast(.struct_t);
                    const struct_info_t = (try vm.findType("StructInfo")).?;
                    const field_t = (try vm.findType("StructField")).?;
                    const list_t = (try vm.findType("List[StructField]")).?;
                    const list_v = try vm.newList(list_t, &.{});
                    for (struct_t.fields()) |field| {
                        const f = try vm.newInstance(field_t, &.{
                            CS.toFieldInit("name", try vm.newString(field.sym.head.name())),
                            CS.toFieldInit("type", try vm.newType(field.type)),
                            CS.toFieldInit("offset", Value.initInt(@intCast(field.offset))),
                        });
                        try vm.listAppend(list_v, f);
                    }
                    const name = type_.name();
                    const namev = try StringSome(vm, try vm.newString(name));
                    const struct_info = try vm.newInstance(struct_info_t, &.{
                        CS.toFieldInit("name", @bitCast(namev)),
                        CS.toFieldInit("fields", list_v),
                    });
                    return vm.newChoice(info_t, "struct_t", struct_info);
                },
                .func_ptr => {
                    const func_info = try newFuncInfo(vm, 0, type_.cast(.func_ptr).sig);
                    return vm.newChoice(info_t, "func_t", func_info);
                },
                .func_union => {
                    const func_info = try newFuncInfo(vm, 1, type_.cast(.func_union).sig);
                    return vm.newChoice(info_t, "func_t", func_info);
                },
                .func_sym => {
                    const func_info = try newFuncInfo(vm, 2, type_.cast(.func_sym).sig);
                    return vm.newChoice(info_t, "func_t", func_info);
                },
                else => {
                    std.debug.panic("Unsupported: {}", .{type_.kind()});
                }
            }
        },
    }
}

fn newFuncInfo(vm: *cy.VM, kind: u8, sig_id: cy.sema.FuncSigId) !Value {
    const func_info_t = (try vm.findType("FuncInfo")).?;
    const param_t = (try vm.findType("FuncParam")).?;
    const list_t = (try vm.findType("List[FuncParam]")).?;
    const list_v = try vm.newList(list_t, &.{});
    const sig = vm.sema.getFuncSig(sig_id);
    for (sig.params()) |param| {
        const p = try vm.newInstance(param_t, &.{
            CS.toFieldInit("type", try vm.newType(param)),
        });
        try vm.listAppend(list_v, p);
    }

    return vm.newInstance(func_info_t, &.{
        CS.toFieldInit("kind", Value.initInt(kind)),
        CS.toFieldInit("ret", try vm.newType(sig.ret)),
        CS.toFieldInit("params", list_v),
    });
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
    return vm.allocObjectSmall(data.PtrSliceByte.id(), &.{ ptr_v, Value.initInt(@intCast(size)) });
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

fn string_fmt(vm: *cy.VM) anyerror!Value {
    const format = vm.getObject(*cy.heap.String, 0);
    const args = vm.getObject(*cy.heap.List, 1).items();
    return stringFmt(vm, format.getSlice(), format.getType().isAstring(), "@", args);
}

fn string_fmt2(vm: *cy.VM) anyerror!Value {
    const format = vm.getObject(*cy.heap.String, 0);
    const placeholder = vm.getString(1);
    const args = vm.getObject(*cy.heap.List, 2).items();
    return stringFmt(vm, format.getSlice(), format.getType().isAstring(), placeholder, args);
}

fn stringFmt(vm: *cy.VM, format: []const u8, ascii_format: bool, placeholder: []const u8, args: []const cy.Value) !cy.Value {
    var ascii_args = true;
    const buf = &@as(*cy.VM, @ptrCast(vm)).u8Buf;
    buf.clearRetainingCapacity();
    defer buf.ensureMaxCapOrClear(vm.alloc, 4096) catch fatal();

    var pos: usize = 0;
    var num_ph: usize = 0;
    while (true) {
        // Find the next placeholder.
        const ph_pos = std.mem.indexOfPos(u8, format, pos, placeholder) orelse {
            if (num_ph != args.len) {
                return error.InvalidArgument;
            }
            try buf.appendSlice(vm.alloc, format[pos..]);
            break;
        };

        try buf.appendSlice(vm.alloc, format[pos..ph_pos]);

        if (num_ph + 1 > args.len) {
            return error.InvalidArgument;
        }

        var out_ascii: bool = undefined;
        const val_str = try vm.getOrBufPrintValueStr2(&cy.tempBuf, args[num_ph], &out_ascii);
        ascii_args = ascii_args and out_ascii;
        try buf.appendSlice(vm.alloc, val_str);

        pos = ph_pos + placeholder.len;
        num_ph += 1;
    }

    if (ascii_format and ascii_args) {
        return vm.retainOrAllocAstring(buf.items());
    } else {
        return vm.retainOrAllocUstring(buf.items());
    }
}

fn stringFmtBytes(vm: *cy.VM) anyerror!Value {
    const str = vm.getString(0);
    const kind = try std.meta.intToEnum(NumberFormat, vm.getInt(1));
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
    var iter = std.mem.splitSequence(u8, slice, delim);
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

    const elem_te = vm.getType(elem_t);
    if (elem_te.kind() == .struct_t) {
        const struct_t = elem_te.cast(.struct_t);
        const elem_size = struct_t.size;
        const dst = ptr[idx*elem_size..idx*elem_size+elem_size];
        const src = val.asHeapObject().object.getValuesPtr()[0..elem_size];
        @memcpy(dst, src);
    } else {
        if (!elem_te.isBoxed()) {
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
            return vm.allocPointer(data.PtrVoid.id(), val);
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
                    if (vm.getType(valT).isPointer()) {
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

fn pointer_fromAddr(vm: *cy.VM) anyerror!Value {
    const val = vm.getInt(0);
    const addr: usize = @intCast(val);
    return Value.initRaw(@intCast(addr));
}

fn pointer_fromRef(vm: *cy.VM) anyerror!Value {
    const ref_raw: u64 = @bitCast(vm.getInt(0));
    const addr: usize = (ref_raw & vmc.PTR_PAYLOAD_MASK) + 8;
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
        // } else if (val.isEnum()) {
        //     const enumT = val.getEnumType();
        //     const enumv = val.getEnumValue();
        //     const name = vm.c.types[enumT].sym.cast(.enum_t).getValueSym(enumv).head.name();
        //     const symId = vm.ensureSymbol(name) catch cy.unexpected();
        //     return Value.initErrorSymbol(symId);
        } else {
            return rt.prepThrowError(vm, .InvalidArgument);
        }
    }
}

fn float_fmt(vm: *cy.VM) anyerror!Value {
    const val = vm.getFloat(0);
    const BufSize = std.fmt.format_float.bufferSize(.decimal, f64);
    var buf: [BufSize]u8 = undefined;
    const str = try std.fmt.formatFloat(&buf, val, .{ .mode = .decimal });
    return vm.retainOrAllocAstring(str);
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
    const format = try std.meta.intToEnum(NumberFormat, vm.getInt(1));
    return intFmtExt(vm, val, format, .{});
}

fn intFmt2(vm: *cy.VM) anyerror!Value {
    const val = vm.getInt(0);
    const format = try std.meta.intToEnum(NumberFormat, vm.getInt(1));
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
            const type_e = vm.getType(val.getTypeId());
            if (type_e.kind() == .enum_t) {
                return Value.initInt(val.asBoxInt());
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
            const type_e = vm.getType(val.getTypeId());
            if (type_e.kind() == .enum_t) {
                return Value.initF64(@floatFromInt(val.asBoxInt()));
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
