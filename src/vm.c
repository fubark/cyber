#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include "vm.h"

#define STATIC_ASSERT(cond, msg) typedef char static_assertion_##msg[(cond)?1:-1]
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

#define VLOG(msg, ...) \
    do { if (TRACE && verbose) zPrintStderr(msg, (FmtValue[]){__VA_ARGS__}, sizeof((FmtValue[]){__VA_ARGS__})/sizeof(FmtValue)); } while (false)
#define LOG(msg, ...) \
    do { if (TRACE) zPrintStderr(msg, (FmtValue[]){__VA_ARGS__}, sizeof((FmtValue[]){__VA_ARGS__})/sizeof(FmtValue)); } while (false)
#define PRINT(msg, ...) \
    do { zPrintStderr(msg, (FmtValue[]){__VA_ARGS__}, sizeof((FmtValue[]){__VA_ARGS__})/sizeof(FmtValue)); } while (false)

#define ASSERT(cond) \
    do { if (!(cond)) zFatal(); } while (false)
#define BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)

// 1000000000000000: Most significant bit.
#define SIGN_MASK ((u64)1 << 63)

// 0111111111110000 (7FF0): +INF (from math.inf, +/zero, overflow)
// 1111111111110000 (FFF0): -INF (from math.neginf, -/zero, overflow)
// 0111111111111000 (7FF8): QNAN (from neg op on -QNAN, math.nan, zero/zero non-intel, QNAN arithmetic)
// 1111111111111000 (FFF8): -QNAN (from neg op on QNAN, zero/zero intel, -QNAN arithmetic, -QNAN/zero)
// Intel uses the sign bit for an indefinite real or -QNAN.

// 0111111111111100 (7FFC): QNAN and one extra bit to the right.
#define TAGGED_VALUE_MASK ((u64)0x7ffc000000000000)

// 0000000000000001
#define INTEGER_MASK ((u64)1 << 48)

// 0111111111111101 0000000000000111
#define TAGGED_PRIMITIVE_MASK (TAGGED_VALUE_MASK | PRIMITIVE_MASK)
// 0000000000000001 0000000000000111: Bits relevant to the primitive's type.
#define PRIMITIVE_MASK (TAGGED_VALUE_MASK | ((u64)TAG_MASK << 32) | INTEGER_MASK)

// 1111111111111100: TaggedMask + Sign bit indicates a pointer value.
#define POINTER_MASK (TAGGED_VALUE_MASK | SIGN_MASK)

// 0111111111111101 
#define TAGGED_INTEGER_MASK (TAGGED_VALUE_MASK | INTEGER_MASK)

// 0111111111111100 0000000000000000
#define NONE_MASK (TAGGED_VALUE_MASK | ((u64)TAG_NONE << 32))

// 0111111111111100 0000000000000001
#define BOOLEAN_MASK (TAGGED_VALUE_MASK | ((u64)TAG_BOOLEAN << 32))

#define TAG_MASK (((uint32_t)1 << 3) - 1)
#define TAG_NONE ((uint8_t)0)
#define TAG_BOOLEAN ((uint8_t)1)
#define TAG_ERROR ((uint8_t)2)
#define TAG_STATIC_ASTRING ((uint8_t)3)
#define TAG_STATIC_USTRING ((uint8_t)4)
#define TAG_ENUM ((uint8_t)5)
#define TAG_SYMBOL ((uint8_t)6)
#define FALSE_MASK BOOLEAN_MASK
#define TRUE_BIT_MASK ((uint64_t)1)
#define TRUE_MASK (BOOLEAN_MASK | TRUE_BIT_MASK)

#define ERROR_MASK (TAGGED_VALUE_MASK | ((u64)TAG_ERROR << 32))
#define ENUM_MASK (TAGGED_VALUE_MASK | ((u64)TAG_ENUM << 32))
#define SYMBOL_MASK (TAGGED_VALUE_MASK | ((u64)TAG_SYMBOL << 32))
#define STATIC_ASTRING_MASK (TAGGED_VALUE_MASK | ((u64)TAG_STATIC_ASTRING << 32))
#define STATIC_USTRING_MASK (TAGGED_VALUE_MASK | ((u64)TAG_STATIC_USTRING << 32))
#define BEFORE_TAG_MASK ((u32)(0x00007fff << 3))
#define NULL_U32 UINT32_MAX
#define NULL_U8 UINT8_MAX

// Construct value.

// _BitInt zeroes padding bits after cast.
#define VALUE_INTEGER_CAST(n) (TAGGED_INTEGER_MASK | (_BitInt(48))n)
// Assumes _BitInt(48) param. `or` op automatically generates trunc on n's undefined padding bits.
#define VALUE_INTEGER(n) (TAGGED_INTEGER_MASK | BITCAST(unsigned _BitInt(48), n))
#define VALUE_BOOLEAN(b) (b ? TRUE_MASK : FALSE_MASK)
#define VALUE_NONE NONE_MASK
#define VALUE_FLOAT(n) ((ValueUnion){ .d = n }.u)
#define VALUE_ENUM(tag, val) ((Value)(ENUM_MASK | tag << 8 | val ))
#define VALUE_RETINFO(nrv, rf, cio) ((Value)(nrv | ((u32)rf << 8) | ((u32)cio << 16)))
#define VALUE_TRUE TRUE_MASK
#define VALUE_FALSE FALSE_MASK
#define VALUE_INTERRUPT (ERROR_MASK | 0xffff) 
#define VALUE_RAW(u) u
#define VALUE_PTR(ptr) (POINTER_MASK | (uint64_t)ptr)
#define VALUE_STATIC_STRING_SLICE(v) ((IndexSlice){ .start = v & 0xffffffff, .len = (((u32)(v >> 32)) & BEFORE_TAG_MASK) >> 3 })
#define VALUE_SYMBOL(symId) (SYMBOL_MASK | symId)

// Value ops.
#define VALUE_AS_HEAPOBJECT(v) ((HeapObject*)(v & ~POINTER_MASK))
#define VALUE_AS_INTEGER(v) BITCAST(_BitInt(48), v) // Padding bits returned are undefined.
#define VALUE_AS_UINTEGER(v) BITCAST(unsigned _BitInt(48), v) // Padding bits returned are undefined.
#define VALUE_AS_U32(v) ((u32)(v & 0xffffffff))
#define VALUE_AS_FLOAT(v) ((ValueUnion){ .u = v }.d)
#define VALUE_AS_FLOAT_TO_INT(v) ((int32_t)VALUE_AS_FLOAT(v))
#define VALUE_AS_FLOAT_TO_INT64(v) ((int64_t)VALUE_AS_FLOAT(v))
#define VALUE_AS_BOOLEAN(v) (v == TRUE_MASK)
#define VALUE_ASSUME_NOT_BOOL_TO_BOOL(v) (!VALUE_IS_NONE(v))
#define VALUE_GET_TAG(v) ((BITCAST(u32, v >> 32)) & TAG_MASK)
#define VALUE_RETINFO_NUMRETVALS(v) (v & 0xff)
#define VALUE_RETINFO_RETFLAG(v) ((v & 0xff00) >> 8)

#define VALUE_IS_BOOLEAN(v) ((v & (TAGGED_PRIMITIVE_MASK | SIGN_MASK)) == BOOLEAN_MASK)
#define VALUE_IS_POINTER(v) (v >= POINTER_MASK)
#define VALUE_IS_CLOSURE(v) (VALUE_IS_POINTER(v) && (VALUE_AS_HEAPOBJECT(v)->head.typeId == TYPE_CLOSURE))
#define VALUE_IS_BOX(v) (VALUE_IS_POINTER(v) && (VALUE_AS_HEAPOBJECT(v)->head.typeId == TYPE_BOX))
#define VALUE_IS_NONE(v) (v == NONE_MASK)
#define VALUE_IS_FLOAT(v) ((v & TAGGED_VALUE_MASK) != TAGGED_VALUE_MASK)
#define VALUE_IS_ERROR(v) ((v & (TAGGED_PRIMITIVE_MASK | SIGN_MASK)) == ERROR_MASK)

#define VALUE_IS_LIST(v) (VALUE_IS_POINTER(v) && (VALUE_AS_HEAPOBJECT(v)->head.typeId == TYPE_LIST))
#define VALUE_IS_MAP(v) (VALUE_IS_POINTER(v) && (VALUE_AS_HEAPOBJECT(v)->head.typeId == TYPE_MAP))
#define VALUE_BOTH_FLOATS(a, b) (VALUE_IS_FLOAT(a) && VALUE_IS_FLOAT(b))
#define VALUE_IS_INTEGER(v) ((v & (TAGGED_INTEGER_MASK | SIGN_MASK)) == TAGGED_INTEGER_MASK)
#define VALUE_BOTH_INTEGERS(a, b) ((a & b & (TAGGED_INTEGER_MASK | SIGN_MASK)) == TAGGED_INTEGER_MASK)
#define VALUE_ALL3_INTEGERS(a, b, c) ((a & b & c & (TAGGED_INTEGER_MASK | SIGN_MASK)) == TAGGED_INTEGER_MASK)

#define FMT_STRZ(s) ((FmtValue){ .type = FMT_TYPE_STRING, .data = { .string = s }, .data2 = { .string = strlen(s) }})
#define FMT_STR(s) ((FmtValue){ .type = FMT_TYPE_STRING, .data = { .string = s.ptr }, .data2 = { .string = s.len }})
#define FMT_STRLEN(s, len) ((FmtValue){ .type = FMT_TYPE_STRING, .data = { .string = s }, .data2 = { .string = len }})
#define FMT_U32(v) ((FmtValue){ .type = FMT_TYPE_U32, .data = { .u32 = v }})

static inline TypeId getTypeId(Value val);
static Str getTypeName(VM* vm, TypeId id);

static inline bool valueAssumeNotPtrIsStaticString(Value v) {
    u64 mask = v & TAGGED_PRIMITIVE_MASK;
    return (mask == STATIC_ASTRING_MASK) || (mask == STATIC_USTRING_MASK);
}

static inline bool valueIsString(Value v) {
    if (VALUE_IS_POINTER(v)) {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(v);
        return (obj->head.typeId == TYPE_ASTRING) || (obj->head.typeId == TYPE_USTRING) || (obj->head.typeId == TYPE_STRING_SLICE);
    } else {
        return valueAssumeNotPtrIsStaticString(v);
    }
}

static inline bool valueIsRawString(Value v) {
    if (!VALUE_IS_POINTER(v)) {
        return false;
    }
    TypeId typeId = VALUE_AS_HEAPOBJECT(v)->head.typeId;
    return (typeId == TYPE_RAWSTRING) || (typeId == TYPE_RAWSTRING_SLICE);
}

static inline uintptr_t getInstOffset(VM* vm, Inst* to) {
    return ((uintptr_t)to) - ((uintptr_t)vm->instPtr);
}

static inline Value objectGetField(Object* obj, uint8_t idx) {
    return (&obj->firstValue)[idx];
}

static inline Value* objectGetFieldPtr(Object* obj, uint8_t idx) {
    return (&obj->firstValue) + idx;
}

static inline Value* objectGetValuesPtr(Object* obj) {
    return &obj->firstValue;
}

static inline Value* closureGetCapturedValuesPtr(Closure* closure) {
    return &closure->firstCapturedVal;
}

static inline void release(VM* vm, Value val) {
#if TRACE
    vm->trace->numReleaseAttempts += 1;
#endif
    if (VALUE_IS_POINTER(val)) {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(val);
        VLOG("release obj: {}, rc={}\n", FMT_STR(getTypeName(vm, getTypeId(val))), FMT_U32(obj->head.rc));
#if TRACE
        zCheckDoubleFree(vm, obj);
#endif
        obj->head.rc -= 1;
#if TRACK_GLOBAL_RC
    #if TRACE
        if (vm->refCounts == 0) {
            PRINT("Double free. {}\n", FMT_U32(getTypeId(val)));
            zFatal();
        }
    #endif
        vm->refCounts -= 1;
#endif
#if TRACE
        vm->trace->numReleases += 1;
#endif
        if (obj->head.rc == 0) {
            zFreeObject(vm, obj);
        }
    } else {
        VLOG("release: {}, nop\n", FMT_STR(getTypeName(vm, getTypeId(val))));
    }
}

static inline void releaseObject(VM* vm, HeapObject* obj) {
    VLOG("release obj: {}, rc={}\n", FMT_STR(getTypeName(vm, obj->head.typeId)), FMT_U32(obj->head.rc));
#if (TRACE)
    zCheckDoubleFree(vm, obj);
#endif
    obj->head.rc -= 1;
#if TRACK_GLOBAL_RC
    #if TRACE
    if (vm->refCounts == 0) {
        PRINT("Double free. {}\n", FMT_U32(obj->head.typeId));
        zFatal();
    }
    #endif
    vm->refCounts -= 1;
#endif
#if TRACE
    vm->trace->numReleases += 1;
    vm->trace->numReleaseAttempts += 1;
#endif
    if (obj->head.rc == 0) {
        zFreeObject(vm, obj);
    }
}

static inline void retainObject(VM* vm, HeapObject* obj) {
    obj->head.rc += 1;
#if TRACE
    zCheckRetainDanglingPointer(vm, obj);
    VLOG("retain {} rc={}\n", FMT_STR(getTypeName(vm, obj->head.typeId)), FMT_U32(obj->head.rc));
#endif
#if TRACK_GLOBAL_RC
    vm->refCounts += 1;
#endif
#if TRACE
    vm->trace->numRetains += 1;
    vm->trace->numRetainAttempts += 1;
#endif
}

static inline void retain(VM* vm, Value val) {
#if TRACE
    vm->trace->numRetainAttempts += 1;
#endif
    if (VALUE_IS_POINTER(val)) {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(val);
        VLOG("retain obj: {}, rc={}\n", FMT_STR(getTypeName(vm, getTypeId(val))), FMT_U32(obj->head.rc));
        obj->head.rc += 1;
#if TRACE
        zCheckRetainDanglingPointer(vm, obj);
#endif
#if TRACK_GLOBAL_RC
        vm->refCounts += 1;
#endif
#if TRACE
        vm->trace->numRetains += 1;
#endif
    } else {
        VLOG("retain: {}, nop\n", FMT_STR(getTypeName(vm, getTypeId(val))));
    }
}

static inline double toF64(Value val) {
    if (VALUE_IS_FLOAT(val)) {
        return VALUE_AS_FLOAT(val);
    } else {
        return zOtherToF64(val);
    }
}

static inline TypeId getTypeId(Value val) {
    u64 bits = val & TAGGED_PRIMITIVE_MASK;
    if (bits >= TAGGED_VALUE_MASK) {
        // Tagged.
        if (VALUE_IS_POINTER(val)) {
            return VALUE_AS_HEAPOBJECT(val)->head.typeId;
        } else {
            if (bits >= TAGGED_INTEGER_MASK) {
                return TYPE_INTEGER;
            } else {
                return VALUE_GET_TAG(bits);
            }
        }
    } else {
        return TYPE_FLOAT;
    }
}

static Str getTypeName(VM* vm, TypeId id) {
#if TRACE
    if (id == NULL_U32) {
        return (Str){ .ptr = "DanglingObject", .len = strlen("DanglingObject") };
    }
#endif
    Type t = ((Type*)vm->types.buf)[id];
    return (Str){ .ptr = t.namePtr, .len = t.nameLen };
}

static inline uint32_t pcOffset(VM* vm, Inst* pc) {
    return (uintptr_t)pc - (uintptr_t)vm->instPtr;
}

static inline uint32_t stackOffset(VM* vm, Value* stack) {
    return ((uintptr_t)stack - (uintptr_t)vm->stackPtr) >> 3;
}

static inline uint8_t getFieldOffset(VM* vm, HeapObject* obj, uint32_t symId) {
    FieldSymbolMap* symMap = ((FieldSymbolMap*)vm->fieldSyms.buf) + symId;
    if (obj->head.typeId == symMap->mruTypeId) {
        return (uint8_t)symMap->mruOffset;
    } else {
        return zGetFieldOffsetFromTable(vm, obj->head.typeId, symId);
    }
}

static inline bool isTypeSymCompat(TypeId typeSymId, TypeId cstrType) {
    if (typeSymId == cstrType) {
        return true;
    }
    if (cstrType == SEMA_TYPE_ANY || cstrType == SEMA_TYPE_DYNAMIC) {
        return true;
    }
    if (cstrType == SEMA_TYPE_STRING && typeSymId == SEMA_TYPE_STATICSTRING) {
        return true;
    }
    return false;
}

static inline Symbol getResolvedSym(VM* vm, SymbolId id) {
    return ((Symbol*)vm->compiler->sema.resolvedSyms.buf)[id];
}

static inline Str getName(VM* vm, NameId nameId) {
    Name name = ((Name*)vm->compiler->sema.nameSyms.buf)[nameId];
    return (Str){ .ptr = name.ptr, .len = name.len };
}

static inline Str getSemaSymName(VM* vm, u32 id) {
    Symbol sym = getResolvedSym(vm, id);
    return getName(vm, sym.key.nameId);
}

static inline FuncSig getResolvedFuncSig(VM* vm, FuncSigId id) {
    return ((FuncSig*)vm->compiler->sema.resolvedFuncSigs.buf)[id];
}

static inline ValueResult allocObject(VM* vm, TypeId typeId, Value* fields, u8 numFields) {
    // First slot holds the typeId and rc.
    HeapObjectResult res = zAllocExternalObject(vm, (1 + numFields) * sizeof(Value));
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->object = (Object){
        .typeId = typeId,
        .rc = 1,
    };

    Value* dst = objectGetValuesPtr(&res.obj->object);
    memcpy(dst, fields, numFields * sizeof(Value));

    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocEmptyMap(VM* vm) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->map = (Map){
        .typeId = TYPE_MAP,
        .rc = 1,
        .inner = {
            .metadata = 0,
            .entries = 0,
            .size = 0,
            .cap = 0,
            .available = 0,
        },
    };
    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocClosure(
    VM* vm, Value* fp, size_t funcPc, u8 numParams, u8 stackSize,
    u16 rFuncSigId, const Inst* capturedVals, u8 numCapturedVals, u8 closureLocal
) {
    HeapObjectResult res;
    if (numCapturedVals <= 2) {
        res = zAllocPoolObject(vm);
    } else {
        res = zAllocExternalObject(vm, (2 + numCapturedVals) * sizeof(Value));
    }
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->closure = (Closure){
        .typeId = TYPE_CLOSURE,
        .rc = 1,
        .funcPc = funcPc,
        .numParams = numParams,
        .stackSize = stackSize,
        .numCaptured = numCapturedVals,
        .local = closureLocal,
        .rFuncSigId = rFuncSigId,
    };
    Value* dst = closureGetCapturedValuesPtr(&res.obj->closure);
    for (int i = 0; i < numCapturedVals; i += 1) {
        Inst local = capturedVals[i];
#if TRACE
        if (!VALUE_IS_BOX(fp[local])) {
            PRINT("Expected box value.");
            zFatal();
        }
#endif
        retain(vm, fp[local]);
        dst[i] = fp[local];
    }
    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocLambda(VM* vm, uint32_t funcPc, uint8_t numParams, uint8_t stackSize, uint16_t rFuncSigId) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->lambda = (Lambda){
        .typeId = TYPE_LAMBDA,
        .rc = 1,
        .funcPc = funcPc,
        .numParams = numParams,
        .stackSize = stackSize,
        .rFuncSigId = rFuncSigId,
    };
    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocBox(VM* vm, Value val) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->box = (Box){
        .typeId = TYPE_BOX,
        .rc = 1,
        .val = val,
    };
    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocMetaType(VM* vm, uint8_t symType, uint32_t symId) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->metatype = (MetaType){
        .typeId = TYPE_METATYPE,
        .rc = 1,
        .type = symType,
        .symId = symId,
    };
    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocNativeFunc1(VM* vm, void* func, u32 numParams, u32 rFuncSigId) {
    HeapObjectResult res = zAllocPoolObject(vm);
    res.obj->nativeFunc1 = (NativeFunc1){
        .typeId = TYPE_NATIVE_FUNC,
        .rc = 1,
        .func = func,
        .numParams = numParams,
        .rFuncSigId = rFuncSigId,
        .hasTccState = false,
    };
    return (ValueResult){ .val = VALUE_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocFuncFromSym(VM* vm, FuncId funcId) {
    FuncSymbol sym = ((FuncSymbol*)vm->funcSyms.buf)[funcId];
    switch (sym.entryT) {
        case FUNC_SYM_NATIVEFUNC1: {
            u32 rFuncSigId = sym.innerExtra.nativeFunc1.rFuncSigId;
            u16 numParams = sym.innerExtra.nativeFunc1.typedFlagNumParams & ~((u16)1 << 15);
            return allocNativeFunc1(vm, sym.inner.nativeFunc1, numParams, rFuncSigId);
        }
        case FUNC_SYM_FUNC: {
            return allocLambda(vm, sym.inner.func.pc,
                sym.inner.func.numParams,
                sym.inner.func.stackSize,
                sym.innerExtra.func.rFuncSigId
            );
        }
        case FUNC_SYM_CLOSURE: {
            retainObject(vm, sym.inner.closure);
            return (ValueResult){ .val = VALUE_PTR(sym.inner.closure), .code = RES_CODE_SUCCESS };
        }
        default:
            zFatal();
            __builtin_unreachable();
    }
}

// Exponentiation by squaring.
static _BitInt(48) ipow(_BitInt(48) b, _BitInt(48) e) {
    if (e < 0) {
        if (b == 1 && e == -1) {
            return 1;
        }
        if (b == -1 && e == -1) {
            return  -1;
        }
        return 0;
    }
    _BitInt(48) result = 1;
    for (;;) {
        if (e & 1) {
            result *= b;
        }
        e >>= 1;
        if (!e) {
            break;
        }
        b *= b;
    }
    return result;
}

static inline Str getValueStaticString(VM* vm, Value val) {
    IndexSlice slice = VALUE_STATIC_STRING_SLICE(val);
    return (Str){ .ptr = vm->strBufPtr + slice.start, .len = slice.len };
}

static void panicStaticMsg(VM* vm, const char* msg) {
    vm->curFiber->panicPayload = (u64)msg | (((u64)(strlen(msg))) << 48);
    vm->curFiber->panicType = PANIC_STATIC_MSG;
    VLOG("{}", FMT_STRZ(msg));
}

static inline void panicDivisionByZero(VM* vm) {
    return panicStaticMsg(vm, "Division by zero.");
}

static inline void panicOutOfBounds(VM* vm) {
    return panicStaticMsg(vm, "Out of bounds.");
}

static inline void panicExpectedInteger(VM* vm) {
    return panicStaticMsg(vm, "Expected integer operand.");
}

static inline void panicExpectedFloat(VM* vm) {
    return panicStaticMsg(vm, "Expected float operand.");
}

static inline void panicFieldMissing(VM* vm) {
    return panicStaticMsg(vm, "Field not found in value.");
}

static void panicIncompatibleFieldType(VM* vm, SemaTypeId fieldSemaTypeId, Value rightv) {
    Str fieldTypeName = getSemaSymName(vm, fieldSemaTypeId);
    TypeId rightTypeId = getTypeId(rightv);
    SemaTypeId rightSemaTypeId = ((Type*)vm->types.buf)[rightTypeId].semaTypeId;
    Str rightTypeName = getSemaSymName(vm, rightSemaTypeId);
    zPanicFmt(vm, "Assigning to `{}` member with incompatible type `{}`.", (FmtValue[]){
        FMT_STR(fieldTypeName), FMT_STR(rightTypeName)
    }, 2);
    release(vm, rightv);
}

#define DEOPTIMIZE_TERNOP() \
    do { \
        pc[0] = CodeCallObjSym; \
        pc[1] = pc[8]; \
        pc[2] = pc[9]; \
        pc[3] = pc[10]; \
        pc[4] = pc[11]; \
    } while (false)

#define DEOPTIMIZE_BINOP() \
    do { \
        pc[0] = CodeCallObjSym; \
        pc[1] = pc[8]; \
        pc[2] = pc[9]; \
        pc[3] = pc[10]; \
    } while (false)

#define DEOPTIMIZE_UNARYOP() \
    do { \
        pc[0] = CodeCallObjSym; \
        pc[1] = pc[8]; \
        pc[2] = pc[9]; \
    } while (false)

#define RETURN(code) \
    do { \
        vm->curPc = pc; \
        vm->curStack = stack; \
        return code; \
    } while (false)

#define INTEGER_UNOP(...) \
    Value val = stack[pc[1]]; \
    if (VALUE_IS_INTEGER(val)) { \
        /* Body... */ \
        __VA_ARGS__; \
        pc += CALL_OBJ_SYM_INST_LEN; \
        NEXT(); \
    } else { \
        /* Always deopt, compiler would not gen if recv is dynamic. */ \
        DEOPTIMIZE_UNARYOP(); \
        NEXT(); \
    }

#define FLOAT_UNOP(...) \
    Value val = stack[pc[1]]; \
    if (VALUE_IS_FLOAT(val)) { \
        /* Body... */ \
        __VA_ARGS__; \
        pc += CALL_OBJ_SYM_INST_LEN; \
        NEXT(); \
    } else { \
        /* Always deopt, compiler would not gen if recv is dynamic. */ \
        DEOPTIMIZE_UNARYOP(); \
        NEXT(); \
    }

#define INTEGER_BINOP(...) \
    Value left = stack[pc[1]]; \
    Value right = stack[pc[2]]; \
    if (VALUE_BOTH_INTEGERS(left, right)) { \
        /* Body... */ \
        __VA_ARGS__; \
        pc += CALL_OBJ_SYM_INST_LEN; \
        NEXT(); \
    } else { \
        if (!VALUE_IS_INTEGER(left)) { \
            DEOPTIMIZE_BINOP(); \
            NEXT(); \
        } \
        panicExpectedInteger(vm); \
        RETURN(RES_CODE_PANIC); \
    }

#define FLOAT_BINOP(...) \
    Value left = stack[pc[1]]; \
    Value right = stack[pc[2]]; \
    if (VALUE_BOTH_FLOATS(left, right)) { \
        /* Body... */ \
        __VA_ARGS__; \
        pc += CALL_OBJ_SYM_INST_LEN; \
        NEXT(); \
    } else { \
        /* Always deopt, compiler would not gen if recv is dynamic. */ \
        if (!VALUE_IS_FLOAT(left)) { \
            DEOPTIMIZE_BINOP(); \
            NEXT(); \
        } \
        panicExpectedFloat(vm); \
        RETURN(RES_CODE_PANIC); \
    }

ResultCode execBytecode(VM* vm) {
    #define READ_I16(offset) ((int16_t)(pc[offset] | ((uint16_t)pc[offset + 1] << 8)))
    #define READ_U16(offset) (pc[offset] | ((uint16_t)pc[offset + 1] << 8))
    #define WRITE_U16(offset, u) pc[offset] = u & 0xff; pc[offset+1] = u >> 8
    #define READ_U32(offset) ((uint32_t)pc[offset] | ((uint32_t)pc[offset+1] << 8) | ((uint32_t)pc[offset+2] << 16) | ((uint32_t)pc[offset+3] << 24))
    #define READ_U48(offset) ((uint64_t)pc[offset] | ((uint64_t)pc[offset+1] << 8) | ((uint64_t)pc[offset+2] << 16) | ((uint64_t)pc[offset+3] << 24) | ((uint64_t)pc[offset+4] << 32) | ((uint64_t)pc[offset+5] << 40))

#if TRACE
    #define PRE_TRACE() \
        vm->trace->opCounts[pc[0]].count += 1; \
        vm->trace->totalOpCounts += 1;
    #define PRE_DUMP() \
        if (verbose) { \
            zDumpEvalOp(vm, pc); \
        } \
        vm->debugPc = pcOffset(vm, pc);
#else
    #define PRE_TRACE()
    #define PRE_DUMP()
#endif
#if CGOTO
    #define JENTRY(op) &&Code_##op
    static void* jumpTable[] = {
        JENTRY(ConstOp),
        JENTRY(ConstI8),
        JENTRY(AddFloat),
        JENTRY(SubFloat),
        JENTRY(True),
        JENTRY(False),
        JENTRY(None),
        JENTRY(Not),
        JENTRY(Copy),
        JENTRY(CopyReleaseDst),
        JENTRY(SetIndex),
        JENTRY(SetIndexRelease),
        JENTRY(CopyRetainSrc),
        JENTRY(IndexList),
        JENTRY(IndexMap),
        JENTRY(List),
        JENTRY(Map),
        JENTRY(MapEmpty),
        JENTRY(SliceList),
        JENTRY(JumpNotCond),
        JENTRY(JumpCond),
        JENTRY(Jump),
        JENTRY(Release),
        JENTRY(ReleaseN),
        JENTRY(CallObjSym),
        JENTRY(CallObjNativeFuncIC),
        JENTRY(CallObjFuncIC),
        JENTRY(CallTypeCheck),
        JENTRY(CallSym),
        JENTRY(CallFuncIC),
        JENTRY(CallNativeFuncIC),
        JENTRY(Ret1),
        JENTRY(Ret0),
        JENTRY(Call),
        JENTRY(Field),
        JENTRY(FieldIC),
        JENTRY(FieldRetain),
        JENTRY(FieldRetainIC),
        JENTRY(Lambda),
        JENTRY(Closure),
        JENTRY(Compare),
        JENTRY(LessFloat),
        JENTRY(GreaterFloat),
        JENTRY(LessEqualFloat),
        JENTRY(GreaterEqualFloat),
        JENTRY(LessInt),
        JENTRY(GreaterInt),
        JENTRY(LessEqualInt),
        JENTRY(GreaterEqualInt),
        JENTRY(MulFloat),
        JENTRY(DivFloat),
        JENTRY(PowFloat),
        JENTRY(ModFloat),
        JENTRY(CompareNot),
        JENTRY(StringTemplate),
        JENTRY(NegFloat),
        JENTRY(Init),
        JENTRY(ObjectSmall),
        JENTRY(Object),
        JENTRY(SetField),
        JENTRY(SetFieldRelease),
        JENTRY(SetFieldReleaseIC),
        JENTRY(SetCheckFieldRelease),
        JENTRY(PushTry),
        JENTRY(PopTry),
        JENTRY(Throw),
        JENTRY(Coinit),
        JENTRY(Coyield),
        JENTRY(Coresume),
        JENTRY(Coreturn),
        JENTRY(Retain),
        JENTRY(CopyRetainRelease),
        JENTRY(Box),
        JENTRY(SetBoxValue),
        JENTRY(SetBoxValueRelease),
        JENTRY(BoxValue),
        JENTRY(BoxValueRetain),
        JENTRY(Captured),
        JENTRY(Tag),
        JENTRY(TagLiteral),
        JENTRY(Cast),
        JENTRY(CastAbstract),
        JENTRY(BitwiseAnd),
        JENTRY(BitwiseOr),
        JENTRY(BitwiseXor),
        JENTRY(BitwiseNot),
        JENTRY(BitwiseLeftShift),
        JENTRY(BitwiseRightShift),
        JENTRY(JumpNotNone),
        JENTRY(AddInt),
        JENTRY(SubInt),
        JENTRY(MulInt),
        JENTRY(DivInt),
        JENTRY(PowInt),
        JENTRY(ModInt),
        JENTRY(NegInt),
        JENTRY(ForRangeInit),
        JENTRY(ForRange),
        JENTRY(ForRangeReverse),
        JENTRY(Match),
        JENTRY(StaticFunc),
        JENTRY(StaticVar),
        JENTRY(SetStaticVar),
        JENTRY(SetStaticFunc),
        JENTRY(Sym),
        JENTRY(End),
    };
    STATIC_ASSERT(sizeof(jumpTable) == (CodeEnd + 1) * sizeof(void*), JUMP_TABLE_INCOMPLETE);
    #define CASE(op) Code_##op
    #define NEXT() \
        do { \
            PRE_TRACE(); \
            PRE_DUMP(); \
            goto *jumpTable[*pc]; \
        } while (false)
#else
    // case name matches enum.
    #define CASE(op) case Code##op
    #define NEXT() \
        do { \
            PRE_TRACE(); \
            PRE_DUMP(); \
            goto beginSwitch \
        } while (false)
#endif

    register Inst* pc; 
    register Value* stack;

    pc = vm->curPc;
    stack = vm->curStack;

#if CGOTO
    // Goto first instruction.
    NEXT();
#else 
beginSwitch:
    switch ((OpCode)*pc) {
#endif
    CASE(ConstOp):
        stack[pc[3]] = VALUE_RAW(vm->constPtr[READ_U16(1)]);
        pc += 4;
        NEXT();
    CASE(ConstI8):
        stack[pc[2]] = VALUE_INTEGER_CAST(pc[1]);
        pc += 3;
        NEXT();
    CASE(AddFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_FLOAT(VALUE_AS_FLOAT(left) + VALUE_AS_FLOAT(right)))
    }
    CASE(SubFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_FLOAT(VALUE_AS_FLOAT(left) - VALUE_AS_FLOAT(right)))
    }
    CASE(True): {
        stack[pc[1]] = VALUE_TRUE;
        pc += 2;
        NEXT();
    }
    CASE(False): {
        stack[pc[1]] = VALUE_FALSE;
        pc += 2;
        NEXT();
    }
    CASE(None): {
        stack[pc[1]] = VALUE_NONE;
        pc += 2;
        NEXT();
    }
    CASE(Not): {
        Value* dst = &stack[pc[1]];
        Value val = *dst;
        bool bval = VALUE_IS_BOOLEAN(val) ? VALUE_AS_BOOLEAN(val) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(val);
        *dst = VALUE_BOOLEAN(!bval);
        pc += 2;
        NEXT();
    }
    CASE(Copy):
        stack[pc[2]] = stack[pc[1]];
        pc += 3;
        NEXT();
    CASE(CopyReleaseDst): {
        uint8_t dst = pc[2];
        release(vm, stack[dst]);
        stack[dst] = stack[pc[1]];
        pc += 3;
        NEXT();
    }
    CASE(SetIndex): {
        Value leftv = stack[pc[1]];
        Value indexv = stack[pc[2]];
        Value rightv = stack[pc[3]];
        ResultCode code = zSetIndex(vm, leftv, indexv, rightv);
        if (UNLIKELY(code != RES_CODE_SUCCESS)) {
            RETURN(code);
        }
        pc += 4;
        NEXT();
    }
    CASE(SetIndexRelease): {
        Value leftv = stack[pc[1]];
        Value indexv = stack[pc[2]];
        Value rightv = stack[pc[3]];
        ResultCode code = zSetIndexRelease(vm, leftv, indexv, rightv);
        if (UNLIKELY(code != RES_CODE_SUCCESS)) {
            RETURN(code);
        }
        pc += 4;
        NEXT();
    }
    CASE(CopyRetainSrc): {
        Value val = stack[pc[1]];
        retain(vm, val);
        stack[pc[2]] = val;
        pc += 3;
        NEXT();
    }
    CASE(IndexList): {
        Value listv = stack[pc[1]];
        Value index = stack[pc[2]];
        if (VALUE_IS_LIST(listv) && VALUE_IS_INTEGER(index)) {
            HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

            _BitInt(48) idx = VALUE_AS_INTEGER(index);
            if (idx < 0) {
                // Reverse index.
                idx = listo->list.list.len + idx;
                if (idx >= 0) {
                    Value val = ((Value*)listo->list.list.buf)[idx];
                    retain(vm, val);
                    stack[pc[3]] = val;
                    pc += CALL_OBJ_SYM_INST_LEN;
                    NEXT();
                } else {
                    panicOutOfBounds(vm);
                    RETURN(RES_CODE_PANIC);
                }
            }
            if (idx < listo->list.list.len) {
                Value val = ((Value*)listo->list.list.buf)[idx];
                retain(vm, val);
                stack[pc[3]] = val;
                pc += CALL_OBJ_SYM_INST_LEN;
                NEXT();
            } else {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }
        } else {
            if (!VALUE_IS_LIST(listv)) {
                DEOPTIMIZE_BINOP();
                NEXT();
            }
            panicExpectedInteger(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(IndexMap): {
        Value mapv = stack[pc[1]];
        Value index = stack[pc[2]];
        if (VALUE_IS_MAP(mapv)) {
            HeapObject* mapo = VALUE_AS_HEAPOBJECT(mapv);
            bool found;
            Value val = zValueMapGet(vm, &mapo->map.inner, index, &found);
            if (found) {
                retain(vm, val);
                stack[pc[3]] = val;
            } else {
                stack[pc[3]] = VALUE_NONE;
            }
            pc += CALL_OBJ_SYM_INST_LEN;
            NEXT();
        } else {
            DEOPTIMIZE_BINOP();
            NEXT();
        }
    }
    CASE(List): {
        uint8_t startLocal = pc[1];
        uint8_t numElems = pc[2];
        ValueResult res = zAllocList(vm, stack + startLocal, numElems);
        if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
            RETURN(res.code);
        }
        stack[pc[3]] = res.val;
        pc += 4;
        NEXT();
    }
    CASE(Map): {
        u8 startLocal = pc[1];
        u8 numEntries = pc[2];
        u16* keyIdxes = (u16*)(pc + 4);
        Value* vals = stack + startLocal;
        ValueResult res = zAllocMap(vm, keyIdxes, vals, numEntries);
        if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
            RETURN(res.code);
        }
        stack[pc[3]] = res.val;
        pc += 4 + numEntries * 2;
        NEXT();
    }
    CASE(MapEmpty): {
        ValueResult res = allocEmptyMap(vm);
        if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
            RETURN(res.code);
        }
        stack[pc[1]] = res.val;
        pc += 2;
        NEXT();
    }
    CASE(SliceList): {
        Value listv = stack[pc[1]];
        Value startv = stack[pc[2]];
        Value endv = stack[pc[3]];
        if (VALUE_IS_LIST(listv) && VALUE_IS_INTEGER(startv) && (VALUE_IS_INTEGER(endv) || VALUE_IS_NONE(endv))) {
            HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

            _BitInt(48) start = VALUE_AS_INTEGER(startv);
            if (start < 0) {
                start = listo->list.list.len + start;
            }
            _BitInt(48) end;
            if (VALUE_IS_NONE(endv)) {
                end = listo->list.list.len;
            } else {
                end = VALUE_AS_INTEGER(endv);
            }
            if (end < 0) {
                end = listo->list.list.len + end;
            }
            if (start < 0 || start > listo->list.list.len) {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }
            if (end < start || end > listo->list.list.len) {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }

            Value* elems = ((Value*)listo->list.list.buf);
            for (int i = start; i < end; i += 1) {
                retain(vm, elems[i]);
            }
            ValueResult res = zAllocList(vm, elems + start, end - start);
            if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
                RETURN(res.code);
            }
            stack[pc[4]] = res.val;
            pc += CALL_OBJ_SYM_INST_LEN;
            NEXT();
        } else {
            if (!VALUE_IS_LIST(listv)) {
                DEOPTIMIZE_TERNOP();
                NEXT();
            }
            panicExpectedInteger(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(JumpNotCond): {
        Value cond = stack[pc[1]];
        bool condVal = VALUE_IS_BOOLEAN(cond) ? VALUE_AS_BOOLEAN(cond) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(cond);
        if (!condVal) {
            pc += READ_U16(2);
            NEXT();
        } else {
            pc += 4;
            NEXT();
        }
    }
    CASE(JumpCond): {
        i16 jump = READ_I16(1);
        Value cond = stack[pc[3]];
        bool condVal = VALUE_IS_BOOLEAN(cond) ?
            VALUE_AS_BOOLEAN(cond) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(cond);
        if (condVal) {
            pc += (uintptr_t)jump;
        } else {
            pc += 4;
        }
        NEXT();
    }
    CASE(Jump): {
        pc += READ_I16(1);
        NEXT();
    }
    CASE(Release): {
        release(vm, stack[pc[1]]);
        pc += 2;
        NEXT();
    }
    CASE(ReleaseN): {
        u8 numLocals = pc[1];
        u8 i;
        for (i = 2; i < 2 + numLocals; i += 1) {
            VLOG("release reg {}\n", FMT_U32(pc[i]));
            release(vm, stack[pc[i]]);
        }
        pc += 2 + numLocals;
        NEXT();
    }
    CASE(CallObjSym): {
        u8 startLocal = pc[1];
        u8 numArgs = pc[2];
        u8 numRet = pc[3];
        u8 mgId = pc[4];
        u16 anySelfFuncSigId = READ_U16(5);

        Value recv = stack[startLocal + 4];
        TypeId typeId = getTypeId(recv);

        CallObjSymResult res = zCallObjSym(vm, pc, stack, recv, typeId, mgId, startLocal, numArgs, numRet, anySelfFuncSigId);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        pc = res.pc;
        stack = res.stack;
        NEXT();
    }
    CASE(CallObjNativeFuncIC): {
        uint8_t startLocal = pc[1];
        uint8_t numArgs = pc[2];
        Value recv = stack[startLocal + 4];
        TypeId typeId = getTypeId(recv);

        TypeId cachedTypeId = READ_U16(14);
        if (typeId == cachedTypeId) {
            // const newFramePtr = framePtr + startLocal;
            vm->curStack = stack;
            MethodPtr fn = (MethodPtr)READ_U48(8);
            Value res = fn(vm, recv, stack + startLocal + 5, numArgs);
            if (res == VALUE_INTERRUPT) {
                RETURN(RES_CODE_PANIC);
            }
            uint8_t numRet = pc[3];
            if (numRet == 1) {
                stack[startLocal] = res;
            } else {
                switch (numRet) {
                    case 0: 
                        // Nop.
                        break;
                    case 1:
                        // Not possible.
                        zFatal();
                    default:
                        zFatal();
                }
            }
            pc += CALL_OBJ_SYM_INST_LEN;
            // In the future, we might allow native functions to change the pc and framePtr.
            // pc = vm.pc;
            // framePtr = vm.framePtr;
            NEXT();
        }

        // Deoptimize.
        pc[0] = CodeCallObjSym;
        NEXT();
    }
    CASE(CallObjFuncIC): {
        uint8_t startLocal = pc[1];
        uint8_t recv = stack[startLocal + 4];
        TypeId typeId = getTypeId(recv);

        TypeId cachedTypeId = READ_U16(14);
        if (typeId == cachedTypeId) {
            uint8_t numLocals = pc[7];
            if (stack + startLocal + numLocals >= vm->stackEndPtr) {
                RETURN(RES_CODE_STACK_OVERFLOW);
            }
            Value retFramePtr = (uintptr_t)stack;
            stack += startLocal;
            *(uint8_t*)(stack + 1) = pc[3];
            *((uint8_t*)(stack + 1) + 1) = 0;
            stack[2] = (uintptr_t)(pc + 16);
            stack[3] = retFramePtr;
            pc = vm->instPtr + READ_U32(8);
            NEXT();
        }

        // Deoptimize.
        pc[0] = CodeCallObjSym;
        NEXT();
    }
    CASE(CallTypeCheck): {
        uint8_t argStartReg = pc[1];
        uint8_t numArgs = pc[2];
        uint16_t funcSigId = READ_U16(3);

        FuncSig funcSig = getResolvedFuncSig(vm, funcSigId);
        Value* args = stack + argStartReg;

        // TODO: numArgs and this check can be removed if overloaded symbols are grouped by numParams.
        if (numArgs != funcSig.paramLen) {
            u16 funcId = READ_U16(5 + 4);
            zPanicIncompatibleFuncSig(vm, funcId, args, numArgs, funcSigId);
            RETURN(RES_CODE_PANIC);
        }

        // Perform type check on args.
        for (int i = 0; i < funcSig.paramLen; i += 1) {
            SemaTypeId cstrTypeId = funcSig.paramPtr[i];
            TypeId argTypeId = getTypeId(args[i]);

            SemaTypeId argSemaTypeId = ((Type*)vm->types.buf)[argTypeId].semaTypeId;
            if (!isTypeSymCompat(argSemaTypeId, cstrTypeId)) {
                // Assumes next inst is callSym/callNativeFuncIC/callFuncIC.
                u16 funcId = READ_U16(5 + 4);
                // return panicIncompatibleFuncSig(vm, funcId, args, funcSigId);
                zPanicIncompatibleFuncSig(vm, funcId, args, numArgs, funcSigId);
                RETURN(RES_CODE_PANIC);
            }
        }
        pc += 5;
        NEXT();
    }
    CASE(CallSym): {
        u8 startLocal = pc[1];
        u8 numArgs = pc[2];
        u8 numRet = pc[3];
        u16 symId = READ_U16(4);
        PcSpResult res = zCallSym(vm, pc, stack, symId, startLocal, numArgs, numRet);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        pc = res.pc;
        stack = res.sp;
        NEXT();
    }
    CASE(CallFuncIC): {
        uint8_t startLocal = pc[1];
        uint8_t numLocals = pc[4];
        if (stack + startLocal + numLocals >= vm->stackEndPtr) {
            RETURN(RES_CODE_STACK_OVERFLOW);
        }

        Value retFramePtr = (uintptr_t)stack;
        stack += startLocal;
        stack[1] = VALUE_RETINFO(pc[3], false, CALL_SYM_INST_LEN);
        stack[2] = (uintptr_t)(pc + CALL_SYM_INST_LEN);
        stack[3] = retFramePtr;
        pc = (Inst*)READ_U48(6);
        NEXT();
    }
    CASE(CallNativeFuncIC): {
        uint8_t startLocal = pc[1];
        uint8_t numArgs = pc[2];

        Value* newStack = stack + startLocal;
        vm->curStack = newStack;
        FuncPtr fn = (FuncPtr)READ_U48(6);
        Value res = fn(vm, newStack + 4, numArgs);
        if (res == VALUE_INTERRUPT) {
            RETURN(RES_CODE_PANIC);
        }
        uint8_t numRet = pc[3];
        if (numRet == 1) {
            newStack[0] = res;
        } else {
            switch (numRet) {
                case 0:
                    // Nop.
                    break;
                case 1:
                    zFatal();
                default:
                    zFatal();
            }
        }
        pc += CALL_SYM_INST_LEN;
        NEXT();
    }
    CASE(Ret1): {
        uint8_t reqNumArgs = VALUE_RETINFO_NUMRETVALS(stack[1]);
        uint8_t retFlag = VALUE_RETINFO_RETFLAG(stack[1]);
        if (reqNumArgs == 1) {
            pc = (Inst*)stack[2];
            stack = (Value*)stack[3];
            if (retFlag == 0) {
                NEXT();
            } else {
                RETURN(RES_CODE_SUCCESS);
            }
        } else {
            switch (reqNumArgs) {
                case 0: {
                    release(vm, stack[0]);
                    break;
                }
                case 1:
                    zFatal();
                default:
                    zFatal();
            }
            pc = (Inst*)stack[2];
            stack = (Value*)stack[3];
            if (retFlag == 0) {
                NEXT();
            } else {
                RETURN(RES_CODE_SUCCESS);
            }
        }
    }
    CASE(Ret0): {
        uint8_t reqNumArgs = VALUE_RETINFO_NUMRETVALS(stack[1]);
        uint8_t retFlag = VALUE_RETINFO_RETFLAG(stack[1]);
        if (reqNumArgs == 0) {
            pc = (Inst*)stack[2];
            stack = (Value*)stack[3];
            if (retFlag == 0) {
                NEXT();
            } else {
                RETURN(RES_CODE_SUCCESS);
            }
        } else {
            switch (reqNumArgs) {
                case 0:
                    zFatal();
                case 1:
                    stack[0] = VALUE_NONE;
                    break;
                default:
                    zFatal();
            }
            pc = (Inst*)stack[2];
            stack = (Value*)stack[3];
            if (retFlag == 0) {
                NEXT();
            } else {
                RETURN(RES_CODE_SUCCESS);
            }
        }
    }
    CASE(Call): {
        u8 startLocal = pc[1];
        u8 numArgs = pc[2];
        u8 numRet = pc[3];

        Value callee = stack[startLocal + numArgs + 4];
        Value retInfo = VALUE_RETINFO(numRet, false, CALL_INST_LEN);
        PcSpResult res = zCall(vm, pc, stack, callee, startLocal, numArgs, retInfo);
        if (LIKELY(res.code == RES_CODE_SUCCESS)) {
            pc = res.pc;
            stack = res.sp;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(Field): {
#define FIELD_BODY(v) \
    uint8_t left = pc[1]; \
    uint8_t dst = pc[2]; \
    uint16_t symId = READ_U16(3); \
    Value recv = stack[left]; \
    if (VALUE_IS_POINTER(recv)) { \
        HeapObject* obj = VALUE_AS_HEAPOBJECT(recv); \
        uint8_t offset = getFieldOffset(vm, obj, symId); \
        if (offset != NULL_U8) { \
            stack[dst] = objectGetField((Object*)obj, offset); \
            pc[0] = FIELD_BODY_IC_##v; \
            WRITE_U16(5, obj->head.typeId); \
            pc[7] = offset; \
        } else { \
            stack[dst] = zGetFieldFallback(vm, obj, ((FieldSymbolMap*)vm->fieldSyms.buf)[symId].nameId); \
        } \
        FIELD_BODY_END_##v \
        pc += 8; \
        NEXT(); \
    } else { \
        panicFieldMissing(vm); \
        RETURN(RES_CODE_PANIC); \
    }
#define FIELD_BODY_IC_0 CodeFieldIC
#define FIELD_BODY_END_0 
        FIELD_BODY(0);
    }
    CASE(FieldIC): {
        Value recv = stack[pc[1]];
        uint8_t dst = pc[2];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            if (obj->head.typeId == READ_U16(5)) {
                stack[dst] = objectGetField((Object*)obj, pc[7]);
                pc += 8;
                NEXT();
            } else {
                // Deoptimize.
                pc[0] = CodeField;
                NEXT();
            }
        } else {
            panicFieldMissing(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(FieldRetain): {
#define FIELD_BODY_IC_1 CodeFieldRetainIC
#define FIELD_BODY_END_1 retain(vm, stack[dst]);
        FIELD_BODY(1);
    }
    CASE(FieldRetainIC): {
        Value recv = stack[pc[1]];
        uint8_t dst = pc[2];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            if (obj->head.typeId == READ_U16(5)) {
                stack[dst] = objectGetField((Object*)obj, pc[7]);
                retain(vm, stack[dst]);
                pc += 8;
                NEXT();
            }
        } else {
            // return vm.getFieldMissingSymbolError();
            RETURN(RES_CODE_UNKNOWN);
        }
        // Deoptimize.
        pc[0] = CodeFieldRetain;
        // framePtr[dst] = try @call(.never_inline, gvm.getField, .{ recv, pc[3].arg });
        // retain(vm, framePtr[dst]);
        // pc += 7;
        NEXT();
    }
    CASE(Lambda): {
        uint32_t funcPc = ((uint32_t)getInstOffset(vm, pc)) - pc[1];
        uint8_t numParams = pc[2];
        uint8_t stackSize = pc[3];
        uint16_t rFuncSigId = READ_U16(4);
        ValueResult res = allocLambda(vm, funcPc, numParams, stackSize, rFuncSigId);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[6]] = res.val;
        pc += 7;
        NEXT();
    }
    CASE(Closure): {
        u32 funcPc = getInstOffset(vm, pc) - pc[1];
        u8 numParams = pc[2];
        u8 numCaptured = pc[3];
        u8 stackSize = pc[4];
        u16 rFuncSigId = READ_U16(5);
        u8 local = pc[7];
        u8 dst = pc[8];
        Inst* capturedVals = pc + 9;

        ValueResult res = allocClosure(vm, stack, funcPc, numParams, stackSize, rFuncSigId, capturedVals, numCaptured, local);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = res.val;
        pc += 9 + numCaptured;
        NEXT();
    }
    CASE(Compare): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (left == right) {
            stack[pc[3]] = VALUE_TRUE;
        } else {
            stack[pc[3]] = zEvalCompare(vm, left, right);
        }
        pc += 4;
        NEXT();
    }
    CASE(LessFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_FLOAT(left) < VALUE_AS_FLOAT(right)))
    }
    CASE(GreaterFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_FLOAT(left) > VALUE_AS_FLOAT(right)))
    }
    CASE(LessEqualFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_FLOAT(left) <= VALUE_AS_FLOAT(right)))
    }
    CASE(GreaterEqualFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_FLOAT(left) >= VALUE_AS_FLOAT(right)))
    }
    CASE(LessInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_INTEGER(left) < VALUE_AS_INTEGER(right)))
    }
    CASE(GreaterInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_INTEGER(left) > VALUE_AS_INTEGER(right)))
    }
    CASE(LessEqualInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_INTEGER(left) <= VALUE_AS_INTEGER(right)))
    }
    CASE(GreaterEqualInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_INTEGER(left) >= VALUE_AS_INTEGER(right)))
    }
    CASE(MulFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_FLOAT(VALUE_AS_FLOAT(left) * VALUE_AS_FLOAT(right)))
    }
    CASE(DivFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_FLOAT(VALUE_AS_FLOAT(left) / VALUE_AS_FLOAT(right)))
    }
    CASE(PowFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_FLOAT(pow(VALUE_AS_FLOAT(left), VALUE_AS_FLOAT(right))))
    }
    CASE(ModFloat): {
        FLOAT_BINOP(stack[pc[3]] = VALUE_FLOAT(fmod(VALUE_AS_FLOAT(left), VALUE_AS_FLOAT(right))))
    }
    CASE(CompareNot): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (left == right) {
            stack[pc[3]] = VALUE_FALSE;
        } else {
            stack[pc[3]] = zEvalCompareNot(vm, left, right);
        }
        pc += 4;
        NEXT();
    }
    CASE(StringTemplate): {
        u8 startLocal = pc[1];
        u8 exprCount = pc[2];
        u8 dst = pc[3];
        u8 strCount = exprCount + 1;
        Inst* strs = pc + 4;
        Value* vals = stack + startLocal;
        ValueResult res = zAllocStringTemplate(vm, strs, strCount, vals, exprCount);
        if (LIKELY(res.code == RES_CODE_SUCCESS)) {
            stack[dst] = res.val;
            pc += 4 + strCount;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(NegFloat): {
        FLOAT_UNOP(stack[pc[2]] = VALUE_FLOAT(-VALUE_AS_FLOAT(val)))
    }
    CASE(Init): {
        uint8_t start = pc[1];
        uint8_t numLocals = pc[2];
        uint8_t i;
        for (i = start; i < start + numLocals; i += 1) {
            stack[i] = VALUE_NONE;
        }
        pc += 3;
        NEXT();
    }
    CASE(ObjectSmall): {
        uint8_t sid = pc[1];
        uint8_t startLocal = pc[2];
        uint8_t numFields = pc[3];
        ValueResult res = zAllocObjectSmall(vm, sid, stack + startLocal, numFields);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[4]] = res.val;
        pc += 5;
        NEXT();
    }
    CASE(Object): {
        u8 typeId = pc[1];
        u8 startLocal = pc[2];
        u8 numFields = pc[3];
        ValueResult res = allocObject(vm, typeId, stack + startLocal, numFields);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[4]] = res.val;
        pc += 5;
        NEXT();
    }
    CASE(SetField): {
        u8 symId = pc[1];
        Value recv = stack[pc[2]];
        Value val = stack[pc[3]];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            uint8_t offset = getFieldOffset(vm, obj, symId);
            if (offset != NULL_U8) {
                Value* lastValue = objectGetFieldPtr((Object*)obj, offset);
                *lastValue = val;
                pc += 4;
                NEXT();
            } else {
                // return vm.getFieldMissingSymbolError();
                RETURN(RES_CODE_UNKNOWN);
            }
        } else {
            // return vm.setFieldNotObjectError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(SetFieldRelease): {
        Value recv = stack[pc[1]];
        Value val = stack[pc[2]];
        u8 symId = pc[3];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            uint8_t offset = getFieldOffset(vm, obj, symId);
            if (offset != NULL_U8) {
                Value* lastValue = objectGetFieldPtr((Object*)obj, offset);
                release(vm, *lastValue);
                *lastValue = val;

                pc[0] = CodeSetFieldReleaseIC;
                WRITE_U16(4, obj->head.typeId);
                pc[6] = offset;
                pc += 7;
                NEXT();
            } else {
                // return vm.getFieldMissingSymbolError();
                RETURN(RES_CODE_UNKNOWN);
            }
        } else {
            // return vm.setFieldNotObjectError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(SetFieldReleaseIC): {
        Value recv = stack[pc[1]];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            if (obj->head.typeId == READ_U16(4)) {
                Value* lastValue = objectGetFieldPtr((Object*)obj, pc[6]);
                release(vm, *lastValue);
                *lastValue = stack[pc[2]];
                pc += 7;
                NEXT();
            } else {
                // Deoptimize.
                pc[0] = CodeSetFieldRelease;
                NEXT();
            }
        } else {
            // return vm.getFieldMissingSymbolError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(SetCheckFieldRelease): {
        Value recv = stack[pc[1]];
        Value val = stack[pc[2]];
        uint8_t symId = pc[3];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            uint8_t offset = getFieldOffset(vm, obj, symId);
            if (offset != NULL_U8) {
                FieldSymbolMap* symMap = ((FieldSymbolMap*)vm->fieldSyms.buf) + symId;
                uint32_t fieldSemaTypeId = symMap->mruFieldTypeSymId;
                TypeId rightTypeId = getTypeId(val);
                Type* type = ((Type*)vm->types.buf) + rightTypeId;
                uint32_t rightSemaTypeId = type->semaTypeId;
                if (!isTypeSymCompat(rightSemaTypeId, fieldSemaTypeId)) {
                    panicIncompatibleFieldType(vm, fieldSemaTypeId, val);
                    RETURN(RES_CODE_PANIC);
                }

                Value* lastValue = objectGetFieldPtr((Object*)obj, offset);
                release(vm, *lastValue);
                *lastValue = val;

                // TODO: Inline cache.
                pc += 7;
                NEXT();
            } else {
                // return vm.getFieldMissingSymbolError();
                RETURN(RES_CODE_UNKNOWN);
            }
        } else {
            // return vm.setFieldNotObjectError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(PushTry): {
        u8 errDst = pc[1];
        u16 catchPcOffset = READ_U16(2);
        if (vm->tryStack.len == vm->tryStack.cap) {
            ResultCode code = zGrowTryStackTotalCapacity(&vm->tryStack, vm->alloc, vm->tryStack.len + 1);
            if (code != RES_CODE_SUCCESS) {
                RETURN(code);
            }
        }
        ((TryFrame*)vm->tryStack.buf)[vm->tryStack.len] = (TryFrame){
            .fp = stack,
            .catchPc = getInstOffset(vm, pc) + catchPcOffset,
            .catchErrDst = errDst,
        };
        vm->tryStack.len += 1; 
        pc += 4;
        NEXT();
    }
    CASE(PopTry): {
        vm->tryStack.len += 1; 
        pc += READ_U16(1);
        NEXT();
    }
    CASE(Throw): {
        Value err = stack[pc[1]];
        if (VALUE_IS_ERROR(err)) {
            PcSpResult res = zThrow(vm, stack, pc, err);
            if (res.code != RES_CODE_SUCCESS) {
                RETURN(res.code);
            }
            stack = res.sp;
            pc = res.pc;
            NEXT();
        } else {
            panicStaticMsg(vm, "Not an error.");
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Coinit): {
        u8 startArgsLocal = pc[1];
        u8 numArgs = pc[2];
        u8 jump = pc[3];
        u8 initialStackSize = pc[4];
        u8 dst = pc[5];

        ValueResult res = zAllocFiber(vm, pcOffset(vm, pc + 6), stack + startArgsLocal, numArgs, initialStackSize);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = res.val;
        pc += jump;
        NEXT();
    }
    CASE(Coyield):
        if (vm->curFiber != &vm->mainFiber) {
            PcSp res = zPopFiber(vm, pcOffset(vm, pc), stack, VALUE_NONE);
            pc = res.pc;
            stack = res.sp;
        } else {
            pc += 3;
        }
        NEXT();
    CASE(Coresume): {
        Value fiber = stack[pc[1]];
        if (VALUE_IS_POINTER(fiber)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(fiber);
            if (obj->head.typeId == TYPE_FIBER) {
                if ((Fiber*)obj != vm->curFiber) {
                    if (obj->fiber.pcOffset != NULL_U32) {
                        PcSp res = zPushFiber(vm, pcOffset(vm, pc + 3), stack, (Fiber*)obj, pc[2]);
                        pc = res.pc;
                        stack = res.sp;
                        NEXT();
                    }
                }
            }
            releaseObject(vm, obj);
        }
        pc += 3;
        NEXT();
    }
    CASE(Coreturn): {
        pc += 1;
        if (vm->curFiber != &vm->mainFiber) {
            PcSp res = zPopFiber(vm, NULL_U32, stack, stack[1]);
            pc = res.pc;
            stack = res.sp;
        }
        NEXT();
    }
    CASE(Retain): {
        retain(vm, stack[pc[1]]);
        pc += 2;
        NEXT();
    }
    CASE(CopyRetainRelease): {
        uint8_t src = pc[1];
        uint8_t dst = pc[2];
        retain(vm, stack[src]);
        release(vm, stack[dst]);
        stack[dst] = stack[src];
        pc += 3;
        NEXT();
    }
    CASE(Box): {
        Value value = stack[pc[1]];
        ValueResult res = allocBox(vm, value);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[2]] = res.val;
        pc += 3;
        NEXT();
    }
    CASE(SetBoxValue): {
        Value box = stack[pc[1]];
        Value rval = stack[pc[2]];
#if TRACE
        ASSERT(VALUE_IS_POINTER(box));
#endif
        HeapObject* obj = VALUE_AS_HEAPOBJECT(box);
#if TRACE
        ASSERT(obj->head.typeId == TYPE_BOX);
#endif
        obj->box.val = rval;
        pc += 3;
        NEXT();
    }
    CASE(SetBoxValueRelease): {
        Value box = stack[pc[1]];
        Value rval = stack[pc[2]];
#if TRACE
        ASSERT(VALUE_IS_POINTER(box));
#endif
        HeapObject* obj = VALUE_AS_HEAPOBJECT(box);
#if TRACE
        ASSERT(obj->head.typeId == TYPE_BOX);
#endif
        release(vm, obj->box.val);
        obj->box.val = rval;
        pc += 3;
        NEXT();
    }
    CASE(BoxValue): {
        Value box = stack[pc[1]];
#if TRACE
        if (!VALUE_IS_BOX(box)) {
            VLOG("Expected box value.");
            zFatal();
        }
#endif
        stack[pc[2]] = VALUE_AS_HEAPOBJECT(box)->box.val;
        pc += 3;
        NEXT();
    }
    CASE(BoxValueRetain): {
        Value box = stack[pc[1]];
#if TRACE
        if (!VALUE_IS_BOX(box)) {
            VLOG("Expected box value.");
            zFatal();
        }
#endif
        Value val = VALUE_AS_HEAPOBJECT(box)->box.val;
        stack[pc[2]] = val;
        retain(vm, val);
        pc += 3;
        NEXT();
    }
    CASE(Captured): {
        Value closure = stack[pc[1]];
#if TRACE
        if (!VALUE_IS_CLOSURE(closure)) {
            VLOG("Expected closure value.");
            zFatal();
        }
#endif
        stack[pc[3]] = closureGetCapturedValuesPtr(&VALUE_AS_HEAPOBJECT(closure)->closure)[pc[2]];
        pc += 4;
        NEXT();
    }
    CASE(Tag): {
        u8 tagId = pc[1];
        u8 val = pc[2];
        stack[pc[3]] = VALUE_ENUM(tagId, val);
        pc += 4;
        NEXT();
    }
    CASE(TagLiteral): {
        u8 symId = pc[1];
        stack[pc[2]] = VALUE_SYMBOL(symId);
        pc += 3;
        NEXT();
    }
    CASE(Cast): {
        Value val = stack[pc[1]];
        u16 expTypeId = READ_U16(2);
        if (getTypeId(val) == expTypeId) {
            pc += 4;
            NEXT();
        } else {
            Type actType = ((Type*)vm->types.buf)[getTypeId(val)];
            Type expType = ((Type*)vm->types.buf)[expTypeId];
            zPanicFmt(vm, "Can not cast `{}` to `{}`.", (FmtValue[]){
                FMT_STRLEN(actType.namePtr, actType.nameLen),
                FMT_STRLEN(expType.namePtr, expType.nameLen)
            }, 2);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(CastAbstract): {
        Value val = stack[pc[1]];
        u16 expSemaTypeId = READ_U16(2);
        if (expSemaTypeId == SEMA_TYPE_ANY) {
            pc += 4;
            NEXT();
        } else if (expSemaTypeId == SEMA_TYPE_STRING) {
            if (valueIsString(val)) {
                pc += 4;
                NEXT();
            }
        } else if (expSemaTypeId == SEMA_TYPE_RAWSTRING) {
            if (valueIsRawString(val)) {
                pc += 4;
                NEXT();
            }
        }
        Symbol sym = getResolvedSym(vm, expSemaTypeId);
        Str name = getName(vm, sym.key.nameId);
        Type actType = ((Type*)vm->types.buf)[getTypeId(val)];
        zPanicFmt(vm, "Can not cast `{}` to `{}`.", (FmtValue[]){
            FMT_STRLEN(actType.namePtr, actType.nameLen),
            FMT_STR(name)
        }, 2);
        RETURN(RES_CODE_PANIC);
    }
    CASE(BitwiseAnd): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) & VALUE_AS_INTEGER(right)))
    }
    CASE(BitwiseOr): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) | VALUE_AS_INTEGER(right)))
    }
    CASE(BitwiseXor): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) ^ VALUE_AS_INTEGER(right)))
    }
    CASE(BitwiseNot): {
        INTEGER_UNOP(stack[pc[2]] = VALUE_INTEGER(~VALUE_AS_INTEGER(val)))
    }
    CASE(BitwiseLeftShift): {
        INTEGER_BINOP(
            _BitInt(48) rightInt = VALUE_AS_INTEGER(right);
            if (rightInt > 48 || rightInt < 0) {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }
            stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) << rightInt);
        )
    }
    CASE(BitwiseRightShift): {
        INTEGER_BINOP(
            _BitInt(48) rightInt = VALUE_AS_INTEGER(right);
            if (rightInt > 48 || rightInt < 0) {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }
            stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) >> rightInt);
        )
    }
    CASE(JumpNotNone): {
        int16_t offset = READ_I16(1);
        if (!VALUE_IS_NONE(stack[pc[3]])) {
            pc += offset;
            NEXT();
        } else {
            pc += 4;
            NEXT();
        }
    }
    CASE(AddInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) + VALUE_AS_INTEGER(right)))
    }
    CASE(SubInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) - VALUE_AS_INTEGER(right)))
    }
    CASE(MulInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) * VALUE_AS_INTEGER(right)))
    }
    CASE(DivInt): {
        INTEGER_BINOP(
            _BitInt(48) rightInt = VALUE_AS_INTEGER(right);
            if (rightInt == 0) {
                panicDivisionByZero(vm);
                RETURN(RES_CODE_PANIC);
            }
            stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) / VALUE_AS_INTEGER(right));
        )
    }
    CASE(PowInt): {
        INTEGER_BINOP(stack[pc[3]] = VALUE_INTEGER(ipow(VALUE_AS_INTEGER(left), VALUE_AS_INTEGER(right))))
    }
    CASE(ModInt): {
        INTEGER_BINOP(
            _BitInt(48) rightInt = VALUE_AS_INTEGER(right);
            if (rightInt == 0) {
                panicDivisionByZero(vm);
                RETURN(RES_CODE_PANIC);
            }
            stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) % VALUE_AS_INTEGER(right));
        )
    }
    CASE(NegInt): {
        INTEGER_UNOP(stack[pc[2]] = VALUE_INTEGER(-VALUE_AS_INTEGER(val)))
    }
    CASE(ForRangeInit): {
        Value startv = stack[pc[1]];
        Value endv = stack[pc[2]];
        Value stepv = stack[pc[3]];
        if (VALUE_ALL3_INTEGERS(startv, endv, stepv)) {
            _BitInt(48) start = VALUE_AS_INTEGER(startv);
            _BitInt(48) end = VALUE_AS_INTEGER(endv);
            _BitInt(48) step = VALUE_AS_INTEGER(stepv);
            if (step < 0) {
                step = -step;
            }
            if (start == end) {
                pc += READ_U16(6) + 7;
                NEXT();
            } else {
                stack[pc[4]] = startv;
                stack[pc[5]] = startv;
                u16 offset = READ_U16(6);
                if (start < end) {
                    pc[offset] = CodeForRange;
                } else {
                    pc[offset] = CodeForRangeReverse;
                }
                pc += 8;
                NEXT();
            }
        } else {
            panicExpectedInteger(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(ForRange): {
        _BitInt(48) counter = VALUE_AS_INTEGER(stack[pc[1]]) + VALUE_AS_INTEGER(stack[pc[2]]);
        if (counter < VALUE_AS_INTEGER(stack[pc[3]])) {
            stack[pc[1]] = VALUE_INTEGER(counter);
            stack[pc[4]] = VALUE_INTEGER(counter);
            pc -= READ_U16(5);
        } else {
            pc += 7;
        }
        NEXT();
    }
    CASE(ForRangeReverse): {
        _BitInt(48) counter = VALUE_AS_INTEGER(stack[pc[1]]) - VALUE_AS_INTEGER(stack[pc[2]]);
        if (counter > VALUE_AS_INTEGER(stack[pc[3]])) {
            stack[pc[1]] = VALUE_INTEGER(counter);
            stack[pc[4]] = VALUE_INTEGER(counter);
            pc -= READ_U16(5);
        } else {
            pc += 7;
        }
        NEXT();
    }
    CASE(Match): {
        pc += zOpMatch(vm, pc, stack);
        NEXT();
    }
    CASE(StaticFunc): {
        u16 funcId = READ_U16(1);
        ValueResult res = allocFuncFromSym(vm, funcId);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[3]] = res.val;
        pc += 4;
        NEXT();
    }
    CASE(StaticVar): {
        uint16_t symId = READ_U16(1);
        Value sym = ((StaticVar*)vm->varSyms.buf)[symId].value;
        retain(vm, sym);
        stack[pc[3]] = sym;
        pc += 4;
        NEXT();
    }
    CASE(SetStaticVar): {
        uint16_t symId = READ_U16(1);
        Value prev = ((StaticVar*)vm->varSyms.buf)[symId].value;
        ((StaticVar*)vm->varSyms.buf)[symId].value = stack[pc[3]];
        release(vm, prev);
        pc += 4;
        NEXT();
    }
    CASE(SetStaticFunc): {
        u16 funcId = READ_U16(1);
        ResultCode code = zSetStaticFunc(vm, funcId, stack[pc[3]]);
        if (code != RES_CODE_SUCCESS) {
            RETURN(code);
        }
        pc += 4;
        NEXT();
    }
    CASE(Sym): {
        uint8_t symType = pc[1];
        uint32_t symId = READ_U32(2);
        ValueResult res = allocMetaType(vm, symType, symId);
        if (LIKELY(res.code == RES_CODE_SUCCESS)) {
            stack[pc[6]] = res.val;
            pc += 7;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(End): {
        zEnd(vm, pc);
        RETURN(RES_CODE_SUCCESS);
    }
#if CGOTO
#else
    }
#endif
}