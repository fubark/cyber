#include <stdarg.h>
#include <string.h>
#include <math.h>
#include "vm.h"

#define STATIC_ASSERT(cond, msg) typedef char static_assertion_##msg[(cond)?1:-1]

#define TRACEV(msg, ...) \
    do { if (TRACE && verbose) zLog(msg, (FmtValue[]){__VA_ARGS__}, sizeof((FmtValue[]){__VA_ARGS__})/sizeof(FmtValue)); } while (false)
#define LTRACE(msg, ...) \
    do { if (TRACE) zLog(msg, (FmtValue[]){__VA_ARGS__}, sizeof((FmtValue[]){__VA_ARGS__})/sizeof(FmtValue)); } while (false)
#define LOG(msg, ...) \
    do { zLog(msg, (FmtValue[]){__VA_ARGS__}, sizeof((FmtValue[]){__VA_ARGS__})/sizeof(FmtValue)); } while (false)

#define ASSERT(cond) \
    do { if (!(cond)) zFatal(); } while (false)

#define FMT_STRZ(s) ((FmtValue){ .type = FMT_TYPE_STRING, .data = { .string = s }, .data2 = { .string = strlen(s) }})
#define FMT_STR(s) ((FmtValue){ .type = FMT_TYPE_STRING, .data = { .string = s.ptr }, .data2 = { .string = s.len }})
#define FMT_STRLEN(s, len) ((FmtValue){ .type = FMT_TYPE_STRING, .data = { .string = s }, .data2 = { .string = len }})
#define FMT_U32(v) ((FmtValue){ .type = FMT_TYPE_U32, .data = { .u32 = v }})
#define FMT_PTR(v) ((FmtValue){ .type = FMT_TYPE_PTR, .data = { .ptr = v }})

static inline TypeId getTypeId(Value val);

static inline uintptr_t getStackOffset(VM* vm, Value* to) {
    return (((uintptr_t)to) - ((uintptr_t)vm->stackPtr)) >> 3;
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
        TRACEV("release obj: {}, rc={}\n", FMT_STR(zGetTypeName(vm, getTypeId(val))), FMT_U32(obj->head.rc));
#if TRACE
        zCheckDoubleFree(vm, obj);
#endif
        obj->head.rc -= 1;
#if TRACK_GLOBAL_RC
    #if TRACE
        if (vm->refCounts == 0) {
            LOG("Double free. {}\n", FMT_U32(getTypeId(val)));
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
        TRACEV("release: {}, nop\n", FMT_STR(zGetTypeName(vm, getTypeId(val))));
    }
}

static inline void releaseObject(VM* vm, HeapObject* obj) {
    TRACEV("release obj: {}, rc={}\n", FMT_STR(zGetTypeName(vm, OBJ_TYPEID(obj))), FMT_U32(obj->head.rc));
#if (TRACE)
    zCheckDoubleFree(vm, obj);
#endif
    obj->head.rc -= 1;
#if TRACK_GLOBAL_RC
    #if TRACE
    if (vm->refCounts == 0) {
        LOG("Double free. {}\n", FMT_U32(OBJ_TYPEID(obj)));
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
    TRACEV("retain {} rc={}\n", FMT_STR(zGetTypeName(vm, OBJ_TYPEID(obj))), FMT_U32(obj->head.rc));
    obj->head.rc += 1;
#if TRACE
    zCheckRetainDanglingPointer(vm, obj);
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
        TRACEV("retain: {}, {}, rc={}\n", FMT_STR(zGetTypeName(vm, getTypeId(val))), FMT_PTR(obj), FMT_U32(obj->head.rc));
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
        TRACEV("retain: {}, nop\n", FMT_STR(zGetTypeName(vm, getTypeId(val))));
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
            return OBJ_TYPEID(VALUE_AS_HEAPOBJECT(val));
        } else {
            if (bits >= TAGGED_INTEGER_MASK) {
                return TYPE_INTEGER;
            } else if (bits >= TAGGED_ENUM_MASK) {
                return val & 0xffffffff;
            } else {
                return VALUE_GET_TAG(bits);
            }
        }
    } else {
        return TYPE_FLOAT;
    }
}

static inline uint32_t pcOffset(VM* vm, Inst* pc) {
    return (uintptr_t)pc - (uintptr_t)vm->instPtr;
}

static inline uint32_t stackOffset(VM* vm, Value* stack) {
    return ((uintptr_t)stack - (uintptr_t)vm->stackPtr) >> 3;
}

static inline uint8_t getFieldOffset(VM* vm, HeapObject* obj, uint32_t symId) {
    FieldSymbolMap* symMap = ((FieldSymbolMap*)vm->fieldSyms.buf) + symId;
    if (OBJ_TYPEID(obj) == symMap->mruTypeId) {
        return (uint8_t)symMap->mruOffset;
    } else {
        return zGetFieldOffsetFromTable(vm, OBJ_TYPEID(obj), symId);
    }
}

static inline bool isTypeCompat(TypeId typeId, TypeId cstrType) {
    if (typeId == cstrType) {
        return true;
    }
    if (cstrType == TYPE_ANY || cstrType == TYPE_DYNAMIC) {
        return true;
    }
    return false;
}

static inline Str getName(VM* vm, NameId nameId) {
    Name name = ((Name*)vm->names.buf)[nameId];
    return (Str){ .ptr = name.ptr, .len = name.len };
}

static inline FuncSig getResolvedFuncSig(VM* vm, FuncSigId id) {
    return ((FuncSig*)vm->compiler->sema.funcSigs.buf)[id];
}

static inline ValueResult allocObjectSmall(VM* vm, TypeId typeId, Value* fields, u8 numFields) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->object = (Object){
        .typeId = typeId | CYC_TYPE_MASK,
        .rc = 1,
    };

    Value* dst = objectGetValuesPtr(&res.obj->object);
    memcpy(dst, fields, numFields * sizeof(Value));

    return (ValueResult){ .val = VALUE_CYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocObject(VM* vm, TypeId typeId, Value* fields, u8 numFields) {
    // First slot holds the typeId and rc.
    HeapObjectResult res = zAllocExternalCycObject(vm, (1 + numFields) * sizeof(Value));
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->object = (Object){
        .typeId = typeId | CYC_TYPE_MASK,
        .rc = 1,
    };

    Value* dst = objectGetValuesPtr(&res.obj->object);
    memcpy(dst, fields, numFields * sizeof(Value));

    return (ValueResult){ .val = VALUE_CYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocEmptyMap(VM* vm) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->map = (Map){
        .typeId = TYPE_MAP | CYC_TYPE_MASK,
        .rc = 1,
        .inner = {
            .metadata = 0,
            .entries = 0,
            .size = 0,
            .cap = 0,
            .available = 0,
        },
    };
    return (ValueResult){ .val = VALUE_CYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocClosure(
    VM* vm, Value* fp, size_t funcPc, u8 numParams, u8 stackSize,
    u16 rFuncSigId, const Inst* capturedVals, u8 numCapturedVals, u8 closureLocal, bool reqCallTypeCheck
) {
    HeapObjectResult res;
    if (numCapturedVals <= 2) {
        res = zAllocPoolObject(vm);
    } else {
        res = zAllocExternalCycObject(vm, (2 + numCapturedVals) * sizeof(Value));
    }
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->closure = (Closure){
        .typeId = TYPE_CLOSURE | CYC_TYPE_MASK,
        .rc = 1,
        .funcPc = funcPc,
        .numParams = numParams,
        .stackSize = stackSize,
        .numCaptured = numCapturedVals,
        .local = closureLocal,
        .reqCallTypeCheck = reqCallTypeCheck,
        .rFuncSigId = rFuncSigId,
    };
    Value* dst = closureGetCapturedValuesPtr(&res.obj->closure);
    for (int i = 0; i < numCapturedVals; i += 1) {
        Inst local = capturedVals[i];
#if TRACE
        if (!VALUE_IS_BOX(fp[local])) {
            TRACEV("Expected box value.");
            zFatal();
        }
#endif
        retain(vm, fp[local]);
        dst[i] = fp[local];
    }
    return (ValueResult){ .val = VALUE_CYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocLambda(VM* vm, u32 funcPc, u8 numParams, u8 stackSize, u16 rFuncSigId, bool reqCallTypeCheck) {
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
        .reqCallTypeCheck = reqCallTypeCheck,
        .rFuncSigId = rFuncSigId,
    };
    return (ValueResult){ .val = VALUE_NOCYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocBox(VM* vm, Value val) {
    HeapObjectResult res = zAllocPoolObject(vm);
    if (UNLIKELY(res.code != RES_CODE_SUCCESS)) {
        return (ValueResult){ .code = res.code };
    }
    res.obj->box = (Box){
        .typeId = TYPE_BOX | CYC_TYPE_MASK,
        .rc = 1,
        .val = val,
    };
    return (ValueResult){ .val = VALUE_CYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
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
    return (ValueResult){ .val = VALUE_NOCYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocHostFunc(VM* vm, void* func, u32 numParams, u32 rFuncSigId, bool reqCallTypeCheck) {
    HeapObjectResult res = zAllocPoolObject(vm);
    res.obj->hostFunc = (HostFunc){
        .typeId = TYPE_HOST_FUNC,
        .rc = 1,
        .func = func,
        .numParams = numParams,
        .rFuncSigId = rFuncSigId,
        .hasTccState = false,
        .reqCallTypeCheck = reqCallTypeCheck,
    };
    return (ValueResult){ .val = VALUE_NOCYC_PTR(res.obj), .code = RES_CODE_SUCCESS };
}

static inline ValueResult allocFuncFromSym(VM* vm, FuncId funcId) {
    FuncSymbol sym = ((FuncSymbol*)vm->funcSyms.buf)[funcId];
    switch (sym.entryT) {
        case FUNC_SYM_HOSTFUNC: {
            u32 rFuncSigId = sym.innerExtra.nativeFunc1.rFuncSigId;
            u16 numParams = sym.innerExtra.nativeFunc1.typedFlagNumParams & ~((u16)1 << 15);
            bool reqCallTypeCheck = (sym.innerExtra.nativeFunc1.typedFlagNumParams & 0x8000) > 0;
            return allocHostFunc(vm, sym.inner.nativeFunc1, numParams, rFuncSigId, reqCallTypeCheck);
        }
        case FUNC_SYM_FUNC: {
            return allocLambda(vm, sym.inner.func.pc,
                sym.inner.func.numParams,
                sym.inner.func.stackSize,
                sym.innerExtra.func.rFuncSigId,
                sym.inner.func.reqCallTypeCheck
            );
        }
        case FUNC_SYM_CLOSURE: {
            retainObject(vm, sym.inner.closure);
            return (ValueResult){ .val = VALUE_CYC_PTR(sym.inner.closure), .code = RES_CODE_SUCCESS };
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

static void panicStaticMsg(VM* vm, const char* msg) {
    vm->curFiber->panicPayload = (u64)msg | (((u64)(strlen(msg))) << 48);
    vm->curFiber->panicType = PANIC_STATIC_MSG;
    TRACEV("{}", FMT_STRZ(msg));
}

static inline void panicDivisionByZero(VM* vm) {
    panicStaticMsg(vm, "Division by zero.");
}

static inline void panicOutOfBounds(VM* vm) {
    panicStaticMsg(vm, "Out of bounds.");
}

static inline void panicExpectedInteger(VM* vm) {
    panicStaticMsg(vm, "Expected integer operand.");
}

static inline void panicExpectedFloat(VM* vm) {
    panicStaticMsg(vm, "Expected float operand.");
}

static inline void panicFieldMissing(VM* vm) {
    panicStaticMsg(vm, "Field not found in value.");
}

static void panicIncompatibleType(VM* vm, TypeId actType, TypeId expType) {
    Str actTypeName = zGetTypeName(vm, actType);
    Str expTypeName = zGetTypeName(vm, expType);
    zPanicFmt(vm, "Expected type `{}`, got `{}` instead.", (FmtValue[]){
        FMT_STR(expTypeName), FMT_STR(actTypeName)
    }, 2);
}

static void panicIncompatibleFieldType(VM* vm, TypeId fieldTypeId, TypeId rightTypeId) {
    Str fieldTypeName = zGetTypeName(vm, fieldTypeId);
    Str rightTypeName = zGetTypeName(vm, rightTypeId);
    zPanicFmt(vm, "Assigning to `{}` field with incompatible type `{}`.", (FmtValue[]){
        FMT_STR(fieldTypeName), FMT_STR(rightTypeName)
    }, 2);
}

static void panicIncompatibleInitFieldType(VM* vm, TypeId fieldTypeId, TypeId rightTypeId) {
    Str fieldTypeName = zGetTypeName(vm, fieldTypeId);
    Str rightTypeName = zGetTypeName(vm, rightTypeId);
    zPanicFmt(vm, "Initializing `{}` field with incompatible type `{}`.", (FmtValue[]){
        FMT_STR(fieldTypeName), FMT_STR(rightTypeName)
    }, 2);
}

static void panicCastFail(VM* vm, TypeId actTypeId, TypeId expTypeId) {
    Str actName = zGetTypeName(vm, actTypeId);
    Str expName = zGetTypeName(vm, expTypeId);
    zPanicFmt(vm, "Can not cast `{}` to `{}`.", (FmtValue[]){
        FMT_STR(actName), FMT_STR(expName),
    }, 2);
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
        pc = zDeoptBinOp(vm, pc); \
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

#define SAVE_STATE(code) \
    do { \
        vm->curPc = pc; \
        vm->curStack = stack; \
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
    #define READ_U32_FROM(from, offset) ((uint32_t)from[offset] | ((uint32_t)from[offset+1] << 8) | ((uint32_t)from[offset+2] << 16) | ((uint32_t)from[offset+3] << 24))
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
        JENTRY(ConstRetain),
        JENTRY(ConstI8),
        JENTRY(AddFloat),
        JENTRY(SubFloat),
        JENTRY(True),
        JENTRY(False),
        JENTRY(None),
        JENTRY(Not),
        JENTRY(Copy),
        JENTRY(CopyReleaseDst),
        JENTRY(CopyRetainSrc),
        JENTRY(CopyRetainRelease),
        JENTRY(SetIndexList),
        JENTRY(SetIndexMap),
        JENTRY(IndexList),
        JENTRY(IndexTuple),
        JENTRY(IndexMap),
        JENTRY(AppendList),
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
        JENTRY(TypeCheck),
        JENTRY(ObjectField),
        JENTRY(Field),
        JENTRY(FieldIC),
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
        JENTRY(ObjectTypeCheck),
        JENTRY(ObjectSmall),
        JENTRY(Object),
        JENTRY(SetField),
        JENTRY(SetFieldIC),
        JENTRY(SetObjectField),
        JENTRY(SetObjectFieldCheck),
        JENTRY(PushTry),
        JENTRY(PopTry),
        JENTRY(Throw),
        JENTRY(Coinit),
        JENTRY(Coyield),
        JENTRY(Coresume),
        JENTRY(Coreturn),
        JENTRY(Retain),
        JENTRY(Box),
        JENTRY(SetBoxValue),
        JENTRY(SetBoxValueRelease),
        JENTRY(BoxValue),
        JENTRY(BoxValueRetain),
        JENTRY(Captured),
        JENTRY(SetCaptured),
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
        JENTRY(JumpNone),
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
        JENTRY(SeqDestructure),
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
    CASE(ConstOp): {
        stack[pc[3]] = VALUE_RAW(vm->constPtr[READ_U16(1)]);
        pc += 4;
        NEXT();
    }
    CASE(ConstRetain): {
        Value val = VALUE_RAW(vm->constPtr[READ_U16(1)]);
        retain(vm, val);
        stack[pc[3]] = val;
        pc += 4;
        NEXT();
    }
    CASE(ConstI8): {
        stack[pc[2]] = VALUE_INTEGER_CAST((int8_t)pc[1]);
        pc += 3;
        NEXT();
    }
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
        Value val = stack[pc[1]];
        bool bval = VALUE_IS_BOOLEAN(val) ? VALUE_AS_BOOLEAN(val) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(val);
        stack[pc[2]] = VALUE_BOOLEAN(!bval);
        pc += 3;
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
    CASE(CopyRetainSrc): {
        Value val = stack[pc[1]];
        retain(vm, val);
        stack[pc[2]] = val;
        pc += 3;
        NEXT();
    }
    CASE(CopyRetainRelease): {
        retain(vm, stack[pc[1]]);
        release(vm, stack[pc[2]]);
        stack[pc[2]] = stack[pc[1]];
        pc += 3;
        NEXT();
    }
    CASE(SetIndexList): {
        Value listv = stack[pc[1]];
        Value index = stack[pc[2]];
        Value right = stack[pc[3]];
        if (VALUE_IS_LIST(listv) && VALUE_IS_INTEGER(index)) {
            HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

            _BitInt(48) idx = VALUE_AS_INTEGER(index);
            if (idx >= 0 && idx < listo->list.list.len) {
                Value existing = ((Value*)listo->list.list.buf)[idx];
                release(vm, existing);
                retain(vm, right);
                ((Value*)listo->list.list.buf)[idx] = right;
                stack[pc[4]] = VALUE_NONE;
                pc += CALL_OBJ_SYM_INST_LEN;
                NEXT();
            } else {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }
        } else {
            if (!VALUE_IS_LIST(listv)) {
                DEOPTIMIZE_TERNOP();
                NEXT();
            }
            panicExpectedInteger(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(SetIndexMap): {
        Value mapv = stack[pc[1]];
        Value index = stack[pc[2]];
        Value right = stack[pc[3]];
        if (VALUE_IS_MAP(mapv)) {
            Map* mapo = (Map*)VALUE_AS_HEAPOBJECT(mapv);
            ResultCode code = zMapSet(vm, mapo, index, right);
            if (LIKELY(code == RES_CODE_SUCCESS)) {
                stack[pc[4]] = VALUE_NONE;
                pc += CALL_OBJ_SYM_INST_LEN;
                NEXT();
            }
            RETURN(code);
        } else {
            DEOPTIMIZE_TERNOP();
            NEXT();
        }
    }
    CASE(IndexList): {
        Value listv = stack[pc[1]];
        Value index = stack[pc[2]];
        if (VALUE_IS_LIST(listv) && VALUE_IS_INTEGER(index)) {
            HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

            _BitInt(48) idx = VALUE_AS_INTEGER(index);
            if (idx >= 0 && idx < listo->list.list.len) {
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
    CASE(IndexTuple): {
        Value tuplev = stack[pc[1]];
        Value index = stack[pc[2]];
        if (VALUE_IS_TUPLE(tuplev) && VALUE_IS_INTEGER(index)) {
            Tuple* tuple = (Tuple*)VALUE_AS_HEAPOBJECT(tuplev);

            _BitInt(48) idx = VALUE_AS_INTEGER(index);
            if (idx < 0) {
                // Reverse index.
                idx = tuple->len + idx;
            }
            if (idx >= 0 && idx < tuple->len) {
                Value val = ((Value*)&tuple->firstValue)[idx];
                retain(vm, val);
                stack[pc[3]] = val;
                pc += CALL_OBJ_SYM_INST_LEN;
                NEXT();
            } else {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }
        } else {
            if (!VALUE_IS_TUPLE(tuplev)) {
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
            Value val = zValueMapGet(&mapo->map.inner, index, &found);
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
    CASE(AppendList): {
        Value listv = stack[pc[1]];
        Value itemv = stack[pc[2]];
        if (VALUE_IS_LIST(listv)) {
            HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

            size_t len = listo->list.list.len;
            if (len == listo->list.list.cap) {
                // ensure cap.
                ResultCode code = zEnsureListCap(vm, &listo->list.list, len + 1);
                if (code != RES_CODE_SUCCESS) {
                    RETURN(code);
                }
            }

            retain(vm, itemv);
            ((Value*)listo->list.list.buf)[len] = itemv;
            listo->list.list.len = len + 1;
            stack[pc[3]] = VALUE_NONE;

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
        if (VALUE_IS_LIST(listv) && (VALUE_IS_INTEGER(startv) || VALUE_IS_NONE(startv)) && (VALUE_IS_INTEGER(endv) || VALUE_IS_NONE(endv))) {
            HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

            _BitInt(48) start;
            if (VALUE_IS_NONE(startv)) {
                start = 0;
            } else {
                start = VALUE_AS_INTEGER(startv);
            }
            if (start < 0) {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }

            _BitInt(48) end;
            if (VALUE_IS_NONE(endv)) {
                end = listo->list.list.len;
            } else {
                end = VALUE_AS_INTEGER(endv);
            }
            if (end > listo->list.list.len) {
                panicOutOfBounds(vm);
                RETURN(RES_CODE_PANIC);
            }

            if (end < start) {
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
        Value cond = stack[pc[1]];
        bool condVal = VALUE_IS_BOOLEAN(cond) ? VALUE_AS_BOOLEAN(cond) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(cond);
        if (condVal) {
            pc += (uintptr_t)READ_I16(2);
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
            TRACEV("release reg {}\n", FMT_U32(pc[i]));
            release(vm, stack[pc[i]]);
        }
        pc += 2 + numLocals;
        NEXT();
    }
    CASE(CallObjSym): {
        u8 ret = pc[1];
        u8 numArgs = pc[2];
        u8 mgId = pc[4];
        u16 anySelfFuncSigId = READ_U16(5);

        Value recv = stack[ret + CALL_ARG_START];
        TypeId typeId = getTypeId(recv);

        CallObjSymResult res = zCallObjSym(vm, pc, stack, recv, typeId, mgId, ret, numArgs, anySelfFuncSigId);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        pc = res.pc;
        stack = res.stack;
        NEXT();
    }
    CASE(CallObjNativeFuncIC): {
        u8 ret = pc[1];
        u8 numArgs = pc[2];
        Value recv = stack[ret + CALL_ARG_START];
        TypeId typeId = getTypeId(recv);

        TypeId cachedTypeId = READ_U16(14);
        if (typeId == cachedTypeId) {
            SAVE_STATE();
            HostFuncFn fn = (HostFuncFn)READ_U48(8);
            Value res = fn(vm, stack + ret + CALL_ARG_START, numArgs);
            if (res == VALUE_INTERRUPT) {
                RETURN(RES_CODE_PANIC);
            }
            stack[ret] = res;
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
        u8 ret = pc[1];
        Value recv = stack[ret + CALL_ARG_START];
        TypeId typeId = getTypeId(recv);

        TypeId cachedTypeId = READ_U16(14);
        if (typeId == cachedTypeId) {
            u8 numLocals = pc[7];
            if (stack + ret + numLocals >= vm->stackEndPtr) {
                RETURN(RES_CODE_STACK_OVERFLOW);
            }
            Value retFramePtr = (uintptr_t)stack;
            stack += ret;
            stack[1] = VALUE_RETINFO(false, CALL_OBJ_SYM_INST_LEN);
            stack[2] = (uintptr_t)(pc + CALL_OBJ_SYM_INST_LEN);
            stack[3] = retFramePtr;
            pc = vm->instPtr + READ_U32(8);
            NEXT();
        }

        // Deoptimize.
        pc[0] = CodeCallObjSym;
        NEXT();
    }
    CASE(CallTypeCheck): {
        u8 argStartReg = pc[1];
        u8 numArgs = pc[2];
        u16 funcSigId = READ_U16(3);

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
            TypeId cstrTypeId = funcSig.paramPtr[i];
            TypeId argTypeId = getTypeId(args[i]);

            if (!isTypeCompat(argTypeId, cstrTypeId)) {
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
        u8 ret = pc[1];
        u8 numArgs = pc[2];
        u16 symId = READ_U16(4);
        PcSpResult res = zCallSym(vm, pc, stack, symId, ret, numArgs);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        pc = res.pc;
        stack = res.sp;
        NEXT();
    }
    CASE(CallFuncIC): {
        u8 ret = pc[1];
        u8 numLocals = pc[4];
        if (stack + ret + numLocals >= vm->stackEndPtr) {
            RETURN(RES_CODE_STACK_OVERFLOW);
        }

        Value retFramePtr = (uintptr_t)stack;
        stack += ret;
        stack[1] = VALUE_RETINFO(false, CALL_SYM_INST_LEN);
        stack[2] = (uintptr_t)(pc + CALL_SYM_INST_LEN);
        stack[3] = retFramePtr;
        pc = (Inst*)READ_U48(6);
        NEXT();
    }
    CASE(CallNativeFuncIC): {
        u8 ret = pc[1];
        u8 numArgs = pc[2];

        SAVE_STATE();
        Value* newStack = stack + ret;
        HostFuncFn fn = (HostFuncFn)READ_U48(6);
        Value res = fn(vm, newStack + CALL_ARG_START, numArgs);
        if (res == VALUE_INTERRUPT) {
            RETURN(RES_CODE_PANIC);
        }
        newStack[0] = res;
        pc += CALL_SYM_INST_LEN;
        NEXT();
    }
    CASE(Ret1): {
        u8 retFlag = VALUE_RETINFO_RETFLAG(stack[1]);
        if (retFlag == 0) {
            pc = (Inst*)stack[2];
            stack = (Value*)stack[3];
            NEXT();
        } else {
            RETURN(RES_CODE_SUCCESS);
        }
    }
    CASE(Ret0): {
        uint8_t retFlag = VALUE_RETINFO_RETFLAG(stack[1]);
        stack[0] = VALUE_NONE;
        if (retFlag == 0) {
            pc = (Inst*)stack[2];
            stack = (Value*)stack[3];
            NEXT();
        } else {
            RETURN(RES_CODE_SUCCESS);
        }
    }
    CASE(Call): {
        u8 ret = pc[1];
        u8 numArgs = pc[2];

        Value callee = stack[ret + CALLEE_START];
        Value retInfo = VALUE_RETINFO(false, CALL_INST_LEN);
        PcSpResult res = zCall(vm, pc, stack, callee, ret, numArgs, retInfo);
        if (LIKELY(res.code == RES_CODE_SUCCESS)) {
            pc = res.pc;
            stack = res.sp;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(TypeCheck): {
        Value val = stack[pc[1]];
        TypeId expType = (TypeId)READ_U16(2);
        TypeId actType = getTypeId(val);
        if (!isTypeCompat(actType, expType)) {
            panicIncompatibleType(vm, actType, expType);
            RETURN(RES_CODE_PANIC);
        }
        pc += 4;
        NEXT();
    }
    CASE(ObjectField): {
        Value recv = stack[pc[1]];
        HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
        stack[pc[3]] = objectGetField((Object*)obj, pc[2]);
        retain(vm, stack[pc[3]]);
        pc += 4;
        NEXT();
    }
    CASE(Field): {
        uint8_t left = pc[1];
        uint8_t dst = pc[2];
        uint16_t symId = READ_U16(3);
        Value recv = stack[left];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            uint8_t offset = getFieldOffset(vm, obj, symId);
            if (offset != NULL_U8) {
                stack[dst] = objectGetField((Object*)obj, offset);
                pc[0] = CodeFieldIC;
                WRITE_U16(5, OBJ_TYPEID(obj));
                pc[7] = offset;
            } else {
                stack[dst] = zGetFieldFallback(vm, obj, ((FieldSymbolMap*)vm->fieldSyms.buf)[symId].nameId);
            }
            retain(vm, stack[dst]);
            pc += 8;
            NEXT();
        } else {
            panicFieldMissing(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(FieldIC): {
        Value recv = stack[pc[1]];
        uint8_t dst = pc[2];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            if (OBJ_TYPEID(obj) == READ_U16(5)) {
                stack[dst] = objectGetField((Object*)obj, pc[7]);
                retain(vm, stack[dst]);
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
    CASE(Lambda): {
        u16 funcOff = READ_U16(1);
        u32 funcPc = ((uint32_t)getInstOffset(vm, pc)) - funcOff;
        u8 numParams = pc[3];
        u8 stackSize = pc[4];
        bool reqCallTypeCheck = pc[5];
        u16 rFuncSigId = READ_U16(6);
        ValueResult res = allocLambda(vm, funcPc, numParams, stackSize, rFuncSigId, reqCallTypeCheck);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[8]] = res.val;
        pc += 9;
        NEXT();
    }
    CASE(Closure): {
        u16 funcOff = READ_U16(1);
        u32 funcPc = getInstOffset(vm, pc) - funcOff;
        u8 numParams = pc[3];
        u8 numCaptured = pc[4];
        u8 stackSize = pc[5];
        u16 rFuncSigId = READ_U16(6);
        u8 local = pc[8];
        bool reqCallTypeCheck = pc[9];
        u8 dst = pc[10];
        Inst* capturedVals = pc + 11;

        ValueResult res = allocClosure(vm, stack, funcPc, numParams, stackSize, rFuncSigId, capturedVals, numCaptured, local, reqCallTypeCheck);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = res.val;
        pc += 11 + numCaptured;
        NEXT();
    }
    CASE(Compare): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (left == right) {
            stack[pc[3]] = VALUE_TRUE;
        } else {
            stack[pc[3]] = zEvalCompare(left, right);
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
            stack[pc[3]] = zEvalCompareNot(left, right);
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
    CASE(ObjectTypeCheck): {
        u8 startLocal = pc[1];
        u8 n = pc[2];
        u8* dataPtr = pc + 3;

        // Perform type check on field initializers.
        for (int i = 0; i < n; i += 1) {
            TypeId valTypeId = getTypeId(stack[startLocal + dataPtr[0]]);
            TypeId cstrTypeId = READ_U32_FROM(dataPtr, 1);

            if (!isTypeCompat(valTypeId, cstrTypeId)) {
                panicIncompatibleInitFieldType(vm, cstrTypeId, valTypeId);
                RETURN(RES_CODE_PANIC);
            }
            dataPtr += 5;
        }
        pc = dataPtr;
        NEXT();
    }
    CASE(ObjectSmall): {
        u16 typeId = READ_U16(1);
        u8 startLocal = pc[3];
        u8 numFields = pc[4];
        ValueResult res = allocObjectSmall(vm, typeId, stack + startLocal, numFields);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[5]] = res.val;
        pc += 6;
        NEXT();
    }
    CASE(Object): {
        u16 typeId = READ_U16(1);
        u8 startLocal = pc[3];
        u8 numFields = pc[4];
        ValueResult res = allocObject(vm, typeId, stack + startLocal, numFields);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[5]] = res.val;
        pc += 6;
        NEXT();
    }
    CASE(SetField): {
        Value recv = stack[pc[1]];
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            u16 fieldId = READ_U16(2);
            u8 offset = getFieldOffset(vm, obj, fieldId);
            if (offset != NULL_U8) {
                Value val = stack[pc[4]];
                FieldSymbolMap* symMap = ((FieldSymbolMap*)vm->fieldSyms.buf) + fieldId;
                u32 fieldTypeId = symMap->mruFieldTypeSymId;
                TypeId rightTypeId = getTypeId(val);
                if (fieldTypeId != TYPE_DYNAMIC) {
                    // Must perform type check on rhs.
                    if (!isTypeCompat(rightTypeId, fieldTypeId)) {
                        panicIncompatibleFieldType(vm, fieldTypeId, rightTypeId);
                        RETURN(RES_CODE_PANIC);
                    }
                }

                Value* lastValue = objectGetFieldPtr((Object*)obj, offset);
                release(vm, *lastValue);
                *lastValue = val;

                pc[0] = CodeSetFieldIC;
                WRITE_U16(5, OBJ_TYPEID(obj));
                WRITE_U16(7, rightTypeId);
                pc[9] = offset;
                pc += 10;
                NEXT();
            } else {
                panicFieldMissing(vm);
                RETURN(RES_CODE_PANIC);
            }
        } else {
            // return vm.setFieldNotObjectError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(SetFieldIC): {
        Value recv = stack[pc[1]];
        if (VALUE_IS_POINTER(recv)) {
            Value val = stack[pc[4]];
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            if ((OBJ_TYPEID(obj) == READ_U16(5)) && (getTypeId(val) == READ_U16(7))) {
                Value* lastValue = objectGetFieldPtr((Object*)obj, pc[9]);
                release(vm, *lastValue);
                *lastValue = val;
                pc += 10;
                NEXT();
            } else {
                // Deoptimize.
                pc[0] = CodeSetField;
                NEXT();
            }
        } else {
            // return vm.getFieldMissingSymbolError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(SetObjectField): {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(stack[pc[1]]);
        Value* lastValue = objectGetFieldPtr((Object*)obj, pc[2]);
        release(vm, *lastValue);
        *lastValue = stack[pc[3]];
        pc += 4;
        NEXT();
    }
    CASE(SetObjectFieldCheck): {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(stack[pc[1]]);
        TypeId cstrType = READ_U16(2);
        TypeId valType = getTypeId(stack[pc[4]]);
        if (!isTypeCompat(valType, cstrType)) {
            panicIncompatibleFieldType(vm, cstrType, valType);
            RETURN(RES_CODE_PANIC);
        }
        Value* lastValue = objectGetFieldPtr((Object*)obj, pc[5]);
        release(vm, *lastValue);
        *lastValue = stack[pc[4]];
        pc += 6;
        NEXT();
    }
    CASE(PushTry): {
        u8 errDst = pc[1];
        bool releaseDst = pc[2];
        u16 catchPcOffset = READ_U16(3);
        if (vm->tryStack.len == vm->tryStack.cap) {
            ResultCode code = zGrowTryStackTotalCapacity(&vm->tryStack, vm->alloc, vm->tryStack.len + 1);
            if (code != RES_CODE_SUCCESS) {
                RETURN(code);
            }
        }
        ((TryFrame*)vm->tryStack.buf)[vm->tryStack.len] = (TryFrame){
            .fp = getStackOffset(vm, stack),
            .catchPc = getInstOffset(vm, pc) + catchPcOffset,
            .catchErrDst = errDst,
            .releaseDst = releaseDst,
        };
        vm->tryStack.len += 1; 
        pc += 5;
        NEXT();
    }
    CASE(PopTry): {
        vm->tryStack.len -= 1; 
        pc += READ_U16(1);
        NEXT();
    }
    CASE(Throw): {
        Value err = stack[pc[1]];
        if (VALUE_IS_ERROR(err)) {
            vm->curFiber->panicPayload = err;
            vm->curFiber->panicType = PANIC_NATIVE_THROW;
            RETURN(RES_CODE_PANIC);
        } else {
            panicStaticMsg(vm, "Not an error.");
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Coinit): {
        u8 startArgsLocal = pc[1];
        u8 numArgs = pc[2];
        u8 argDst = pc[3];
        u8 jump = pc[4];
        u8 initialStackSize = pc[5];
        u8 dst = pc[6];

        ValueResult res = zAllocFiber(vm, pcOffset(vm, pc + INST_COINIT_LEN), stack + startArgsLocal, numArgs, argDst, initialStackSize);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = res.val;
        pc += jump;
        NEXT();
    }
    CASE(Coyield):
        if (vm->curFiber != &vm->mainFiber) {
            PcSpOff res = zPopFiber(vm, pcOffset(vm, pc), stack, VALUE_NONE);
            pc = vm->instPtr + res.pc;
            stack = vm->stackPtr + res.sp;
        } else {
            pc += 3;
        }
        NEXT();
    CASE(Coresume): {
        Value fiber = stack[pc[1]];
        if (VALUE_IS_POINTER(fiber)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(fiber);
            if (OBJ_TYPEID(obj) == TYPE_FIBER) {
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
            PcSpOff res = zPopFiber(vm, NULL_U32, stack, stack[1]);
            pc = vm->instPtr + res.pc;
            stack = vm->stackPtr + res.sp;
        }
        NEXT();
    }
    CASE(Retain): {
        retain(vm, stack[pc[1]]);
        pc += 2;
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
        ASSERT(OBJ_TYPEID(obj) == TYPE_BOX);
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
        ASSERT(OBJ_TYPEID(obj) == TYPE_BOX);
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
            TRACEV("Expected box value.");
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
            TRACEV("Expected box value.");
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
            TRACEV("Expected closure value.");
            zFatal();
        }
#endif
        Value box = closureGetCapturedValuesPtr(&VALUE_AS_HEAPOBJECT(closure)->closure)[pc[2]];
#if TRACE
        if (!VALUE_IS_BOX(box)) {
            TRACEV("Expected box value.");
            zFatal();
        }
#endif
        Value val = VALUE_AS_HEAPOBJECT(box)->box.val;
        retain(vm, val);
        stack[pc[3]] = val;
        pc += 4;
        NEXT();
    }
    CASE(SetCaptured): {
        Value closure = stack[pc[1]];
#if TRACE
        if (!VALUE_IS_CLOSURE(closure)) {
            TRACEV("Expected closure value.");
            zFatal();
        }
#endif
        Value box = closureGetCapturedValuesPtr(&VALUE_AS_HEAPOBJECT(closure)->closure)[pc[2]];
        HeapObject* obj = VALUE_AS_HEAPOBJECT(box);
#if TRACE
        ASSERT(OBJ_TYPEID(obj) == TYPE_BOX);
#endif
        release(vm, obj->box.val);
        obj->box.val = stack[pc[3]];
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
            stack[pc[4]] = val;
            pc += 5;
            NEXT();
        } else {
            panicCastFail(vm, getTypeId(val), expTypeId);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(CastAbstract): {
        Value val = stack[pc[1]];
        u16 expTypeId = READ_U16(2);
        if (expTypeId == TYPE_ANY) {
            stack[pc[4]] = val;
            pc += 5;
            NEXT();
        } else {
            panicCastFail(vm, getTypeId(val), expTypeId);
            RETURN(RES_CODE_PANIC);
        }
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
    CASE(JumpNone): {
        int16_t offset = READ_I16(1);
        if (VALUE_IS_NONE(stack[pc[3]])) {
            pc += offset;
            NEXT();
        } else {
            pc += 4;
            NEXT();
        }
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
        bool increment = pc[3];
        if (VALUE_BOTH_INTEGERS(startv, endv)) {
            _BitInt(48) start = VALUE_AS_INTEGER(startv);
            _BitInt(48) end = VALUE_AS_INTEGER(endv);
            stack[pc[4]] = startv;
            stack[pc[5]] = startv;
            u16 offset = READ_U16(6);
            if (increment) {
                if (start >= end) {
                    pc += offset + 6;
                    NEXT();
                }
                pc[offset] = CodeForRange;
            } else {
                if (start <= end) {
                    pc += offset + 6;
                    NEXT();
                }
                pc[offset] = CodeForRangeReverse;
            }
            pc += 8;
            NEXT();
        } else {
            panicExpectedInteger(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(ForRange): {
        _BitInt(48) counter = VALUE_AS_INTEGER(stack[pc[1]]) + 1;
        if (counter < VALUE_AS_INTEGER(stack[pc[2]])) {
            stack[pc[1]] = VALUE_INTEGER(counter);
            stack[pc[3]] = VALUE_INTEGER(counter);
            pc -= READ_U16(4);
        } else {
            pc += 6;
        }
        NEXT();
    }
    CASE(ForRangeReverse): {
        _BitInt(48) counter = VALUE_AS_INTEGER(stack[pc[1]]) - 1;
        if (counter > VALUE_AS_INTEGER(stack[pc[2]])) {
            stack[pc[1]] = VALUE_INTEGER(counter);
            stack[pc[3]] = VALUE_INTEGER(counter);
            pc -= READ_U16(4);
        } else {
            pc += 6;
        }
        NEXT();
    }
    CASE(SeqDestructure): {
        Value val = stack[pc[1]];
        u8 numDst = pc[2];

        TypeId typeId = getTypeId(val);
        if (typeId == TYPE_TUPLE) {
            Tuple* tuple = (Tuple*)VALUE_AS_HEAPOBJECT(val);
            if (tuple->len < numDst) {
                panicStaticMsg(vm, "Not enough elements.");
                RETURN(RES_CODE_PANIC);
            }
            for (int i = 0; i < numDst; i += 1) {
                Value elem = (&tuple->firstValue)[i];
                retain(vm, elem);
                stack[pc[3+i]] = elem;
            }
            pc += 3 + numDst;
            NEXT();
        } else {
            panicStaticMsg(vm, "Expected tuple.");
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Match): {
        pc += zOpMatch(pc, stack);
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
        u8 symType = pc[1];
        u32 symId = READ_U32(2);
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