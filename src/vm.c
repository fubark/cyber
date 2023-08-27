#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "vm.h"

#define STATIC_ASSERT(cond, msg) typedef char static_assertion_##msg[(cond)?1:-1]
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)
#define DLOG(fmt, ...) \
    do { if (DEBUG) fprintf(stderr, fmt, __VA_ARGS__); } while (0)

#define SIGN_MASK ((uint64_t)1 << 63)
#define TAGGED_VALUE_MASK ((uint64_t)0x7ffc000000000000)
#define TAG_MASK (((uint32_t)1 << 3) - 1)
#define TAGGED_PRIMITIVE_MASK (TAGGED_VALUE_MASK | ((uint64_t)TAG_MASK << 32))
#define TAG_NONE ((uint8_t)0)
#define TAG_BOOLEAN ((uint8_t)1)
#define TAG_ERROR ((uint8_t)2)
#define TAG_STATIC_ASTRING ((uint8_t)3)
#define TAG_STATIC_USTRING ((uint8_t)4)
#define TAG_SYMBOL ((uint8_t)6)
#define TAG_INTEGER ((uint8_t)7)
#define INTEGER_MASK (TAGGED_VALUE_MASK | ((uint64_t)TAG_INTEGER << 32))
#define BOOLEAN_MASK (TAGGED_VALUE_MASK | ((uint64_t)TAG_BOOLEAN << 32))
#define FALSE_MASK BOOLEAN_MASK
#define TRUE_BIT_MASK ((uint64_t)1)
#define TRUE_MASK (BOOLEAN_MASK | TRUE_BIT_MASK)
#define NONE_MASK (TAGGED_VALUE_MASK | ((u64)TAG_NONE << 32))
#define POINTER_MASK (TAGGED_VALUE_MASK | SIGN_MASK)
#define ERROR_MASK (TAGGED_VALUE_MASK | ((u64)TAG_ERROR << 32)
#define SYMBOL_MASK (TAGGED_VALUE_MASK | ((u64)TAG_SYMBOL << 32))
#define STATIC_ASTRING_MASK (TAGGED_VALUE_MASK | ((u64)TAG_STATIC_ASTRING << 32))
#define STATIC_USTRING_MASK (TAGGED_VALUE_MASK | ((u64)TAG_STATIC_USTRING << 32))
#define BEFORE_TAG_MASK ((u32)(0x00007fff << 3))
#define NULL_U32 UINT32_MAX
#define NULL_U8 UINT8_MAX

// Construct value.
#define VALUE_INTEGER(n) (INTEGER_MASK | n)
#define VALUE_BOOLEAN(b) (b ? TRUE_MASK : FALSE_MASK)
#define VALUE_NONE NONE_MASK
#define VALUE_NUMBER(n) ((ValueUnion){ .d = n }.u)
#define VALUE_RETINFO(nrv, rf, cio) (Value)(nrv | (((uint32_t)rf) << 8) | (((uint32_t)cio) << 16))
#define VALUE_TRUE TRUE_MASK
#define VALUE_FALSE FALSE_MASK
#define VALUE_RAW(u) u
#define VALUE_PTR(ptr) (POINTER_MASK | (uint64_t)ptr)
#define VALUE_STATIC_STRING_SLICE(v) ((IndexSlice){ .start = v & 0xffffffff, .len = (((u32)(v >> 32)) & BEFORE_TAG_MASK) >> 3 })
#define VALUE_SYMBOL(symId) (SYMBOL_MASK | symId);

// Value ops.
#define VALUE_AS_HEAPOBJECT(v) ((HeapObject*)(v & ~POINTER_MASK))
#define VALUE_AS_INTEGER(v) ((int32_t)(v & 0xffffffff))
#define VALUE_AS_NUMBER(v) ((ValueUnion){ .u = v }.d)
#define VALUE_AS_NUMBER_TO_INT(v) ((int32_t)VALUE_AS_NUMBER(v))
#define VALUE_AS_BOOLEAN(v) (v == TRUE_MASK)
#define VALUE_IS_BOOLEAN(v) ((v & (TAGGED_PRIMITIVE_MASK | SIGN_MASK)) == BOOLEAN_MASK)
#define VALUE_IS_POINTER(v) ((v & POINTER_MASK) == POINTER_MASK)
#define VALUE_ASSUME_NOT_BOOL_TO_BOOL(v) (!VALUE_IS_NONE(v))
#define VALUE_IS_NONE(v) (v == NONE_MASK)
#define VALUE_IS_PANIC(v) (ERROR_MASK | (uint32_t)(0xff << 8) | UINT8_MAX)
#define VALUE_IS_NUMBER(v) ((v & TAGGED_VALUE_MASK) != TAGGED_VALUE_MASK)
#define VALUE_BOTH_NUMBERS(v1, v2) (VALUE_IS_NUMBER(v1) && VALUE_IS_NUMBER(v2))
#define VALUE_GET_TAG(v) (((uint32_t)(v >> 32)) & TAG_MASK)
#define VALUE_RETINFO_NUMRETVALS(v) (v & 0xff)
#define VALUE_RETINFO_RETFLAG(v) (v & 0xff00)

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

static inline void release(VM* vm, Value val) {
#if TRACE_ENABLED
    vm->trace->numReleaseAttempts += 1;
#endif
    if (VALUE_IS_POINTER(val)) {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(val);
        // if (builtin.mode == .Debug) {
        //     if (obj.head.typeId == cy.NullId) {
        //         log.debug("object already freed. {*}", .{obj});
        //         cy.debug.dumpObjectTrace(vm, obj) catch stdx.fatal();
        //         stdx.fatal();
        //     }
        // }
        obj->head.rc -= 1;
        // if (builtin.mode == .Debug) {
        //     if (cy.verbose) {
        //         log.debug("release {} {}", .{val.getUserTag(), obj.head.rc});
        //     }
        // }
#if TRACK_GLOBAL_RC
        vm->refCounts -= 1;
#endif
#if TRACE_ENABLED
        vm->trace->numReleases += 1;
#endif
        if (obj->head.rc == 0) {
            zFreeObject(vm, obj);
        }
    }
}

static inline void releaseObject(VM* vm, HeapObject* obj) {
    // if (builtin.mode == .Debug or builtin.is_test) {
    //     if (obj.head.typeId == cy.NullId) {
    //         stdx.panic("object already freed.");
    //     }
    // }
    obj->head.rc -= 1;
    // if (builtin.mode == .Debug) {
    //     if (cy.verbose) {
    //         log.debug("release {} {}", .{obj.getUserTag(), obj.head.rc});
    //     }
    // }
#if TRACK_GLOBAL_RC
        vm->refCounts -= 1;
#endif
#if TRACE_ENABLED
    vm->trace->numReleases += 1;
    vm->trace->numReleaseAttempts += 1;
#endif
    if (obj->head.rc == 0) {
        zFreeObject(vm, obj);
    }
}

static inline void retain(VM* vm, Value val) {
#if TRACE_ENABLED
    vm->trace->numRetainAttempts += 1;
#endif
    if (VALUE_IS_POINTER(val)) {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(val);
        obj->head.rc += 1;
        // if (builtin.mode == .Debug) {
        //     if (cy.verbose) {
        //         log.debug("retain {} {}", .{obj.getUserTag(), obj.head.rc});
        //     }
        // }
#if TRACK_GLOBAL_RC
        vm->refCounts += 1;
#endif
#if TRACE_ENABLED
        vm->trace->numRetains += 1;
#endif
    }
}

static inline double toF64(Value val) {
    if (VALUE_IS_NUMBER(val)) {
        return VALUE_AS_NUMBER(val);
    } else {
        return zOtherToF64(val);
    }
}

static inline TypeId getPrimitiveTypeId(Value val) {
    if (VALUE_IS_NUMBER(val)) {
        return TYPE_NUMBER;
    } else {
        return VALUE_GET_TAG(val);
    }
}

static inline TypeId getTypeId(Value val) {
    if (VALUE_IS_POINTER(val)) {
        return VALUE_AS_HEAPOBJECT(val)->head.typeId;
    } else {
        return getPrimitiveTypeId(val);
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

static inline Str getName(VM* vm, NameId nameId) {
    Name name = ((Name*)vm->compiler.sema.nameSyms.buf)[nameId];
    return (Str){ .ptr = name.ptr, .len = name.len };
}

static inline ResolvedFuncSig getResolvedFuncSig(VM* vm, ResolvedFuncSigId id) {
    return ((ResolvedFuncSig*)vm->compiler.sema.resolvedFuncSigs.buf)[id];
}

static inline ResolvedSym getResolvedSym(VM* vm, ResolvedSymId id) {
    return ((ResolvedSym*)vm->compiler.sema.resolvedSyms.buf)[id];
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

static inline Str getValueStaticString(VM* vm, Value val) {
    IndexSlice slice = VALUE_STATIC_STRING_SLICE(val);
    return (Str){ .ptr = vm->strBufPtr + slice.start, .len = slice.len };
}

static void panicStaticMsg(VM* vm, const char* msg) {
    vm->curFiber->panicPayload = (u64)msg | (((u64)(strlen(msg))) << 48);
    vm->curFiber->panicType = PANIC_STATIC_MSG;
    DLOG("%s", msg);
}

static inline void panicExpectedNumber(VM* vm) {
    return panicStaticMsg(vm, "Expected number operand.");
}

static void panicFmt(VM* vm, const char* format, ...) {
    va_list arglist;

    va_start(arglist, format);
    int bufSize = vsnprintf(NULL, 0, format, arglist) + 1;
    BufferResult res = zAlloc(vm->alloc, bufSize);
    if (res.code != RES_CODE_SUCCESS) {
        vm->curFiber->panicType = PANIC_INFLIGHT_OOM;
        return;
    }
    vsnprintf(res.buf, bufSize, format, arglist);
    va_end(arglist);

    vm->curFiber->panicPayload = (u64)res.buf | (((u64)bufSize) << 48);
    vm->curFiber->panicType = PANIC_MSG;
    DLOG("%s", (char *)res.buf);
}

#define RETURN(code) \
    vm->curPc = pc; \
    vm->curStack = stack; \
    return code

ResultCode execBytecode(VM* vm) {
    #define READ_I16(offset) ((int16_t)(pc[offset] | ((uint16_t)pc[offset + 1] << 8)))
    #define READ_U16(offset) (pc[offset] | ((uint16_t)pc[offset + 1] << 8))
    #define WRITE_U16(offset, u) pc[offset] = u & 0xff; pc[offset+1] = u >> 8
    #define READ_U32(offset) ((uint32_t)pc[offset] | ((uint32_t)pc[offset+1] << 8) | ((uint32_t)pc[offset+2] << 16) | ((uint32_t)pc[offset+3] << 24))
    #define READ_U48(offset) ((uint64_t)pc[offset] | ((uint64_t)pc[offset+1] << 8) | ((uint64_t)pc[offset+2] << 16) | ((uint64_t)pc[offset+3] << 24) | ((uint64_t)pc[offset+4] << 32) | ((uint64_t)pc[offset+5] << 40))
#if DEBUG    
    #define PRE_NEXT() \
        if (verbose) { \
            zDumpEvalOp(vm, pc); \
        } \
        //vm.debugPc = pcOffset(vm, pc);
#else
    #define PRE_NEXT()
#endif
#if CGOTO
    #define JENTRY(op) &&Code_##op
    static void* jumpTable[] = {
        JENTRY(ConstOp),
        JENTRY(ConstI8),
        JENTRY(ConstI8Int),
        JENTRY(Add),
        JENTRY(Sub),
        JENTRY(True),
        JENTRY(False),
        JENTRY(None),
        JENTRY(Not),
        JENTRY(Copy),
        JENTRY(CopyReleaseDst),
        JENTRY(SetIndex),
        JENTRY(SetIndexRelease),
        JENTRY(CopyRetainSrc),
        JENTRY(Index),
        JENTRY(ReverseIndex),
        JENTRY(List),
        JENTRY(Map),
        JENTRY(MapEmpty),
        JENTRY(Slice),
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
        JENTRY(Call0),
        JENTRY(Call1),
        JENTRY(Field),
        JENTRY(FieldIC),
        JENTRY(FieldRetain),
        JENTRY(FieldRetainIC),
        JENTRY(Lambda),
        JENTRY(Closure),
        JENTRY(Compare),
        JENTRY(Less),
        JENTRY(Greater),
        JENTRY(LessEqual),
        JENTRY(GreaterEqual),
        JENTRY(Mul),
        JENTRY(Div),
        JENTRY(Pow),
        JENTRY(Mod),
        JENTRY(CompareNot),
        JENTRY(StringTemplate),
        JENTRY(Neg),
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
        JENTRY(LessInt),
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
    #define NEXT() PRE_NEXT(); goto *jumpTable[*pc]
#else
    // case name matches enum.
    #define CASE(op) case Code##op
    #define NEXT() PRE_NEXT(); goto beginSwitch
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
        stack[pc[2]] = VALUE_NUMBER((double)(int8_t)pc[1]);
        pc += 3;
        NEXT();
    CASE(ConstI8Int):
        stack[pc[2]] = VALUE_INTEGER((int32_t)pc[1]);
        pc += 3;
        NEXT();
    CASE(Add): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_NUMBER(VALUE_AS_NUMBER(left) + VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Sub): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_NUMBER(VALUE_AS_NUMBER(left) - VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(True):
        stack[pc[1]] = VALUE_TRUE;
        pc += 2;
        NEXT();
    CASE(False):
        stack[pc[1]] = VALUE_FALSE;
        pc += 2;
        NEXT();
    CASE(None):
        stack[pc[1]] = VALUE_NONE;
        pc += 2;
        NEXT();
    CASE(Not):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
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
    CASE(Index): {
        Value* recv = &stack[pc[1]];
        Value indexv = stack[pc[2]];
        ValueResult res = zGetIndex(vm, recv, indexv);
        if (res.code != RES_CODE_SUCCESS) {
            RETURN(res.code);
        }
        stack[pc[3]] = res.val;
        pc += 4;
        NEXT();
    }
    CASE(ReverseIndex):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
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
    CASE(Slice):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
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
    CASE(JumpCond):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
    CASE(Jump):
        pc += READ_I16(1);
        NEXT();
    CASE(Release):
        release(vm, stack[pc[1]]);
        pc += 2;
        NEXT();
    CASE(ReleaseN): {
        uint8_t numLocals = pc[1];
        uint8_t i;
        for (i = 2; i < 2 + numLocals; i += 1) {
            release(vm, stack[pc[i]]);
        }
        pc += 2 + numLocals;
        NEXT();
    }
    CASE(CallObjSym): {
        uint8_t startLocal = pc[1];
        uint8_t numArgs = pc[2];
        uint8_t numRet = pc[3];
        uint8_t symId = pc[4];
        uint16_t anySelfFuncSigId = READ_U16(5);

        Value recv = stack[startLocal + numArgs + 4 - 1];
        TypeId typeId = getTypeId(recv);

        CallObjSymResult res = zCallObjSym(vm, pc, stack, recv, typeId, symId, startLocal, numArgs, numRet, anySelfFuncSigId);
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
        Value recv = stack[startLocal + numArgs + 4 - 1];
        TypeId typeId = getTypeId(recv);

        TypeId cachedTypeId = READ_U16(14);
        if (typeId == cachedTypeId) {
            // const newFramePtr = framePtr + startLocal;
            vm->curStack = stack;
            MethodPtr fn = (MethodPtr)READ_U48(8);
            Value res = fn(vm, recv, stack + startLocal + 4, numArgs);
            // if (VALUE_IS_PANIC(res)) {
            //     return error.Panic;
            // }
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
        uint8_t numArgs = pc[2];
        uint8_t recv = stack[startLocal + numArgs + 4 - 1];
        TypeId typeId = getTypeId(recv);

        TypeId cachedTypeId = READ_U16(14);
        if (typeId == cachedTypeId) {
            uint8_t numLocals = pc[7];
            if (stack + startLocal + numLocals >= vm->stackEndPtr) {
                // return error.StackOverflow;
                zFatal();
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

        ResolvedFuncSig funcSig = getResolvedFuncSig(vm, funcSigId);
        Value* args = stack + argStartReg;

        // TODO: numArgs and this check can be removed if overloaded symbols are grouped by numParams.
        if (numArgs != funcSig.paramLen) {
            uint16_t funcId = READ_U16(5 + 4);
            RETURN(RES_CODE_UNKNOWN);
            // return panicIncompatibleFuncSig(vm, funcId, args, funcSigId);
        }

        // Perform type check on args.
        for (int i = 0; i < funcSig.paramLen; i += 1) {
            SemaTypeId cstrTypeId = funcSig.paramPtr[i];
            TypeId argTypeId = getTypeId(args[i]);

            SemaTypeId argSemaTypeId = ((VmType*)vm->types.buf)[argTypeId].typeSymId;
            if (!isTypeSymCompat(argSemaTypeId, cstrTypeId)) {
                // Assumes next inst is callSym/callNativeFuncIC/callFuncIC.
                // const funcId = @as(*const align(1) u16, @ptrCast(pc + 5 + 4)).*;
                // return panicIncompatibleFuncSig(vm, funcId, args, funcSigId);
                RETURN(RES_CODE_UNKNOWN);
            }
        }
        pc += 5;
        NEXT();
    }
    CASE(CallSym): {
        uint8_t startLocal = pc[1];
        uint8_t numArgs = pc[2];
        uint8_t numRet = pc[3];
        uint16_t symId = READ_U16(4);
        PcSp res = zCallSym(vm, pc, stack, symId, startLocal, numArgs, numRet);
        pc = res.pc;
        stack = res.sp;
        NEXT();
    }
    CASE(CallFuncIC): {
        uint8_t startLocal = pc[1];
        uint8_t numLocals = pc[4];
        if (stack + startLocal + numLocals >= vm->stackEndPtr) {
        //     return error.StackOverflow;
            zFatal();
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
        FuncPtr fn = (FuncPtr)READ_U48(5);
        Value res = fn(vm, newStack + 4, numArgs);
        // if (res.isPanic()) {
        //     return error.Panic;
        // }
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
        pc += 11;
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
    CASE(Call0): {
        uint8_t startLocal = pc[1];
        uint8_t numArgs = pc[2];
        pc += 3;

        Value callee = stack[startLocal + numArgs + 4];
        Value retInfo = VALUE_RETINFO(0, false, CALL_INST_LEN);
        PcSpResult res = zCall(vm, pc, stack, callee, startLocal, numArgs, retInfo);
        if (LIKELY(res.code == RES_CODE_SUCCESS)) {
            pc = res.pc;
            stack = res.sp;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(Call1): {
        uint8_t startLocal = pc[1];
        uint8_t numArgs = pc[2];
        pc += 3;

        Value callee = stack[startLocal + numArgs + 4];
        Value retInfo = VALUE_RETINFO(1, false, CALL_INST_LEN);
        PcSpResult res = zCall(vm, pc, stack, callee, startLocal, numArgs, retInfo);
        if (LIKELY(res.code == RES_CODE_SUCCESS)) {
            pc = res.pc;
            stack = res.sp;
            NEXT();
        }
        RETURN(res.code);
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
                WRITE_U16(5, obj->head.typeId);
                pc[7] = offset;
            } else {
                // framePtr[dst] = @call(.never_inline, gvm.getFieldFallback, .{obj, gvm.fieldSyms.buf[symId].name});
                RETURN(RES_CODE_UNKNOWN);
            }
            pc += 8;
            NEXT();
        } else {
            // return vm.getFieldMissingSymbolError();
            RETURN(RES_CODE_UNKNOWN);
        }
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
                // stack[dst] = try @call(.never_inline, gvm.getField, .{ recv, pc[3].arg });
                // pc += 7;
                NEXT();
            }
        } else {
            // return vm.getFieldMissingSymbolError();
            RETURN(RES_CODE_UNKNOWN);
        }
    }
    CASE(FieldRetain): {
        Value recv = stack[pc[1]];
        u8 dst = pc[2];
        u16 symId = READ_U16(3);
        if (VALUE_IS_POINTER(recv)) {
            HeapObject* obj = VALUE_AS_HEAPOBJECT(recv);
            u8 offset = getFieldOffset(vm, obj, symId);
            if (offset != NULL_U8) {
                stack[dst] = objectGetField((Object*)obj, offset);

                pc[0] = CodeFieldRetainIC;
                WRITE_U16(5, obj->head.typeId);
                pc[7] = offset;
            } else {
                stack[dst] = zGetFieldFallback(vm, obj, ((FieldSymbolMap*)vm->fieldSyms.buf)[symId].nameId);
            }
            retain(vm, stack[dst]);
            pc += 8;
            NEXT();
        } else {
            // return vm.getFieldMissingSymbolError();
            RETURN(RES_CODE_UNKNOWN);
        }
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
        if (res.code == RES_CODE_SUCCESS) {
            stack[pc[6]] = res.val;
            pc += 7;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(Closure):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
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
    CASE(Less): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_NUMBER(left) < VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Greater): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_NUMBER(left) > VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(LessEqual): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_NUMBER(left) <= VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(GreaterEqual): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_NUMBER(left) >= VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Mul): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_NUMBER(VALUE_AS_NUMBER(left) * VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Div): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_NUMBER(VALUE_AS_NUMBER(left) / VALUE_AS_NUMBER(right));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Pow): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_NUMBER(pow(VALUE_AS_NUMBER(left), VALUE_AS_NUMBER(right)));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
    }
    CASE(Mod): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            stack[pc[3]] = VALUE_NUMBER(fmod(VALUE_AS_NUMBER(left), VALUE_AS_NUMBER(right)));
            pc += 4;
            NEXT();
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
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
    CASE(Neg):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
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
    CASE(Object):
    CASE(SetField):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
    CASE(SetFieldRelease): {
        Value recv = stack[pc[1]];
        Value val = stack[pc[2]];
        uint8_t symId = pc[3];
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
                // framePtr[dst] = try gvm.getField(recv, pc[3].arg);
                // try @call(.never_inline, gvm.setFieldRelease, .{ recv, pc[3].arg, framePtr[pc[2].arg] });
                // pc += 7;
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
                VmType* type = ((VmType*)vm->types.buf) + rightTypeId;
                uint32_t rightSemaTypeId = type->typeSymId;
                if (!isTypeSymCompat(rightSemaTypeId, fieldSemaTypeId)) {
                    // return panicIncompatibleFieldType(vm, fieldSemaTypeId, val);
                    RETURN(RES_CODE_UNKNOWN);
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
    CASE(PushTry):
    CASE(PopTry):
    CASE(Throw):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
    CASE(Coinit): {
        uint8_t startArgsLocal = pc[1];
        uint8_t numArgs = pc[2];
        uint8_t jump = pc[3];
        uint8_t initialStackSize = pc[4];
        uint8_t dst = pc[5];

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
    CASE(Coreturn):
        pc += 1;
        if (vm->curFiber != &vm->mainFiber) {
            PcSp res = zPopFiber(vm, NULL_U32, stack, stack[1]);
            pc = res.pc;
            stack = res.sp;
        }
        NEXT();
    CASE(Retain):
        retain(vm, stack[pc[1]]);
        pc += 2;
        NEXT();
    CASE(CopyRetainRelease): {
        uint8_t src = pc[1];
        uint8_t dst = pc[2];
        retain(vm, stack[src]);
        release(vm, stack[dst]);
        stack[dst] = stack[src];
        pc += 3;
        NEXT();
    }
    CASE(Box):
    CASE(SetBoxValue):
    CASE(SetBoxValueRelease):
    CASE(BoxValue):
    CASE(BoxValueRetain):
    CASE(Captured):
    CASE(Tag):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
    CASE(TagLiteral): {
        u8 symId = pc[1];
        stack[pc[2]] = VALUE_SYMBOL(symId)
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
            panicFmt(vm, "Can not cast `%.*s` to `%.*s`.",
                 ((VmType*)vm->types.buf)[getTypeId(val)].nameLen,
                 ((VmType*)vm->types.buf)[getTypeId(val)].namePtr,
                 ((VmType*)vm->types.buf)[expTypeId].nameLen,
                 ((VmType*)vm->types.buf)[expTypeId].namePtr
            );
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
        ResolvedSym sym = getResolvedSym(vm, expSemaTypeId);
        Str name = getName(vm, sym.key.nameId);
        panicFmt(vm, "Can not cast `%.*s` to `%.*s`.", 
             ((VmType*)vm->types.buf)[getTypeId(val)].nameLen,
             ((VmType*)vm->types.buf)[getTypeId(val)].namePtr,
             name.len,
             name.ptr
        );
        RETURN(RES_CODE_PANIC);
    }
    CASE(BitwiseAnd): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            int32_t res = VALUE_AS_NUMBER_TO_INT(left) & VALUE_AS_NUMBER_TO_INT(right);
            stack[pc[3]] = VALUE_NUMBER((double)res);
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
        pc += 4;
        NEXT();
    }
    CASE(BitwiseOr): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            int32_t res = VALUE_AS_NUMBER_TO_INT(left) | VALUE_AS_NUMBER_TO_INT(right);
            stack[pc[3]] = VALUE_NUMBER((double)res);
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
        pc += 4;
        NEXT();
    }
    CASE(BitwiseXor): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            int32_t res = VALUE_AS_NUMBER_TO_INT(left) ^ VALUE_AS_NUMBER_TO_INT(right);
            stack[pc[3]] = VALUE_NUMBER((double)res);
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
        pc += 4;
        NEXT();
    }
    CASE(BitwiseNot): {
        Value val = stack[pc[1]];
        if (VALUE_IS_NUMBER(val)) {
            int32_t res = ~VALUE_AS_NUMBER_TO_INT(val);
            stack[pc[2]] = VALUE_NUMBER((double)res);
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
        pc += 3;
        NEXT();
    }
    CASE(BitwiseLeftShift): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            int32_t res = VALUE_AS_NUMBER_TO_INT(left) << VALUE_AS_NUMBER_TO_INT(right);
            stack[pc[3]] = VALUE_NUMBER((double)res);
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
        pc += 4;
        NEXT();
    }
    CASE(BitwiseRightShift): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        if (VALUE_BOTH_NUMBERS(left, right)) {
            int32_t res = VALUE_AS_NUMBER_TO_INT(left) >> VALUE_AS_NUMBER_TO_INT(right);
            stack[pc[3]] = VALUE_NUMBER((double)res);
        } else {
            panicExpectedNumber(vm);
            RETURN(RES_CODE_PANIC);
        }
        pc += 4;
        NEXT();
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
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) + VALUE_AS_INTEGER(right));
        pc += 4;
        NEXT();
    }
    CASE(SubInt): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        stack[pc[3]] = VALUE_INTEGER(VALUE_AS_INTEGER(left) - VALUE_AS_INTEGER(right));
        pc += 4;
        NEXT();
    }
    CASE(LessInt): {
        Value left = stack[pc[1]];
        Value right = stack[pc[2]];
        stack[pc[3]] = VALUE_BOOLEAN(VALUE_AS_INTEGER(left) < VALUE_AS_INTEGER(right));
        pc += 4;
        NEXT();
    }
    CASE(ForRangeInit): {
        double start = toF64(stack[pc[1]]);
        double end = toF64(stack[pc[2]]);
        stack[pc[2]] = VALUE_NUMBER(end);
        double step = toF64(stack[pc[3]]);
        if (step < 0) {
            step = -step;
        }
        stack[pc[3]] = VALUE_NUMBER(step);
        if (start == end) {
            pc += READ_U16(6) + 7;
            NEXT();
        } else {
            stack[pc[4]] = VALUE_NUMBER(start);
            stack[pc[5]] = VALUE_NUMBER(start);
            uint16_t offset = READ_U16(6);
            if (start < end) {
                pc[offset] = CodeForRange;
            } else {
                pc[offset] = CodeForRangeReverse;
            }
            pc += 8;
            NEXT();
        }
    }
    CASE(ForRange): {
        double counter = VALUE_AS_NUMBER(stack[pc[1]]) + VALUE_AS_NUMBER(stack[pc[2]]);
        if (counter < VALUE_AS_NUMBER(stack[pc[3]])) {
            stack[pc[1]] = VALUE_NUMBER(counter);
            stack[pc[4]] = VALUE_NUMBER(counter);
            pc -= READ_U16(5);
        } else {
            pc += 7;
        }
        NEXT();
    }
    CASE(ForRangeReverse):
    CASE(Match):
    CASE(StaticFunc):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
    CASE(StaticVar): {
        uint16_t symId = READ_U16(1);
        Value sym = ((Value*)vm->varSyms.buf)[symId];
        retain(vm, sym);
        stack[pc[3]] = sym;
        pc += 4;
        NEXT();
    }
    CASE(SetStaticVar): {
        uint16_t symId = READ_U16(1);
        Value prev = *(Value*)(vm->varSyms.buf + symId);
        *(Value*)(vm->varSyms.buf + symId) = stack[pc[3]];
        release(vm, prev);
        pc += 4;
        NEXT();
    }
    CASE(SetStaticFunc):
        printf("Unsupported %s\n", zOpCodeName(*pc));
        zFatal();
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
