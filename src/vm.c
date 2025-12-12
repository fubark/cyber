#if defined(_WIN32)
    // Declare dependencies so they are automatically linked when consuming libcyber.
    #pragma comment(lib, "ntdll.lib")
#endif

#include <stdarg.h>
// #include <stdio.h>
#include "vm.h"

// NOTE: Don't depend on libc that is not available in wasm-freestanding.

#define TRACE_DUMP_OPS 0
#define TRACE_DUMP_NOP 0

__thread ZThread* cur_thread = NULL;

// Dump assembly.
// zig cc -c src/vm.c -S -O2 -DDEBUG=0 -DCGOTO=1 -DTRACE=0 -DIS_32BIT=0 -DHAS_CYC=1

#define STATIC_ASSERT(cond, msg) typedef char static_assertion_##msg[(cond)?1:-1]

#if TRACE
    #define TRACEV_IF(cond, ...) \
        if (clVerbose && cond) {\
        __VA_ARGS__; \
        }

    #define TRACEV(...) \
        if (clVerbose) {\
        __VA_ARGS__; \
        }
#else
    #define TRACEV_IF(cond, ...)
    #define TRACEV(...)
#endif

#define ASSERT(cond) \
    do { if (!(cond)) zFatal(); } while (false)

#define memcpy(dst, src, n) __builtin_memcpy(dst, src, n)
#define memset(dst, x, n) __builtin_memset(dst, x, n)

static size_t strlen(const char* s) {
    size_t len = 0;
    while (s[len] != '\0') {
        len += 1;
    }
    return len;
}

static inline size_t write_str_len(Bytes buf, const char* s, size_t len) {
    if (len > buf.len) {
        zFatal();
    }
    memcpy(buf.ptr, s, len);
    return len;
}

static size_t write_str(Bytes buf, const char* s) {
    size_t len = strlen(s);
    return write_str_len(buf, s, len);
}

#define BYTES(x) ((Bytes){ x, strlen(x) })

// static inline FmtValue FMT_U32(u32 v) {
//     return (FmtValue){ .type = FMT_TYPE_U32, .data = { .u32 = v }};
// }

static inline TypeId getRefeeType(Value val);

static inline Value objectGetField(Object* obj, uint8_t idx) {
    return ((Value*)obj)[idx];
}

static inline Value* objectGetFieldPtr(Object* obj, uint8_t idx) {
    return ((Value*)obj) + idx;
}

static inline Value* objectGetValuesPtr(Object* obj) {
    return (Value*)obj;
}

static inline Value* closureGetCapturedValuesPtr(FuncUnion* closure) {
    return &closure->data.closure.firstCapturedVal;
}

static inline Bytes buf_rem(Bytes buf, size_t len) {
    return (Bytes){buf.ptr+len, buf.len-len};
}

static inline bool releaseOnly(ZThread* t, HeapObject* obj) {
    ObjectHeader* head = (ObjectHeader*)((intptr_t)obj - 8);

#if TRACE
    char buf_arr[128];
    Bytes buf = (Bytes){buf_arr, sizeof(buf_arr)};

    if (zCheckDoubleFree(t, obj)) {
        size_t len = 0;
        len += z_write_ptr(buf_rem(buf, len), obj);
        len += write_str(buf_rem(buf, len), " at t");
        len += z_write_u64(buf_rem(buf, len), t->c.id);
        len += write_str(buf_rem(buf, len), "@");
        len += z_write_ptr(buf_rem(buf, len), (Inst*)t->heap.c.ctx);
        len += write_str(buf_rem(buf, len), "(code=");
        len += z_write_u64(buf_rem(buf, len), *(Inst*)t->heap.c.ctx);
        len += write_str(buf_rem(buf, len), ")");
        Bytes title = BYTES("double free");       
        Bytes msg = (Bytes){buf_arr, len};
        zPrintTraceAtPc(t, t->heap.c.ctx, title, msg);
        zDumpObjectTrace(t, obj);
        zFatal();
    }
    TRACEV_IF(LOG_MEM,
        Bytes name = zGetTypeName(t->c.vm, OBJ_TYPEID(obj));
        size_t len = 0;
        len += write_str(buf_rem(buf, len), "t");
        len += z_write_u64(buf_rem(buf, len), t->c.id);
        len += write_str(buf_rem(buf, len), ": vmc | ");
        len += z_write_u64(buf_rem(buf, len), head->rc);
        len += write_str(buf_rem(buf, len), " -1 | ");
        len += write_str_len(buf_rem(buf, len), name.ptr, name.len);
        len += write_str(buf_rem(buf, len), ", ");
        len += z_write_ptr(buf_rem(buf, len), obj);
        z_log(t, buf_arr, len);
    )
#endif
    head->rc -= 1;
#if TRACE
    if (t->heap.c.refCounts == 0) {
        size_t len = 0;
        len += write_str(buf_rem(buf, len), "Double free. ");
        len += z_write_u64(buf_rem(buf, len), OBJ_TYPEID(head));
        z_log(t, buf_arr, len);
        zFatal();
    }
    t->heap.c.refCounts -= 1;
    t->heap.c.numReleases += 1;
#endif
    return head->rc == 0;
}

static inline void release(ZThread* t, HeapObject* obj) {
    if (releaseOnly(t, obj)) {
        zDestroyObject(t, obj);
    }
}

static inline void freeObject(ZThread* t, HeapObject* obj, size_t size) {
    TRACEV_IF(LOG_MEM,
        Bytes name = zGetTypeName(t->c.vm, OBJ_TYPEID(obj));

        char buf_arr[128];
        Bytes buf = (Bytes){buf_arr, sizeof(buf_arr)};
        size_t len = 0;
        len += write_str(buf_rem(buf, len), "free ");
        len += write_str_len(buf_rem(buf, len), name.ptr, name.len);
        len += write_str(buf_rem(buf, len), " ");
        len += z_write_ptr(buf_rem(buf, len), obj);
        z_log(t, buf_arr, len)
    )
    if (size <= 8 * 4) {
        zFreePoolObject(t, obj);
    } else {
        zFreeBigObject(t, obj, size);
    }
}

// For debugging.
void z_dump_stack_trace2() {
    z_dump_stack_trace(cur_thread);
}

static inline void retain(ZThread* t, HeapObject* obj) {
    ObjectHeader* head = (ObjectHeader*)((intptr_t)obj - 8); 
#if TRACE
    char buf_arr[128];
    if (zCheckRetainDanglingPointer(t, obj)) {
        Bytes buf = (Bytes){buf_arr, sizeof(buf_arr)};
        size_t len = 0;
        len += z_write_ptr(buf_rem(buf, len), obj);
        len += write_str(buf_rem(buf, len), " at t");
        len += z_write_u64(buf_rem(buf, len), t->c.id);
        len += write_str(buf_rem(buf, len), "@");
        len += z_write_ptr(buf_rem(buf, len), (Inst*)t->heap.c.ctx);
        len += write_str(buf_rem(buf, len), "(code=");
        len += z_write_u64(buf_rem(buf, len), *(Inst*)t->heap.c.ctx);
        len += write_str(buf_rem(buf, len), ")");
        Bytes title = BYTES("retain dangling ptr");
        Bytes msg = (Bytes){buf_arr, len};
        zPrintTraceAtPc(t, t->heap.c.ctx, title, msg);
        zDumpObjectTrace(t, obj);
        zFatal();
    }
    if (clVerbose && LOG_MEM) {
        Bytes buf = (Bytes){buf_arr, sizeof(buf_arr)};

        size_t len = 0;
        len += write_str(buf_rem(buf, len), "t");
        len += z_write_u64(buf_rem(buf, len), t->c.id);
        len += write_str(buf_rem(buf, len), ": vmc | ");
        len += z_write_u64(buf_rem(buf, len), head->rc);
        len += write_str(buf_rem(buf, len), " +1 | ");
        Bytes name = zGetTypeName(t->c.vm, OBJ_TYPEID(obj));
        len += write_str_len(buf_rem(buf, len), name.ptr, name.len);
        len += write_str(buf_rem(buf, len), ", ");
        len += z_write_ptr(buf_rem(buf, len), obj);
        z_log(t, buf_arr, len);
    }
#endif
    head->rc += 1;
#if TRACE
    t->heap.c.refCounts += 1;
    t->heap.c.numRetains += 1;
#endif
}

static inline TypeId getRefeeType(Value val) {
    return OBJ_TYPEID(VALUE_AS_HEAPOBJECT(val));
}

static inline uint32_t stackOffset(ZThread* t, Value* stack) {
    return ((uintptr_t)stack - (uintptr_t)t->c.stack_ptr) >> 3;
}

static inline bool isDynTypeCompat(u32 act_t, u32 cstr_rt) {
    bool cstr_ref = (cstr_rt & 0x80000000) != 0;
    u32 cstr_t = cstr_rt & 0x7fffffff;
    if (act_t == cstr_t) {
        return true;
    }
    if (!cstr_ref) {
        if (cstr_t == TYPE_OBJECT) {
            return true;
        }
    }
    return false;
}

static inline bool isTypeCompat(TypeId typeId, TypeId cstrType) {
    if (typeId == cstrType) {
        return true;
    }
    if (cstrType == TYPE_OBJECT) {
        return true;
    }
    return false;
}

static inline HeapObjectResult allocObjectInit(ZThread* t, TypeId typeId, u16 size) {
    // First slot holds the typeId and rc.
    HeapObjectResult res;
    if (size <= 4 * 8) {
        res = zAllocPoolObject(t, typeId);
    } else {
        res = zAllocBigObject(t, typeId, size);
    }
    if (UNLIKELY(res.code != RES_SUCCESS)) {
        return res;
    }
    return res;
}

static inline String static_str(ZThread* t, u8* ptr, u32 len, bool ascii) {
    return (String){
        .buf = NULL,
        .ptr = ptr,
        .len = len | (ascii?(u64)1 << 63:0),
    };
}

// Exponentiation by squaring.
static i64 ipow(i64 b, i64 e) {
    if (e < 0) {
        if (b == 1 && e == -1) {
            return 1;
        }
        if (b == -1 && e == -1) {
            return  -1;
        }
        return 0;
    }
    i64 result = 1;
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

static void panicStaticMsg(ZThread* t, const char* msg) {
    t->c.panic_payload = (u64)msg | (((u64)(strlen(msg))) << 48);
    t->c.panic_type = PANIC_STATIC_MSG;
    TRACEV(
        size_t len = strlen(msg);
        z_log(t, msg, len);
    );
}

static inline void panicDivisionByZero(ZThread* t) {
    panicStaticMsg(t, "Division by zero.");
}

static inline void panicOutOfBounds(ZThread* t) {
    panicStaticMsg(t, "Out of bounds.");
}

static inline void panicExpectedInteger(ZThread* t) {
    panicStaticMsg(t, "Expected integer operand.");
}

static Value buildGenCallInfo(bool cont, u8 call_inst_off, u8 stack_size) {
    return ((u8)cont) | (call_inst_off << 1) | (stack_size << 8) | (3 << 16);
}

#define RETURN(code) \
    do { \
        t->c.pc = pc; \
        t->c.fp = stack; \
        return code; \
    } while (false)

#define SAVE_STATE(code) \
    do { \
        vm->c.curPc = pc; \
        vm->c.curStack = stack; \
    } while (false)

#define RESTORE_STATE(code) \
    do { \
        pc = vm->c.curPc; \
        stack = vm->c.curStack; \
    } while (false)

#define INTEGER_UNOP(...) \
    u16 dst = READ_U16(1); \
    i64 val = BITCAST(i64, stack[READ_U16(3)]); \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 5; \
    NEXT(); \

#define INTEGER_BINOP(...) \
    u16 dst = READ_U16(1); \
    i64 left = BITCAST(i64, stack[READ_U16(3)]); \
    i64 right = BITCAST(i64, stack[READ_U16(5)]); \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 7; \
    NEXT();

#define UINT_BINOP(...) \
    u16 dst = READ_U16(1); \
    u64 left = stack[READ_U16(3)]; \
    u64 right = stack[READ_U16(5)]; \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 7; \
    NEXT();

#define FLOAT_UNOP(...) \
    u16 dst = READ_U16(1); \
    f64 val = VALUE_AS_FLOAT(stack[READ_U16(3)]); \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 5; \
    NEXT();

#define FLOAT_BINOP(...) \
    u16 dst = READ_U16(1); \
    f64 left = VALUE_AS_FLOAT(stack[READ_U16(3)]); \
    f64 right = VALUE_AS_FLOAT(stack[READ_U16(5)]); \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 7; \
    NEXT();

#define F32_UNOP(...) \
    u16 dst = READ_U16(1); \
    f32 val = *(f32*)&stack[READ_U16(3)]; \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 5; \
    NEXT();

#define F32_BINOP(...) \
    u16 dst = READ_U16(1); \
    f32 left = *(f32*)&stack[READ_U16(3)]; \
    f32 right = *(f32*)&stack[READ_U16(5)]; \
    /* Body... */ \
    __VA_ARGS__; \
    pc += 7; \
    NEXT();

ResultCode execBytecode(ZThread* t) {
    #define READ_I16(offset) ((int16_t)(pc[offset] | ((uint16_t)pc[offset + 1] << 8)))
    #define READ_U16(offset) (pc[offset] | ((uint16_t)pc[offset + 1] << 8))
    #define WRITE_U16(offset, u) pc[offset] = u & 0xff; pc[offset+1] = u >> 8
    #define READ_U32(offset) ((uint32_t)pc[offset] | ((uint32_t)pc[offset+1] << 8) | ((uint32_t)pc[offset+2] << 16) | ((uint32_t)pc[offset+3] << 24))
    #define READ_U32_FROM(from, offset) ((uint32_t)from[offset] | ((uint32_t)from[offset+1] << 8) | ((uint32_t)from[offset+2] << 16) | ((uint32_t)from[offset+3] << 24))
    #define READ_U48(offset) ((uint64_t)pc[offset] | ((uint64_t)pc[offset+1] << 8) | ((uint64_t)pc[offset+2] << 16) | ((uint64_t)pc[offset+3] << 24) | ((uint64_t)pc[offset+4] << 32) | ((uint64_t)pc[offset+5] << 40))
    #define READ_U64(offset) ((uint64_t)pc[offset] | ((uint64_t)pc[offset+1] << 8) | ((uint64_t)pc[offset+2] << 16) | ((uint64_t)pc[offset+3] << 24) | ((uint64_t)pc[offset+4] << 32) | ((uint64_t)pc[offset+5] << 40) | ((u64)pc[offset+6] << 48) | ((u64)pc[offset+7] << 56))

#if TRACE
    #define PRE_TRACE() \
        /* Stop at call depth. */ \
        /* if (vm->c.trace_indent > 15) zFatal(); */ \
        t->c.trace->opCounts[pc[0]].count += 1; \
        t->c.trace->totalOpCounts += 1; \
        t->heap.c.ctx = (u64)pc; \
        /* Persist context before each inst for segfault/internal panic reporting */ \
        t->c.trace_pc = pc; \
        t->c.trace_fp = stack; \
        if (clVerbose && TRACE_DUMP_OPS) { \
            z_dump_thread_inst(t, pc); \
        }
    #define DUMP_OP() \
        if (clVerbose) { \
            if (!TRACE_DUMP_OPS) { \
                z_dump_thread_inst(t, pc); \
            } \
        }
#else
    #define PRE_TRACE()
    #define DUMP_OP()
#endif
#if CGOTO
    #define JENTRY(op) &&Code_##op
    static void* jumpTable[] = {
        JENTRY(CONST_64),
        JENTRY(CONST_STR),
        JENTRY(CONST_8S),
        JENTRY(CONST_8),
        JENTRY(CONST_16),
        JENTRY(CONST_32),
        JENTRY(True),
        JENTRY(False),
        JENTRY(LNOT),
        JENTRY(IsZero),
        JENTRY(MOV),
        JENTRY(MOV_2),
        JENTRY(MOV_3),
        JENTRY(MOV_4),
        JENTRY(MOV_N),
        // JENTRY(SetIndexList),
        // JENTRY(SetIndexMap),
        // JENTRY(IndexList),
        // JENTRY(IndexMap),
        // JENTRY(AppendList),
        // JENTRY(List),
        // JENTRY(Map),
        // JENTRY(SliceList),
        JENTRY(JUMP_F),
        JENTRY(JUMP_T),
        JENTRY(JUMP),
        JENTRY(ReleaseOpt),
        JENTRY(Release),
        JENTRY(DTOR_STR),
        JENTRY(CHK_STK),
        JENTRY(CALL),
        JENTRY(CALL_HOST),
        JENTRY(CALL_TRAIT),
        JENTRY(RET_0),
        JENTRY(RET),
        JENTRY(RET_N),
        JENTRY(CALL_PTR),
        JENTRY(CALL_UNION),
        JENTRY(LOAD_8),
        JENTRY(LOAD_16),
        JENTRY(LOAD_32),
        JENTRY(LOAD_64),
        JENTRY(LOAD_2W),
        JENTRY(LOAD_3W),
        JENTRY(LOAD_4W),
        JENTRY(LOAD_N),
        JENTRY(CLOSURE),
        JENTRY(CMP_8),
        JENTRY(CMP),
        JENTRY(CMP_STR),
        JENTRY(FEQ32),
        JENTRY(FLT32),
        JENTRY(FGT32),
        JENTRY(FLE32),
        JENTRY(FGE32),
        JENTRY(FADD32),
        JENTRY(FSUB32),
        JENTRY(FMUL32),
        JENTRY(FDIV32),
        JENTRY(FMOD32),
        JENTRY(FNEG32),
        JENTRY(FEQ),
        JENTRY(FLT),
        JENTRY(FGT),
        JENTRY(FLE),
        JENTRY(FGE),
        JENTRY(LT),
        JENTRY(GT),
        JENTRY(LE),
        JENTRY(GE),
        JENTRY(UCMP),
        JENTRY(FADD),
        JENTRY(FSUB),
        JENTRY(FMUL),
        JENTRY(FDIV),
        JENTRY(FMOD),
        JENTRY(FNEG),
        JENTRY(NEW),
        JENTRY(Trait),
        JENTRY(ADDR),
        JENTRY(UnwrapAddr),
        JENTRY(UnwrapNZ),
        JENTRY(STORE_8),
        JENTRY(STORE_16),
        JENTRY(STORE_32),
        JENTRY(STORE_64),
        JENTRY(STORE_2W),
        JENTRY(STORE_3W),
        JENTRY(STORE_4W),
        JENTRY(STORE_N),
        JENTRY(MEMSETZ),
        JENTRY(RET_GEN),
        JENTRY(RET_Y),
        JENTRY(GEN_NEXT),
        JENTRY(GEN_END),
        JENTRY(AWAIT),
        JENTRY(RetainNZ),
        JENTRY(Retain),
        JENTRY(Captured),
        JENTRY(Cast),
        JENTRY(CastAbstract),
        JENTRY(AND),
        JENTRY(OR),
        JENTRY(XOR),
        JENTRY(NOT),
        JENTRY(LSL),
        JENTRY(LSR),
        JENTRY(Add),
        JENTRY(AddI16),
        JENTRY(Sub),
        JENTRY(IMUL),
        JENTRY(MUL),
        JENTRY(IDIV),
        JENTRY(DIV),
        JENTRY(Pow),
        JENTRY(IMOD),
        JENTRY(MOD),
        JENTRY(Neg),
        JENTRY(ZEXT),
        JENTRY(SEXT),
        JENTRY(F2I),
        JENTRY(F32_2I),
        JENTRY(I2F),
        JENTRY(I2F32),
        JENTRY(FABS),
        JENTRY(F32ABS),
        JENTRY(FN_VM),
        JENTRY(FN_HOST),
        JENTRY(FN_UNION),
        JENTRY(ExternFunc),
        JENTRY(NOP32),
        JENTRY(NOPS),
        JENTRY(TRAP),
        JENTRY(TRAP_DEBUG),
        JENTRY(END),
    };
    STATIC_ASSERT(sizeof(jumpTable) == (CodeEND + 1) * sizeof(void*), JUMP_TABLE_INCOMPLETE);
    #define CASE(op) Code_##op
    #define NEXT() \
        do { \
            PRE_TRACE(); \
            goto *jumpTable[*pc]; \
        } while (false)
#else
    // case name matches enum.
    #define CASE(op) case Code##op
    #define NEXT() \
        do { \
            PRE_TRACE(); \
            goto beginSwitch \
        } while (false)
#endif

    register Inst* pc; 
    register Value* stack;

    pc = t->c.pc;
    stack = t->c.fp;

#if CGOTO
    // Goto first instruction.
    NEXT();
#else 
beginSwitch:
    switch ((OpCode)*pc) {
#endif
    CASE(CONST_64): {
        u16 dst = READ_U16(1);
        stack[dst] = READ_U64(3);
        pc += 11;
        NEXT();
    }
    CASE(CONST_STR): {
        u16 dst = READ_U16(1);
        u64 ptr_len = READ_U64(3);
        u8* ptr = (u8*)(ptr_len & 0xffffffffffff);
        u32 len = ptr_len >> 48;
        bool ascii = pc[11];
        String res = static_str(t, ptr, len, ascii);
        *(String*)&stack[dst] = res;
        pc += 12;
        NEXT();
    }
    CASE(CONST_8S): {
        i64 i = (i64)BITCAST(i8, pc[3]);
        stack[READ_U16(1)] = BITCAST(u64, i);
        pc += 4;
        NEXT();
    }
    CASE(CONST_8): {
        u8 val = pc[3];
        stack[READ_U16(1)] = val;
        pc += 4;
        NEXT();
    }
    CASE(CONST_16): {
        u16 val = READ_U16(3);
        *(u16*)(stack + READ_U16(1)) = val;
        pc += 5;
        NEXT();
    }
    CASE(CONST_32): {
        u32 val = READ_U32(3);
        *(u32*)(stack + READ_U16(1)) = val;
        pc += 7;
        NEXT();
    }
    CASE(True): {
        stack[READ_U16(1)] = true;
        pc += 3;
        NEXT();
    }
    CASE(False): {
        stack[READ_U16(1)] = false;
        pc += 3;
        NEXT();
    }
    CASE(LNOT): {
        stack[READ_U16(1)] = !(*(bool*)&stack[READ_U16(3)]);
        pc += 5;
        NEXT();
    }
    CASE(IsZero): {
        u16 dst = READ_U16(1);
        u64 val = stack[READ_U16(3)];
        stack[dst] = val == 0;
        pc += 5;
        NEXT();
    }
    CASE(MOV): {
        stack[READ_U16(1)] = stack[READ_U16(3)];
        pc += 5;
        NEXT();
    }
    CASE(MOV_2): {
        u16 dst = READ_U16(1);
        u16 src = READ_U16(3);
        stack[dst] = stack[src];
        stack[dst+1] = stack[src+1];
        pc += 5;
        NEXT();
    }
    CASE(MOV_3): {
        u16 dst = READ_U16(1);
        u16 src = READ_U16(3);
        stack[dst] = stack[src];
        stack[dst+1] = stack[src+1];
        stack[dst+2] = stack[src+2];
        pc += 5;
        NEXT();
    }
    CASE(MOV_4): {
        u16 dst = READ_U16(1);
        u16 src = READ_U16(3);
        stack[dst] = stack[src];
        stack[dst+1] = stack[src+1];
        stack[dst+2] = stack[src+2];
        stack[dst+3] = stack[src+3];
        pc += 5;
        NEXT();
    }
    CASE(MOV_N): {
        u16 dst = READ_U16(1);
        u16 src = READ_U16(3);
        u16 n = READ_U16(5);
        memcpy(stack + dst, stack + src, 8 * n);
        pc += 7;
        NEXT();
    }
    // CASE(SetIndexList): {
    //     Value listv = stack[pc[1]];
    //     Value index = stack[pc[2]];
    //     Value right = stack[pc[3]];
    //     HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

    //     i64 idx = VALUE_AS_INTEGER(index);
    //     if (idx >= 0 && idx < listo->list.list.len) {
    //         Value existing = ((Value*)listo->list.list.buf)[idx];
    //         release(vm, existing);
    //         retain(vm, right);
    //         ((Value*)listo->list.list.buf)[idx] = right;
    //         stack[pc[4]] = VALUE_VOID;
    //         pc += 5;
    //         NEXT();
    //     } else {
    //         panicOutOfBounds(vm);
    //         RETURN(RES_PANIC);
    //     }
    // }
    // CASE(SetIndexMap): {
    //     Value mapv = stack[pc[1]];
    //     Value index = stack[pc[2]];
    //     Value right = stack[pc[3]];
    //     Map* mapo = (Map*)VALUE_AS_HEAPOBJECT(mapv);
    //     ResultCode code = zMapSet(vm, mapo, index, right);
    //     if (LIKELY(code == RES_SUCCESS)) {
    //         stack[pc[4]] = VALUE_VOID;
    //         pc += 5;
    //         NEXT();
    //     }
    //     RETURN(code);
    // }
    // CASE(IndexList): {
    //     Value listv = stack[pc[1]];
    //     Value index = stack[pc[2]];
    //     HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

    //     i64 idx = VALUE_AS_INTEGER(index);
    //     if (idx >= 0 && idx < listo->list.list.len) {
    //         Value val = ((Value*)listo->list.list.buf)[idx];
    //         retain(vm, val);
    //         stack[pc[3]] = val;
    //         pc += 4;
    //         NEXT();
    //     } else {
    //         panicOutOfBounds(vm);
    //         RETURN(RES_PANIC);
    //     }
    // }
    // CASE(IndexMap): {
    //     Value mapv = stack[pc[1]];
    //     Value index = stack[pc[2]];
    //     HeapObject* mapo = VALUE_AS_HEAPOBJECT(mapv);
    //     bool found;
    //     Value val = zValueMapGet(&mapo->map.inner, index, &found);
    //     if (found) {
    //         retain(vm, val);
    //         stack[pc[3]] = val;
    //     } else {
    //         panicFieldMissing(vm);
    //         RETURN(RES_PANIC);
    //     }
    //     pc += 4;
    //     NEXT();
    // }
    // CASE(AppendList): {
    //     Value listv = stack[pc[1]];
    //     Value itemv = stack[pc[2]];

    //     HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);

    //     size_t len = listo->list.list.len;
    //     if (len == listo->list.list.cap) {
    //         // ensure cap.
    //         ResultCode code = zEnsureListCap(vm, &listo->list.list, len + 1);
    //         if (code != RES_SUCCESS) {
    //             RETURN(code);
    //         }
    //     }

    //     retain(vm, itemv);
    //     ((Value*)listo->list.list.buf)[len] = itemv;
    //     listo->list.list.len = len + 1;
    //     stack[pc[3]] = VALUE_VOID;

    //     pc += 4;
    //     NEXT();
    // }
    // CASE(List): {
    //     u8 startLocal = pc[1];
    //     u8 numElems = pc[2];
    //     u16 type_id = READ_U16(3);
    //     ValueResult res = zAllocList(vm, type_id, stack + startLocal, numElems);
    //     if (UNLIKELY(res.code != RES_SUCCESS)) {
    //         RETURN(res.code);
    //     }
    //     stack[pc[5]] = res.val;
    //     pc += 6;
    //     NEXT();
    // }
    // CASE(SliceList): {
    //     Value listv = stack[pc[1]];
    //     Value rangev = stack[pc[2]];
    //     HeapObject* listo = VALUE_AS_HEAPOBJECT(listv);
    //     HeapObject* rangeo = VALUE_AS_HEAPOBJECT(rangev);

    //     i64 start = rangeo->range.start;
    //     if (start < 0) {
    //         panicOutOfBounds(vm);
    //         RETURN(RES_PANIC);
    //     }

    //     i64 end = rangeo->range.end;
    //     if (end > listo->list.list.len) {
    //         panicOutOfBounds(vm);
    //         RETURN(RES_PANIC);
    //     }

    //     if (end < start) {
    //         panicOutOfBounds(vm);
    //         RETURN(RES_PANIC);
    //     }

    //     Value* elems = ((Value*)listo->list.list.buf);
    //     for (int i = start; i < end; i += 1) {
    //         retain(vm, elems[i]);
    //     }
    //     ValueResult res = zAllocList(vm, OBJ_TYPEID(listo), elems + start, end - start);
    //     if (UNLIKELY(res.code != RES_SUCCESS)) {
    //         RETURN(res.code);
    //     }
    //     stack[pc[3]] = res.val;
    //     pc += 4;
    //     NEXT();
    // }
    CASE(JUMP_F): {
        if (*(bool*)&stack[READ_U16(1)]) {
            pc += 5;
        } else {
            pc += READ_U16(3);
        }
        NEXT();
    }
    CASE(JUMP_T): {
        if (!*(bool*)&stack[READ_U16(1)]) {
            pc += (uintptr_t)READ_I16(3);
        } else {
            pc += 5;
        }
        NEXT();
    }
    CASE(JUMP): {
        pc += READ_I16(1);
        NEXT();
    }
    CASE(ReleaseOpt): {
        if (stack[READ_U16(1)] == 0) {
            pc += (uintptr_t)READ_I16(3);
            NEXT();
        }
        if (!releaseOnly(t, (HeapObject*)stack[READ_U16(1)])) {
            pc += (uintptr_t)READ_I16(3);
            NEXT();
        }
        pc += 5;
        NEXT();
    }
    CASE(Release): {
        if (!releaseOnly(t, (HeapObject*)(size_t)stack[READ_U16(1)])) {
            pc += (uintptr_t)READ_I16(3);
            NEXT();
        }
        pc += 5;
        NEXT();
    }
    CASE(DTOR_STR): {
        String* str = (String*)stack[READ_U16(1)];
        if (str->buf != 0) {
            Buffer* buf = (Buffer*)str->buf;
            release(t, (HeapObject*)buf);
        }
        pc += 3;
        NEXT();
    }
    CASE(CHK_STK): {
        u16 ret_size = READ_U16(1);
        u16 frame_size = READ_U16(3);
        // Update fp before stack check so it can be reliably at ret.
        stack -= ret_size;

        // NOTE: Don't zero the ret memory since the parameters may depend on it.
        // e.g. a = foo(&a)
        
        if (stack + frame_size >= t->c.stack_end) {
            RETURN(RES_STACK_OVERFLOW);
        }
        pc += 5;
        NEXT();
    }
    CASE(CALL): {
        DUMP_OP()
        u16 base = READ_U16(1);
        uintptr_t ret_fp = (uintptr_t)stack;
        stack += base;
        stack[0] = VALUE_CALLINFO(false, CALL_INST_LEN, 0);
        stack[1] = (uintptr_t)(pc + CALL_INST_LEN);
        stack[2] = ret_fp;
        pc = (Inst*)READ_U48(3);
        NEXT();
    }
    CASE(CALL_HOST): {
        DUMP_OP()
        u16 base = READ_U16(1);

        t->c.pc = NULL;
        t->c.fp = stack + base;
#if TRACE
        t->c.trace_pc = NULL;
        t->c.trace_fp = stack + base;
#endif

        stack[base] = VALUE_HOSTCALLINFO(false);
        stack[base+1] = (Value)pc;
        stack[base+2] = (Value)stack;

        HostFn fn = (HostFn)READ_U48(3);
        Ret res = fn(t);
        if (res != 0) {
            RETURN(RES_PANIC);
        }
        pc += CALL_INST_LEN;
        NEXT();
    }
    CASE(CALL_TRAIT): {
        DUMP_OP()
        u16 base = READ_U16(1);
        u16 vtable_idx = READ_U16(3);
        PcFpResult res = zCallTrait(t, pc, stack, vtable_idx, base);
        if (res.code != RES_SUCCESS) {
            RETURN(res.code);
        }
        pc = res.pc;
        stack = res.fp;
        NEXT();
    }
    CASE(RET_0): {
        DUMP_OP()
        pc = (Inst*)stack[1];
        stack = (Value*)stack[2];
        NEXT();
    }
    CASE(RET): {
        DUMP_OP()
        pc = (Inst*)stack[2];
        stack = (Value*)stack[3];
        NEXT();
    }
    CASE(RET_N): {
        DUMP_OP()
        u16 ret_size = READ_U16(1);
        pc = (Inst*)stack[ret_size+1];
        stack = (Value*)stack[ret_size+2];
        NEXT();
    }
    CASE(CALL_PTR): {
        DUMP_OP()
        u16 base = READ_U16(1);

        PcFpResult res = zCallPtr(t, pc, stack, base);
        if (LIKELY(res.code == RES_SUCCESS)) {
            pc = res.pc;
            stack = res.fp;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(CALL_UNION): {
        DUMP_OP()
        u16 base = READ_U16(1);

        PcFpResult res = zCallUnion(t, pc, stack, base);
        if (LIKELY(res.code == RES_SUCCESS)) {
            pc = res.pc;
            stack = res.fp;
            NEXT();
        }
        RETURN(res.code);
    }
    CASE(LOAD_8): {
        u16 dst = READ_U16(1);
        u8* ptr = (u8*)stack[READ_U16(3)];
        u16 offset = READ_U16(5);
        stack[dst] = *(ptr + offset);
        pc += 7;
        NEXT();
    }
    CASE(LOAD_16): {
        u16 dst = READ_U16(1);
        u8* ptr = (u8*)stack[READ_U16(3)];
        u16 offset = READ_U16(5);
        stack[dst] = *(u16*)(ptr + offset);
        pc += 7;
        NEXT();
    }
    CASE(LOAD_32): {
        u16 dst = READ_U16(1);
        u8* ptr = (u8*)stack[READ_U16(3)];
        u16 offset = READ_U16(5);
        stack[dst] = *(u32*)(ptr + offset);
        pc += 7;
        NEXT();
    }
    CASE(LOAD_64): {
        u16 dst = READ_U16(1);
        u8* ptr = (u8*)stack[READ_U16(3)];
        u16 offset = READ_U16(5);
        stack[dst] = *(Value*)(ptr + offset);
        pc += 7;
        NEXT();
    }
    CASE(LOAD_2W): {
        u16 dst = READ_U16(1);
        u8* ptr = ((u8*)stack[READ_U16(3)]) + READ_U16(5);
        Value* src = (Value*)ptr;
        stack[dst] = src[0];
        stack[dst+1] = src[1];
        pc += 7;
        NEXT();
    }
    CASE(LOAD_3W): {
        u16 dst = READ_U16(1);
        u8* ptr = ((u8*)stack[READ_U16(3)]) + READ_U16(5);
        Value* src = (Value*)ptr;
        stack[dst] = src[0];
        stack[dst+1] = src[1];
        stack[dst+2] = src[2];
        pc += 7;
        NEXT();
    }
    CASE(LOAD_4W): {
        u16 dst = READ_U16(1);
        u8* ptr = ((u8*)stack[READ_U16(3)]) + READ_U16(5);
        Value* src = (Value*)ptr;
        stack[dst] = src[0];
        stack[dst+1] = src[1];
        stack[dst+2] = src[2];
        stack[dst+3] = src[3];
        pc += 7;
        NEXT();
    }
    CASE(LOAD_N): {
        u8* dst = (u8*)&stack[READ_U16(1)];
        u8* ptr = (u8*)stack[READ_U16(3)];
        u16 offset = READ_U16(5);
        u16 size = READ_U16(7);
        memcpy(dst, ptr + offset, size);
        pc += 9;
        NEXT();
    }
    CASE(CLOSURE): {
        u16 dst = READ_U16(1);
        u16 union_t = READ_U16(3);
        Inst* func_pc = (Inst*)READ_U48(5);
        u8 numCaptured = pc[11];
        bool pinned_closure = pc[12];
        Inst* captured_regs = pc + 13;
        ValueResult res = zAllocClosure(t, stack, func_pc, union_t, captured_regs, numCaptured, pinned_closure);
        if (res.code != RES_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = res.val;
        pc += 13 + numCaptured * 2;
        NEXT();
    }
    CASE(CMP_8): {
        u16 dst = READ_U16(1);
        Value left = stack[READ_U16(3)];
        Value right = stack[READ_U16(5)];
        stack[dst] = ((u8)left == (u8)right);
        pc += 7;
        NEXT();
    }
    CASE(CMP): {
        u16 dst = READ_U16(1);
        Value left = stack[READ_U16(3)];
        Value right = stack[READ_U16(5)];
        stack[dst] = (left == right);
        pc += 7;
        NEXT();
    }
    CASE(CMP_STR): {
        u16 dst = READ_U16(1);
        Value a = stack[READ_U16(3)];
        Value b = stack[READ_U16(5)];
        u64 len = VALUE_GET_STRLEN(a);
        if (len != VALUE_GET_STRLEN(b)) {
            stack[dst] = false;
            pc += 7;
            NEXT();
        }

        const char* a_ptr = VALUE_GET_STRPTR(a);
        const char* b_ptr = VALUE_GET_STRPTR(b);
        while (len > 0) {
            if (*a_ptr != *b_ptr) {
                stack[dst] = 0;
                pc += 7;
                NEXT();
            }
            a_ptr += 1;
            b_ptr += 1;
            len -= 1;
        }
        stack[dst] = 1;
        pc += 7;
        NEXT();
    }
    CASE(FEQ32): {
        F32_BINOP(stack[dst] = left == right)
    }
    CASE(FLT32): {
        F32_BINOP(stack[dst] = left < right)
    }
    CASE(FGT32): {
        F32_BINOP(stack[dst] = left > right)
    }
    CASE(FLE32): {
        F32_BINOP(stack[dst] = left <= right)
    }
    CASE(FGE32): {
        F32_BINOP(stack[dst] = left >= right)
    }
    CASE(FADD32): {
        F32_BINOP(stack[dst] = VALUE_F32(left + right))
    }
    CASE(FSUB32): {
        F32_BINOP(stack[dst] = VALUE_F32(left - right))
    }
    CASE(FMUL32): {
        F32_BINOP(stack[dst] = VALUE_F32(left * right))
    }
    CASE(FDIV32): {
        F32_BINOP(stack[dst] = VALUE_F32(left / right))
    }
    CASE(FMOD32): {
        F32_BINOP(stack[dst] = VALUE_F32(__builtin_fmodf(left, right)))
    }
    CASE(FNEG32): {
        F32_UNOP(stack[dst] = VALUE_F32(-val))
    }
    CASE(FEQ): {
        FLOAT_BINOP(stack[dst] = left == right)
    }
    CASE(FLT): {
        FLOAT_BINOP(stack[dst] = left < right)
    }
    CASE(FGT): {
        FLOAT_BINOP(stack[dst] = left > right)
    }
    CASE(FLE): {
        FLOAT_BINOP(stack[dst] = left <= right)
    }
    CASE(FGE): {
        FLOAT_BINOP(stack[dst] = left >= right)
    }
    CASE(LT): {
        INTEGER_BINOP(stack[dst] = left < right)
    }
    CASE(GT): {
        INTEGER_BINOP(stack[dst] = left > right)
    }
    CASE(LE): {
        INTEGER_BINOP(stack[dst] = left <= right)
    }
    CASE(GE): {
        INTEGER_BINOP(stack[dst] = left >= right)
    }
    CASE(UCMP): {
        u16 dst = READ_U16(1);
        u16 left = READ_U16(3);
        u16 right = READ_U16(5);
        u64 sign_xor = (stack[READ_U16(3)] >> 63) ^ (stack[READ_U16(5)] >> 63);
        if (sign_xor != 0) {
            stack[dst] = !(stack[dst]);
        }
        pc += 7;
        NEXT();
    }
    CASE(FADD): {
        FLOAT_BINOP(stack[dst] = VALUE_FLOAT(left + right))
    }
    CASE(FSUB): {
        FLOAT_BINOP(stack[dst] = VALUE_FLOAT(left - right))
    }
    CASE(FMUL): {
        FLOAT_BINOP(stack[dst] = VALUE_FLOAT(left * right))
    }
    CASE(FDIV): {
        FLOAT_BINOP(stack[dst] = VALUE_FLOAT(left / right))
    }
    CASE(FMOD): {
        FLOAT_BINOP(stack[dst] = VALUE_FLOAT(__builtin_fmod(left, right)))
    }
    CASE(FNEG): {
        FLOAT_UNOP(stack[dst] = VALUE_FLOAT(-val))
    }
    CASE(NEW): {
        u16 dst = READ_U16(1);
        u16 val_t = READ_U16(3);
        u16 size = READ_U16(5);
        HeapObjectResult res = allocObjectInit(t, val_t, size);
        if (res.code != RES_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = (Value)res.obj;
        pc += 7;
        NEXT();
    }
    CASE(Trait): {
        u16 dst = READ_U16(1);
        Value impl = stack[READ_U16(3)];
        u16 type_id = READ_U16(5);
        u16 vtable = READ_U16(7);
        stack[dst] = vtable;
        stack[dst+1] = impl;
        pc += 9;
        NEXT();
    }
    CASE(ADDR): {
        u16 dst = READ_U16(1);
        HeapObject* obj = (HeapObject*)(&stack[READ_U16(3)]);
        stack[dst] = (Value)obj;
        pc += 5;
        NEXT();
    }
    CASE(UnwrapAddr): {
        u16 dst = READ_U16(1);
        Value* ptr = (Value*)stack[READ_U16(3)];
        if (VALUE_AS_INTEGER(ptr[0]) == (i64)pc[5]) {
            stack[dst] = ((u64)ptr) + pc[6];
            pc += 7;
            NEXT();
        } else {
            z_panic_unexpected_choice(t, (i64)pc[5], VALUE_AS_INTEGER(ptr[0]));
            RETURN(RES_PANIC);
        }
    }
    CASE(UnwrapNZ): {
        u16 dst = READ_U16(1);
        Value val = stack[READ_U16(3)];
        if (val != 0) {
            stack[dst] = val;
            pc += 5;
            NEXT();
        } else {
            z_panic_unexpected_choice(t, 1, 0);
            RETURN(RES_PANIC);
        }
    }
    CASE(STORE_8): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        *(dst + offset) = *(u8*)&stack[READ_U16(5)];
        pc += 7;
        NEXT();
    }
    CASE(STORE_16): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        *(u16*)(dst + offset) = *(u16*)&stack[READ_U16(5)];
        pc += 7;
        NEXT();
    }
    CASE(STORE_32): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        *(u32*)(dst + offset) = *(u32*)&stack[READ_U16(5)];
        pc += 7;
        NEXT();
    }
    CASE(STORE_64): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        *(Value*)(dst + offset) = stack[READ_U16(5)];
        pc += 7;
        NEXT();
    }
    CASE(STORE_2W): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        u16 src = READ_U16(5);
        *(Value*)(dst + offset) = stack[src];
        *(Value*)(dst + offset + 8) = stack[src + 1];
        pc += 7;
        NEXT();
    }
    CASE(STORE_3W): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        u16 src = READ_U16(5);
        *(Value*)(dst + offset) = stack[src];
        *(Value*)(dst + offset + 8) = stack[src + 1];
        *(Value*)(dst + offset + 16) = stack[src + 2];
        pc += 7;
        NEXT();
    }
    CASE(STORE_4W): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        u16 src = READ_U16(5);
        *(Value*)(dst + offset) = stack[src];
        *(Value*)(dst + offset + 8) = stack[src + 1];
        *(Value*)(dst + offset + 16) = stack[src + 2];
        *(Value*)(dst + offset + 24) = stack[src + 3];
        pc += 7;
        NEXT();
    }
    CASE(STORE_N): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        u8* src = (u8*)&stack[READ_U16(5)];
        u16 size = READ_U16(7);
        memcpy(dst + offset, src, size);
        pc += 9;
        NEXT();
    }
    CASE(MEMSETZ): {
        u8* dst = (u8*)stack[READ_U16(1)];
        u16 offset = READ_U16(3);
        u16 size = READ_U16(5);
        memset(dst + offset, 0, size);
        pc += 7;
        NEXT();
    }
    CASE(RET_GEN): {
        u16 type_id = READ_U16(1);
        u16 ret_size = READ_U16(3);
        u16 reg_end = READ_U16(5);
        u8 resume_off = pc[7];
        Inst* deinit_pc = pc + 8;
        Inst* resume_pc = pc + resume_off;
        if (resume_pc == deinit_pc) {
            deinit_pc = NULL;
        }
        ResultCode res = z_ret_generator(t, type_id, ret_size, stack, reg_end, resume_pc, deinit_pc);
        if (res != RES_SUCCESS) {
            RETURN(res);
        }
        pc = (Inst*)stack[ret_size + 1];
        stack = (Value*)stack[ret_size + 2];
        NEXT();
    }
    CASE(RET_Y): {
        u16 ret_size = READ_U16(1);
        Generator* gen = (Generator*)stack[ret_size + 2];
        gen->running = false;
        if (pc[4] == 1) {
            gen->done = true;
        } else {
            gen->resume_pc = pc + pc[3];
            if (pc[3] == 5) {
                // If resume pc is the next inst, then there is no deinit pc.
                gen->deinit_pc = NULL;
            } else {
                gen->deinit_pc = pc + 5;
            }
        }
        pc = (Inst*)stack[ret_size + 1];
        stack = gen->prev_fp;
        NEXT();
    }
    CASE(GEN_NEXT): {
        u16 ret = READ_U16(1);
        u16 base = READ_U16(3);
        Generator* gen = (Generator*)stack[READ_U16(5)];
        if (gen->running) {
            panicStaticMsg(t, "Generator is already running.");
            RETURN(RES_PANIC);
        }
        if (gen->done) {
            panicStaticMsg(t, "Generator is already done.");
            RETURN(RES_PANIC);
        }

        Value* base_ptr = stack + base;
        base_ptr[0] = buildGenCallInfo(true, 7, 128);
        base_ptr[1] = (u64)(pc + 7);
        base_ptr[2] = (u64)gen;
        gen->prev_fp = stack;
        gen->running = true;

        memcpy(base_ptr + CALL_ARG_START, gen->frame_ptr, gen->frame_len * sizeof(Value));
        pc = gen->resume_pc;

        // Coroutine expects `fp` to be in the right place.
        stack = stack + ret;
        NEXT();
    }
    CASE(GEN_END): {
        u16 ret = READ_U16(1);
        u16 base = READ_U16(3);
        Generator* gen = (Generator*)stack[READ_U16(5)];
        if (gen->running) {
            panicStaticMsg(t, "Generator is already running.");
            RETURN(RES_PANIC);
        }
        if (gen->done) {
            pc += 7;
            NEXT();
        }

        if (gen->deinit_pc == NULL) {
            gen->done = true;
            pc += 7;
            NEXT();
        }

        Value* base_ptr = stack + base;
        base_ptr[0] = buildGenCallInfo(true, 7, 128);
        base_ptr[1] = (u64)(pc + 7);
        base_ptr[2] = (u64)gen;
        gen->prev_fp = stack;
        gen->running = true;

        memcpy(base_ptr + CALL_ARG_START, gen->frame_ptr, gen->frame_len * sizeof(Value));
        pc = gen->deinit_pc;

        // Coroutine expects `fp` to be in the right place even though the `end` path will never return anything.
        stack = stack + ret;
        NEXT();
    }
    CASE(AWAIT): {
        Value future = stack[READ_U16(1)];
        pc += 3;
        t->c.pc = pc;
        t->c.fp = stack;
        ResultCode code = zAwait(t, future);
        if (code == RES_AWAIT) {
            RETURN(RES_AWAIT);
        }
        NEXT();
    }
    CASE(RetainNZ): {
        size_t addr = stack[pc[1]];
        if (addr != 0) {
            retain(t, (HeapObject*)addr);
        }
        pc += 2;
        NEXT();
    }
    CASE(Retain): {
        retain(t, (HeapObject*)(size_t)stack[READ_U16(1)]);
        pc += 3;
        NEXT();
    }
    CASE(Captured): {
        u16 dst = READ_U16(1);
        HeapObject* closure = (HeapObject*)stack[READ_U16(3)];
        HeapObject* obj = (HeapObject*)closureGetCapturedValuesPtr(&closure->func_union)[pc[5]];
        stack[dst] = (Value)obj;
        pc += 6;
        NEXT();
    }
    CASE(Cast): {
        u16 dst = READ_U16(1);
        Value val = stack[READ_U16(3)];
        u16 expTypeId = READ_U16(5);
        bool exp_ref = pc[7];
        u32 shape_t = getRefeeType(val);
        if (shape_t == expTypeId) {
            stack[dst] = val;
            pc += 8;
            NEXT();
        } else {
            panicStaticMsg(t, "Failed to downcast.");
            RETURN(RES_PANIC);
        }
    }
    CASE(CastAbstract): {
        u16 dst = READ_U16(1);
        Value val = stack[READ_U16(3)];
        u16 expTypeId = READ_U16(5);
        if (expTypeId == TYPE_OBJECT) {
            stack[dst] = val;
            pc += 7;
            NEXT();
        } else {
            panicStaticMsg(t, "Failed to downcast.");
            RETURN(RES_PANIC);
        }
    }
    CASE(AND): {
        INTEGER_BINOP(stack[dst] = BITCAST(u64, left & right))
    }
    CASE(OR): {
        INTEGER_BINOP(stack[dst] = BITCAST(u64, left | right))
    }
    CASE(XOR): {
        INTEGER_BINOP(stack[dst] = BITCAST(u64, left ^ right))
    }
    CASE(NOT): {
        INTEGER_UNOP(stack[dst] = BITCAST(u64, ~val))
    }
    CASE(LSL): {
        UINT_BINOP(
            stack[dst] = left << (right & 0xff);
        )
    }
    CASE(LSR): {
        UINT_BINOP(
            stack[dst] = left >> (right & 0xff);
        )
    }
    CASE(Add): {
        UINT_BINOP(stack[dst] = left + right)
    }
    CASE(AddI16): {
        u16 dst = READ_U16(1);
        u64 left = stack[READ_U16(3)];
        u16 right = READ_U16(5);
        stack[dst] = BITCAST(u64, left) + BITCAST(u64, (i64)BITCAST(i16, right));
        pc += 7;
        NEXT();
    }
    CASE(Sub): {
        UINT_BINOP(stack[dst] = left - right)
    }
    CASE(IMUL): {
        INTEGER_BINOP(stack[dst] = BITCAST(u64, left * right))
    }
    CASE(MUL): {
        UINT_BINOP(stack[dst] = left * right)
    }
    CASE(IDIV): {
        INTEGER_BINOP(
            if (right == 0) {
                panicDivisionByZero(t);
                RETURN(RES_PANIC);
            }
            stack[dst] = BITCAST(u64, left / right);
        )
    }
    CASE(DIV): {
        UINT_BINOP(
            if (right == 0) {
                panicDivisionByZero(t);
                RETURN(RES_PANIC);
            }
            stack[dst] = left / right;
        )
    }
    CASE(Pow): {
        INTEGER_BINOP(stack[dst] = BITCAST(u64, ipow(left, right)))
    }
    CASE(IMOD): {
        INTEGER_BINOP(
            if (right == 0) {
                panicDivisionByZero(t);
                RETURN(RES_PANIC);
            }
            stack[dst] = BITCAST(u64, left % right);
        )
    }
    CASE(MOD): {
        UINT_BINOP(
            if (right == 0) {
                panicDivisionByZero(t);
                RETURN(RES_PANIC);
            }
            stack[dst] = left % right;
        )
    }
    CASE(Neg): {
        INTEGER_UNOP(stack[dst] = BITCAST(u64, -val))
    }
    CASE(ZEXT): {
        // TODO: Inline assembly.
        u16 dst = READ_U16(1);
        u64 val = stack[READ_U16(3)];
        stack[dst] = val & (((u64)1 << pc[5]) - 1);
        pc += 6;
        NEXT();
    }
    CASE(SEXT): {
        // TODO: Inline assembly.
        u16 dst = READ_U16(1);
        u64 val = stack[READ_U16(3)];
        stack[dst] = (i64)(val << (64 - pc[5])) >> (64 - pc[5]);
        pc += 6;
        NEXT();
    }
    CASE(F2I): {
        u16 dst = READ_U16(1);
        f64 val = BITCAST(f64, stack[READ_U16(3)]);
        stack[dst] = BITCAST(u64, (i64)val);
        pc += 5;
        NEXT();
    }
    CASE(F32_2I): {
        u16 dst = READ_U16(1);
        f32 val = *(f32*)&stack[READ_U16(3)];
        stack[dst] = BITCAST(u64, (i64)val);
        pc += 5;
        NEXT();
    }
    CASE(I2F): {
        u16 dst = READ_U16(1);
        i64 val = BITCAST(i64, stack[READ_U16(3)]);
        stack[dst] = BITCAST(u64, (f64)val);
        pc += 5;
        NEXT();
    }
    CASE(I2F32): {
        u16 dst = READ_U16(1);
        i64 val = BITCAST(i64, stack[READ_U16(3)]);
        stack[dst] = BITCAST(u32, (f32)val);
        pc += 5;
        NEXT();
    }
    CASE(FABS): {
        u16 dst = READ_U16(1);
        f64 val = BITCAST(f64, stack[READ_U16(3)]);
        stack[dst] = BITCAST(u64, __builtin_fabs(val));
        pc += 5;
        NEXT();
    }
    CASE(F32ABS): {
        u16 dst = READ_U16(1);
        f32 val = *(f32*)&stack[READ_U16(3)];
        stack[dst] = BITCAST(u32, __builtin_fabsf(val));
        pc += 5;
        NEXT();
    }
    CASE(FN_VM): {
        stack[READ_U16(1)] = READ_U48(3);
        pc += 9;
        NEXT();
    }
    CASE(FN_HOST): {
        u64 addr = READ_U48(3);
        stack[READ_U16(1)] = addr | (1<<63);
        pc += 9;
        NEXT();
    }
    CASE(FN_UNION): {
        u16 dst = READ_U16(1);
        Value val = stack[READ_U16(3)];
        u16 union_t = READ_U16(5);

        ValueResult res = zAllocFuncUnion(t, (TypeId)union_t, val);
        if (res.code != RES_SUCCESS) {
            RETURN(res.code);
        }
        stack[dst] = res.val;
        pc += 7;
        NEXT();
    }
    CASE(ExternFunc): {
        u16 dst = READ_U16(1);
        u16 func_id = READ_U16(3);
        void* ptr = zGetExternFunc(t->c.vm, func_id);
        stack[dst] = (Value)ptr;
        pc += 5;
        NEXT();
    }
    CASE(NOP32): {
        pc += 5;
        NEXT();
    }
    CASE(NOPS): {
#if TRACE_DUMP_NOP
        DUMP_OP()
#endif
        pc += 9;
        NEXT();
    }
    CASE(TRAP): {
        DUMP_OP()
        panicStaticMsg(t, "Reached trap.");
        RETURN(RES_PANIC);
    }
    CASE(TRAP_DEBUG): {
        DUMP_OP()
#if TRACE
        __builtin_debugtrap();
#endif
        pc += 1;
        NEXT();
    }
    CASE(END): {
        // t->c.pc = pc + 2;
        RETURN(RES_SUCCESS);
    }
#if CGOTO
#else
    }
#endif
}