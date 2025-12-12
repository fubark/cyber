#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;
typedef double f64;
typedef float f32;

#define BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

typedef struct Bytes {
    char* ptr;
    size_t len;
} Bytes;

typedef struct IndexSlice {
    u32 start;
    u32 len;
} IndexSlice;

// 1000000000000000: Most significant bit.
#define SIGN_BIT ((u64)1 << 63)

// 0111111111110000 (7FF0): +INF (from math.inf, +/zero, overflow)
// 1111111111110000 (FFF0): -INF (from math.neginf, -/zero, overflow)
// 0111111111111000 (7FF8): QNAN (from neg op on -QNAN, math.nan, zero/zero non-intel, QNAN arithmetic)
// 1111111111111000 (FFF8): -QNAN (from neg op on QNAN, zero/zero intel, -QNAN arithmetic, -QNAN/zero)
// Intel uses the sign bit for an indefinite real or -QNAN.

// 0111111111111100 (7FFC): QNAN and one extra bit to the right.
#define TAGGED_VALUE_MASK ((u64)0x7ffc000000000000)

// 0000000000000010
#define UPPER1_BIT ((u64)1 << 49)
// 0000000000000001
#define UPPER2_BIT ((u64)1 << 48)

#define PTR_BIT (SIGN_BIT)

// 1111111111111111
#define TAGGED_UPPER_MASK (TAGGED_VALUE_MASK | SIGN_BIT | UPPER1_BIT | UPPER2_BIT)

// 1111111111111111 0000000000001111
#define TAGGED_PRIMITIVE_MASK (TAGGED_UPPER_MASK | ((u64)TAG_MASK << 32))

// 1111111111111110: Indicates a runtime ref type for a dyn type.
#define MIN_REF_MASK (PTR_BIT | TAGGED_VALUE_MASK | UPPER1_BIT)

// 1111111111111100: Struct pointers, must be >= to this mask.
#define MIN_PTR_MASK (PTR_BIT | TAGGED_VALUE_MASK)
#if IS_32BIT
    #define PTR_PAYLOAD_MASK 0xFFFFFFFF
#else
    #define PTR_PAYLOAD_MASK 0xFFFFFFFFFFFF
#endif

// 0111111111111100 0000000000000001
#define BOOLEAN_MASK (TAGGED_VALUE_MASK | ((u64)TAG_BOOLEAN << 32))

#define TAG_MASK (((uint32_t)1 << 4) - 1)
#define TAG_VOID ((uint8_t)1)
#define TAG_BOOLEAN ((uint8_t)2)
#define TAG_ERROR ((uint8_t)3)
#define TAG_SYMBOL ((uint8_t)6)

#define ERROR_MASK (TAGGED_VALUE_MASK | ((u64)TAG_ERROR << 32))
#define SYMBOL_MASK (TAGGED_VALUE_MASK | ((u64)TAG_SYMBOL << 32))
#define BEFORE_TAG_MASK ((u32)(0x00007fff << 3))
#define NULL_U32 UINT32_MAX
#define NULL_U16 UINT16_MAX
#define NULL_U8 UINT8_MAX

// 0000111111111111
#define TYPE_MASK ((u32)0x0fffffff)

// 1111000000000000
#define TYPE_BITS ((u32)0xf0000000)

// 0001000000000000:
#define UNUSED_TYPE_BIT2 ((u32)0x10000000)

// 0010000000000000: External object bit (allocated using the GPA). VM checks this bit to free host objects (allocated from libcyber).
#define EXTERNAL_TYPE_BIT ((u32)0x20000000)

// 0100000000000000: 
#define UNUSED_TYPE_BIT ((u32)0x40000000)

// 1000000000000000: Mark bit.
#define GC_MARK_BIT ((u32)0x80000000)

#define FRAME_VM 0
#define FRAME_HOST 1

// [Construct values]
#define VALUE_FLOAT(n) ((ValueUnion){ .d = n }.u)
#define VALUE_F32(n) ((ValueUnion){ .f = n }.u)
#define VALUE_CALLINFO(retFlag, callInstOff, stack_size) ((Value)(retFlag | ((u32)callInstOff << 1) | ((u32)stack_size << 8)))
#define VALUE_HOSTCALLINFO(retFlag) ((Value)(retFlag | ((u32)4 << 8) | ((u32)1 << 16)))
#define VALUE_INTERRUPT (ERROR_MASK | 0xffff) 
#define VALUE_RAW(u) u
#define VALUE_PTR(ptr) ((intptr_t)ptr)
#define VALUE_BOX_PTR(ptr) (MIN_PTR_MASK | ((size_t)ptr & PTR_PAYLOAD_MASK))

#define VALUE_ERROR(symId) (ERROR_MASK | symId)

// [Value ops]
#define VALUE_AS_OBJHEADER(v) ((ObjectHeader*)((intptr_t)v - 8))
#define VALUE_AS_HEAPOBJECT(v) ((HeapObject*)(v & PTR_PAYLOAD_MASK))
#define VALUE_AS_INTEGER(v) BITCAST(i64, v)
#define VALUE_AS_UINTEGER(v) BITCAST(u64, v) // Padding bits returned are undefined.
#define VALUE_AS_U32(v) ((u32)(v & 0xffffffff))
#define VALUE_AS_FLOAT(v) ((ValueUnion){ .u = v }.d)
#define VALUE_AS_FLOAT_TO_INT(v) ((int32_t)VALUE_AS_FLOAT(v))
#define VALUE_AS_FLOAT_TO_INT64(v) ((int64_t)VALUE_AS_FLOAT(v))
#define VALUE_AS_BOOLEAN(v) (v == TRUE_MASK)
#define VALUE_GET_TAG(v) ((BITCAST(u32, v >> 32)) & TAG_MASK)
#define VALUE_GET_STRPTR(v) ((char*)((String*)v)->ptr)
#define VALUE_GET_STRLEN(v) (((String*)v)->len & ~((u64)1 << 63))
#define VALUE_CALLINFO_RETFLAG(v) (v & 0x1)

#define VALUE_IS_BOOLEAN(v) ((v & TAGGED_PRIMITIVE_MASK) == BOOLEAN_MASK)
#define VALUE_IS_CLOSURE(vm, v) ((vm->c.typesPtr[OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v))]->kind == TYPE_KIND_FUNC) && (VALUE_AS_HEAPOBJECT(v)->func_union.kind == 2))
#define VALUE_IS_FLOAT(v) ((v & TAGGED_VALUE_MASK) != TAGGED_VALUE_MASK)
#define VALUE_IS_ERROR(v) (OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v)) == TYPE_ERROR)

#define VALUE_IS_ARRAY(v) ((OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v)) == TYPE_ARRAY))
#define VALUE_IS_STRING(v) ((OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v)) == TYPE_STRING))
#define OBJ_TYPEID(o) (((ObjectHeader*)(((intptr_t)o) - 8))->meta & TYPE_MASK)
#define OBJHEADER_TYPEID(o) (o->meta & TYPE_MASK)

#define CALL_INST_LEN 9
#define CALL_TRAIT_INST_LEN 5
#define CALL_PTR_INST_LEN 3
#define CALL_UNION_INST_LEN 3
#define INST_COINIT_LEN 7

#define CALL_ARG_START 4

// From ret info base.
#define CALLEE_START 3

typedef enum {
    CodeCONST_64 = 0,

    CodeCONST_STR,
    
    /// Sets an immediate i8 value as an integer to a dst local.
    CodeCONST_8S,

    CodeCONST_8,
    CodeCONST_16,
    CodeCONST_32,

    CodeTrue,
    CodeFalse,
    CodeLNOT,
    CodeIsZero,
    CodeMOV,
    CodeMOV_2,
    CodeMOV_3,
    CodeMOV_4,
    CodeMOV_N,

    /// [listReg] [indexReg] [rightReg]
    /// Releases existing value and retains right.
    // CodeSetIndexList,

    // CodeSetIndexMap,

    // CodeIndexList,
    // CodeIndexMap,
    // CodeAppendList,
    // CodeList,
    // CodeMap,
    // CodeSliceList,
    CodeJUMP_F,
    CodeJUMP_T,
    CodeJUMP,
    CodeReleaseOpt,
    CodeRelease,
    CodeDTOR_STR,

    CodeCHK_STK,
    CodeCALL,
    CodeCALL_HOST,
    CodeCALL_TRAIT,
    CodeRET_0,
    CodeRET,
    CodeRET_N,
    CodeCALL_PTR,
    CodeCALL_UNION,
    CodeLOAD_8,
    CodeLOAD_16,
    CodeLOAD_32,
    CodeLOAD_64,
    CodeLOAD_2W,
    CodeLOAD_3W,
    CodeLOAD_4W,
    CodeLOAD_N,
    CodeCLOSURE,
    CodeCMP_8,
    CodeCMP,
    CodeCMP_STR,
    CodeFEQ32,
    CodeFLT32,
    CodeFGT32,
    CodeFLE32,
    CodeFGE32,
    CodeFADD32,
    CodeFSUB32,
    CodeFMUL32,
    CodeFDIV32,
    CodeFMOD32,
    CodeFNEG32,
    CodeFEQ,
    CodeFLT,
    CodeFGT,
    CodeFLE,
    CodeFGE,
    CodeLT,
    CodeGT,
    CodeLE,
    CodeGE,
    CodeUCMP,
    CodeFADD,
    CodeFSUB,
    CodeFMUL,
    CodeFDIV,
    CodeFMOD,
    CodeFNEG,
    CodeNEW,
    CodeTrait,

    CodeADDR,
    CodeUnwrapAddr,
    CodeUnwrapNZ,

    CodeSTORE_8,
    CodeSTORE_16,
    CodeSTORE_32,
    CodeSTORE_64,
    CodeSTORE_2W,
    CodeSTORE_3W,
    CodeSTORE_4W,
    CodeSTORE_N,
    CodeMEMSETZ,
    
    CodeRET_GEN,
    CodeRET_Y,
    CodeGEN_NEXT,
    CodeGEN_END,
    CodeAWAIT,
    CodeRetainNZ,
    CodeRetain,
    CodeCaptured,
    CodeCast,
    CodeCastAbstract,
    CodeAND,
    CodeOR,
    CodeXOR,
    CodeNOT,
    CodeLSL,
    CodeLSR,
    CodeAdd,
    CodeAddI16,
    CodeSub,
    CodeIMUL,
    CodeMUL,
    CodeIDIV,
    CodeDIV,
    CodePow,
    CodeIMOD,
    CodeMOD,
    CodeNeg,
    CodeZEXT,
    CodeSEXT,
    CodeF2I,
    CodeF32_2I,
    CodeI2F,
    CodeI2F32,
    CodeFABS,
    CodeF32ABS,
    CodeFN_VM,
    CodeFN_HOST,
    CodeFN_UNION,
    CodeExternFunc,
    CodeNOP32,
    CodeNOPS,
    CodeTRAP,
    CodeTRAP_DEBUG,
    CodeEND,
    NumCodes,
} OpCode;

typedef uint32_t TypeId;
enum {
    // These types are reserved so that the IDs are consistent across builds.
    TYPE_NULL = 0,
    TYPE_VOID = 1,
    TYPE_BOOL = 2,
    TYPE_I8 = 3,
    TYPE_I16 = 4,
    TYPE_I32 = 5,
    TYPE_I64 = 6,
    TYPE_EVAL_INT = 7,
    TYPE_R8 = 8,
    TYPE_R16 = 9,
    TYPE_R32 = 10,
    TYPE_R64 = 11,
    TYPE_F32 = 12,
    TYPE_F64 = 13,
    TYPE_ERROR = 14,
    TYPE_SYMBOL = 15,
    TYPE_OBJECT = 16,
    TYPE_ANY = 17,
    TYPE_TYPE = 18,
    TYPE_THREAD = 19,
    TYPE_CODE = 20,
    TYPE_FUNC_SIG = 21,
    TYPE_PARTIAL_STRUCT_LAYOUT = 22,
    TYPE_STR = 23,
    TYPE_STR_BUFFER = 24,
    TYPE_EVAL_STR = 25,
    TYPE_NEVER = 26,
    TYPE_INFER = 27,
    TYPE_DEPENDENT = 28,
    TYPE_TCC_STATE = 29,
    TYPE_RANGE = 30,
    TYPE_TABLE = 31,
    TYPE_NO_COPY = 32,
    TYPE_MUT_STR = 33,
};

#define BuiltinEnd 34

typedef uint8_t Inst;
typedef uint64_t Value;
typedef uint8_t Ret;

typedef union ValueUnion {
    double d;
    uint64_t u;
    float f;
} ValueUnion;
typedef uint64_t Const;

typedef u32 FuncId;
typedef u32 NodeId;
typedef u32 ChunkId;
typedef u32 MethodId;
typedef u32 TypeMethodId;
typedef u32 SymbolId;
typedef u32 FuncSigId;
typedef u32 ModuleId;
typedef u32 ModuleSymId;

typedef struct SemaSym {
    struct SemaSym* parent;
    u8 type;
    u16 metadata;
    u16 nameLen;
    char* namePtr;
} SemaSym;

typedef struct FuncSig {
    SymbolId* paramPtr;
    SymbolId retSymId;
    uint16_t paramLen;
} FuncSig;

typedef enum {
    /// Panic from user code. Error value is in `panicPayload`.
    PANIC_ERROR,

    /// Static msg.
    PANIC_STATIC_MSG,

    /// Msg string is in `panicPayload`. Lower u48 is the pointer, and upper u16 is the length.
    PANIC_MSG,

    /// Out of memory during panic. Masks underlying error.
    PANIC_INFLIGHT_OOM,

    PANIC_NONE,
} PanicType;

/// Minimal stack frame to reconstruct a `StackFrame`.
typedef struct CompactFrame {
    // If `pc == NULL`, then this is a host frame.
    Inst* pc;
    u32 fpOffset;
} CompactFrame;

typedef struct Object {
    Value firstValue;
} Object;

typedef struct Trait {
    Value impl;
    u32 vtable;
} Trait;

#define FUNC_PTR_BC 0
#define FUNC_PTR_HOST 1

typedef union FuncData {
    struct {
        void* ptr;
    } host;
    struct {
        Inst* pc;
    } bc;
    struct {
        Inst* pc;
        u64 numCaptured;
        Value firstCapturedVal;
    } closure;
} FuncData;

typedef struct FuncPtr {
    u8 kind;
    u8 padding;
    u16 padding2;
    u32 sig;

    FuncData data;
} FuncPtr;

typedef struct FuncUnion {
    u8 kind;
    u8 padding;
    u16 padding2;
    u32 stack_size;

    FuncData data;
} FuncUnion;

typedef struct Type {
    uint32_t type;
} Type;

typedef struct Map {
    u64 padding[5];
} Map;

typedef struct String {
    void* buf;
    u8* ptr;
    u64 len;
} String;

typedef struct Buffer {
    u64 len;
    u8  data;
} Buffer;

typedef struct Int {
    i64 val;
} Int;

typedef struct Pointer {
    void* ptr;
} Pointer;

typedef struct Range {
    i64 start;
    i64 end;
} Range;

typedef struct ObjectHeader {
    u32 meta;
    u32 rc;
} ObjectHeader;

typedef struct Slice {
    u64 buf;
    void* ptr;
    u64 len;
    u64 header;
} Slice;

typedef struct Generator {
    Value* frame_ptr;
    u32 frame_len;
    Inst* resume_pc;
    Inst* deinit_pc;
    Value* prev_fp;
    bool running;
    bool done;
} Generator;

// Unused members are declared for debugging.
typedef union HeapObject {
    Object object;
    Trait trait;
    Range range;
    Type type;
    FuncPtr func_ptr;
    FuncUnion func_union;
    Map map;
    // List list;
    Pointer pointer;
    Int integer;
    String string;
    Slice slice;
    Generator generator;
} HeapObject;

typedef struct ZSlice {
    void* ptr;
    size_t len;
} ZSlice;

typedef struct ZAllocator {
    void* ptr;
    void* vtable;
} ZAllocator;

typedef struct ZHashMap {
    void* metadata;
    u32 size;
    u32 available;
#if RT_SAFETY
    u8 pointer_stability;
#endif
} ZHashMap;

typedef struct ZList {
    void* buf;
    size_t len;
    size_t cap;
} ZList;

#define TYPE_KIND_NULL 0
#define TYPE_KIND_BOOL 1
#define TYPE_KIND_INT 2
#define TYPE_KIND_RAW 3
#define TYPE_KIND_FLOAT 4
#define TYPE_KIND_ENUM 5
#define TYPE_KIND_CHOICE 6
#define TYPE_KIND_STRUCT 7
#define TYPE_KIND_OPTION 8
#define TYPE_KIND_GENERIC_TRAIT 9
#define TYPE_KIND_BARE 10
#define TYPE_KIND_GENERIC 11
#define TYPE_KIND_VECTOR 12
#define TYPE_KIND_FUNC_PTR 13
#define TYPE_KIND_FUNC 14
#define TYPE_KIND_FUNC_SYM 15
#define TYPE_KIND_PTR 16
#define TYPE_KIND_BORROW 17
#define TYPE_KIND_VOID 18
#define TYPE_KIND_RESULT 19
#define TYPE_KIND_REF_TRAIT 20
#define TYPE_KIND_BORROW_TRAIT 21
#define TYPE_KIND_GENERIC_VECTOR 22
#define TYPE_KIND_NEVER 23
#define TYPE_KIND_CVARIADIC 24
#define TYPE_KIND_EVAL_INT 25
#define TYPE_KIND_CUNION 26
#define TYPE_KIND_PARTIAL_VECTOR 27
#define TYPE_KIND_EXBORROW 28
#define TYPE_KIND_DYN_TRAIT 29
#define TYPE_KIND_EVAL_REF 30

typedef struct TypeBase {
    u8 kind;
    void* sym;
    TypeId id;

    u8 info;
    bool has_get_method;
    bool has_set_method;
    bool has_init_pair_method;

    void* retain_layout;
    bool retain_layout_owned;
} TypeBase;

typedef struct BoxedEntry {
    u32 offset;
} BoxedEntry;

typedef struct StructType {
    TypeBase base;

    void* fields_ptr;
    u32 fields_len;
    bool fields_owned;

    u32 size;

    void* impls_ptr;
    u32 impls_len;

    bool cstruct;
    bool tuple;

    bool resolving_struct;
} StructType;

typedef struct ArrayType {
    TypeBase base;

    size_t n;
    TypeBase* elem_t;
} ArrayType;

// NOTE: Keep for reference when designing the compact type entry.
typedef struct TypeEntryOld {
    void* sym;
    u8 kind;
    bool has_get_method;
    bool has_set_method;
    bool has_init_pair_method;
    bool cyclable;
    u8 info;
    union {
        struct {
            u16 numFields;
        } object;
        struct {
            u16 numFields;

            bool cstruct;
            bool has_boxed_fields;
            bool tuple;

            bool* fields;
        } struct_t;
        struct {
            size_t n;
            TypeId elem_t;
        } array;
        struct {
            void* getChildrenFn;
            void* finalizerFn;
        } hostObject;
    } data;
} TypeEntryOld;

typedef struct StackTrace {
    void* framePtr;
    size_t frameLen;
} StackTrace;

typedef struct DebugSym {
    uint32_t pc;
    uint32_t loc;
    uint32_t frameLoc;
    uint32_t file;
} DebugSym;

typedef struct VM VM;

typedef struct Module {
    ZHashMap symMap;
    ZHashMap overloadedFuncMap;
    ZList retainedVars;
    void* chunk;
} Module;

typedef struct OpCount {
    u32 code;
    u32 count;
} OpCount;

typedef struct TraceInfo {
    OpCount opCounts[NumCodes];
    u32 totalOpCounts;
} TraceInfo;

typedef enum {
    FUNC_SYM_FUNC,
    FUNC_SYM_HOSTFUNC,
    FUNC_SYM_NONE,
} FuncSymbolType;

typedef struct FuncSymbol {
    u8 type;
    union {
        void* host_func;
        struct {
            void* pc;
        } func;
    } data;
} FuncSymbol;

// typedef struct GlobalSym {
//     Value value;

//     // *cy.Sym for debugging.
//     void* sym;
//} GlobalSym;

typedef struct EvalConfig {
    bool single_run;
    bool file_modules;
    bool gen_all_debug_syms;
    u8 backend;
    bool reload;
    bool spawn_exe;
} EvalConfig;

typedef struct TypeInfo {
    const char* name_ptr;
    size_t name_len;
} TypeInfo;

typedef struct ZVM ZVM;
typedef struct VM {
    // Not protected for incremental compilation.
    Const* consts_ptr;
    size_t consts_len;

    u8 rw_lock;

    // Trace mode type info. Protected by `rw_lock`.
    TypeInfo* types_ptr;
    size_t types_len;
} VM;

typedef struct ZThread ZThread;
typedef struct Heap {
    ZVM* vm;
    ZThread* thread;
    // Used to record the current context when tracing object allocations/frees.
    // e.g. The current PC for the VM.
    // The VM updates this before each instruction is interpreted.
    u64 ctx;

    u32 numRetains;
    u32 numReleases;
    size_t refCounts;

    u64 trace_event;
    u64 trace_panic_at_event;
    u64 trace_track_object_event;
} Heap;

typedef struct ZHeap {
#if !defined(__wasm__)
    u64 padding[14];
#endif
    Heap c;
} ZHeap;

typedef struct Thread {
    size_t id;
    ZVM* vm;
    // ZAllocator alloc;

    /// Program counter. Pointer to the current instruction data in `ops`.
    Inst* pc;

    // Current stack frame ptr.
    Value* fp;

    // End of current frame. This is optionally set in host calls to mark the end of a frame
    // so that another call frame can be pushed onto the stack.
    Value* fp_end;

    Value* stack_ptr;
    size_t stack_len;
    Value* stack_end;

    // bool unwinding;

    u64 panic_payload;
    u8 panic_type;

    // ZList compact_trace;
    // ZList stack_trace;    

    TraceInfo* trace;

    // Separate traced context from actual context.
    // This allows bugs related to actual context to surface. (VM persists context when exiting the hot loop)
    Inst* trace_pc;
    Value* trace_fp;
} Thread;

typedef struct ZThread {
#if !defined(__wasm__)
    u8 padding[16];
#endif
    Thread c;
    ZHeap heap;
} ZThread;

extern __thread ZThread* cur_thread;

typedef struct ZVM {
#if defined(__wasm__)
    u8 padding[184];
#elif !(DEBUG) || defined(__linux__)
    u8 padding[456];
#else
    u8 padding[464];
#endif
    VM c;
} ZVM;

typedef struct Fiber {
    struct Fiber* prevFiber;
    Value* stackPtr;
    uint32_t stackLen;

    /// If pc == NULL, the fiber is done.
    Inst* pc;
    uint32_t stackOffset;

    uint32_t throwTraceCap;
    CompactFrame* throwTracePtr;
    uint32_t throwTraceLen;

    /// Points to the first inst of the fiber.
    /// This is used to find end locals pc if any.
    uint32_t initialPcOffset;

    /// When panic is triggered again during a panic unwinding, the fiber will perform an abort instead.
    bool unwinding;

    /// Where coyield and coreturn should copy the return value to.
    /// If this is the NullByteId, no value is copied and instead released.
    u8 parentDstLocal;

    u8 argStart;
    u8 numArgs;
    u8 stack_size;
} Fiber;

typedef struct FiberObject {
    u32 meta;
    u32 rc;
    Fiber fiber;
} FiberObject;

typedef int ResultCode;

enum {
    RES_SUCCESS = 0,
    RES_AWAIT,
    RES_PANIC,
    RES_STACK_OVERFLOW,
    RES_UNKNOWN,
};

typedef struct BufferResult {
    u8* buf;
    size_t len;
    ResultCode code;
} BufferResult;

typedef struct HeapObjectResult {
    HeapObject* obj;
    ResultCode code;
} HeapObjectResult;

typedef struct ValueResult {
    Value val;
    ResultCode code;
} ValueResult;

typedef struct PcFp {
    Inst* pc;
    Value* fp;
} PcFp;

typedef struct PcFpOff {
    u32 pc;
    u32 fp;
} PcFpOff;

typedef struct PcFpResult {
    Inst* pc;
    Value* fp;
    ResultCode code;
} PcFpResult;

typedef uint8_t (*HostFn)(ZThread* t);

// C API.
ResultCode execBytecode(ZThread* t);

// Zig vars.
extern bool clVerbose;

// Zig functions.
void zFatal();
BufferResult zAlloc(ZVM* vm, size_t n);
char* zOpCodeName(OpCode code);
PcFpResult zCallTrait(ZThread* t, Inst* pc, Value* stack, u16 vtable_idx, u16 ret);
void z_dump_thread_inst(ZThread* t, Inst* pc);
void zDestroyObject(ZThread* t, HeapObject* obj);
void zFreePoolObject(ZThread* t, HeapObject* obj);
void zFreeBigObject(ZThread* t, HeapObject* obj, size_t size);
ResultCode z_ret_generator(ZThread* t, TypeId type_id, u16 ret_size, Value* fp, size_t frame_len, Inst* resume_pc, Inst* deinit_pc);
ResultCode zAwait(ZThread* t, Value value);
PcFpResult zCallPtr(ZThread* t, Inst* pc, Value* stack, uint16_t base);
PcFpResult zCallUnion(ZThread* t, Inst* pc, Value* stack, uint16_t base);
HeapObjectResult zAllocPoolObject(ZThread* t, TypeId id);
HeapObjectResult zAllocBigObject(ZThread* t, TypeId id, size_t size);
ValueResult zAllocClosure(ZThread* t, Value* fp, Inst* func_pc, TypeId ptr_t, Inst* captures, u8 ncaptures, bool stack_func);
ValueResult zAllocFuncUnion(ZThread* t, TypeId id, Value func_ptr);
void z_log(ZThread* t, const char* msg, size_t len);
bool zCheckDoubleFree(ZThread* t, HeapObject* obj);
bool zCheckRetainDanglingPointer(ZThread* t, HeapObject* obj);
void z_panic_unexpected_choice(ZThread* t, int64_t exp, int64_t act);
void zFree(ZThread* t, Bytes bytes);
Bytes zGetTypeName(ZVM* vm, TypeId id);
ResultCode zEnsureListCap(ZVM* vm, ZList* list, size_t cap);
void zTraceRetain(ZVM* vm, Value v);
ValueResult zCopyStruct(ZVM* vm, HeapObject* obj);
void zPrintTraceAtPc(ZThread* t, u32 debugPc, Bytes title, Bytes msg);
void z_dump_stack_trace(ZThread* t);
void zDumpObjectTrace(ZThread* t, HeapObject* obj);
void* zGetExternFunc(ZVM* vm, u32 func);
size_t z_write_ptr(Bytes buf, void* ptr);
size_t z_write_u64(Bytes buf, u64 x);