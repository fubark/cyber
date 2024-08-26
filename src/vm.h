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
typedef _BitInt(48) i48;

#define BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

typedef struct Str {
    const char* ptr;
    size_t len;
} Str;

typedef struct IndexSlice {
    u32 start;
    u32 len;
} IndexSlice;

// 1000000000000000: Most significant bit.
#define SIGN_MASK ((u64)1 << 63)

// 0111111111110000 (7FF0): +INF (from math.inf, +/zero, overflow)
// 1111111111110000 (FFF0): -INF (from math.neginf, -/zero, overflow)
// 0111111111111000 (7FF8): QNAN (from neg op on -QNAN, math.nan, zero/zero non-intel, QNAN arithmetic)
// 1111111111111000 (FFF8): -QNAN (from neg op on QNAN, zero/zero intel, -QNAN arithmetic, -QNAN/zero)
// Intel uses the sign bit for an indefinite real or -QNAN.

// 0111111111111100 (7FFC): QNAN and one extra bit to the right.
#define TAGGED_VALUE_MASK ((u64)0x7ffc000000000000)
// 1111111111111111 (FFFF): Tagged pointers, integers, and enums.
#define TAGGED_UPPER_VALUE_MASK ((u64)0xffff000000000000)

// 0000000000000010
#define UPPER_PLACEHOLDER_MASK ((u64)1 << 49)

// 0000000000000001
#define ENUM_MASK ((u64)1 << 48)

// 0111111111111111 0000000000000111
#define TAGGED_PRIMITIVE_MASK (TAGGED_VALUE_MASK | PRIMITIVE_MASK)
// 0000000000000011 0000000000000111: Bits relevant to the primitive's type.
#define PRIMITIVE_MASK (TAGGED_VALUE_MASK | ((u64)TAG_MASK << 32) | UPPER_PLACEHOLDER_MASK | ENUM_MASK)

// 1111111111111100: TaggedMask + Sign bit indicates a pointer value.
#define NOCYC_POINTER_MASK (TAGGED_VALUE_MASK | SIGN_MASK)

// 1111111111111110: Extra bit indicating cyclable pointer.
//                   GC uses this to skip an expensive dereference.
#define CYC_POINTER_MASK (NOCYC_POINTER_MASK | ((u64)1 << 49))

#define POINTER_MASK (CYC_POINTER_MASK)
#if IS_32BIT
    #define POINTER_PAYLOAD_MASK 0xFFFFFFFF
#else
    #define POINTER_PAYLOAD_MASK 0xFFFFFFFFFFFF
#endif

// 0111111111111110(7FFE) 0...
#define TAGGED_PLACEHOLDER_MASK (TAGGED_VALUE_MASK | UPPER_PLACEHOLDER_MASK)

// 0111111111111101(7FFD) 0...
#define TAGGED_ENUM_MASK (TAGGED_VALUE_MASK | ENUM_MASK)

// 0111111111111100 0000000000000000
#define VOID_MASK (TAGGED_VALUE_MASK | ((u64)TAG_VOID << 32))

// 0111111111111100 0000000000000001
#define BOOLEAN_MASK (TAGGED_VALUE_MASK | ((u64)TAG_BOOLEAN << 32))

#define TAG_MASK (((uint32_t)1 << 3) - 1)
#define TAG_VOID ((uint8_t)0)
#define TAG_BOOLEAN ((uint8_t)1)
#define TAG_ERROR ((uint8_t)2)
#define TAG_TAGLIT ((uint8_t)5)
#define TAG_SYMBOL ((uint8_t)6)
#define FALSE_MASK BOOLEAN_MASK
#define TRUE_BIT_MASK ((uint64_t)1)
#define TRUE_MASK (BOOLEAN_MASK | TRUE_BIT_MASK)

#define ERROR_MASK (TAGGED_VALUE_MASK | ((u64)TAG_ERROR << 32))
#define SYMBOL_MASK (TAGGED_VALUE_MASK | ((u64)TAG_SYMBOL << 32))
#define TAGLIT_MASK (TAGGED_VALUE_MASK | ((u64)TAG_TAGLIT << 32))
#define BEFORE_TAG_MASK ((u32)(0x00007fff << 3))
#define NULL_U32 UINT32_MAX
#define NULL_U16 UINT16_MAX
#define NULL_U8 UINT8_MAX

// 0001111111111111
#define TYPE_MASK ((u32)0x1fffffff)

// 0010000000000000: External object bit (allocated using the GPA). VM checks this bit to free host objects (allocated from libcyber).
#define EXTERNAL_MASK ((u32)0x20000000)

// 0100000000000000: Cyclable type bit. Currently has two purposes.
//                   1. Since heap pages don't segregate non-cyc from cyc, it's used
//                      to check which objects to visit.
//                   2. Re-encapsulate an object pointer back to a cyc/non-cyc Value.
#define CYC_TYPE_MASK ((u32)0x40000000)

// 1000000000000000: Mark bit.
#define GC_MARK_MASK ((u32)0x80000000)

// 1100000000000000
#define GC_MARK_CYC_TYPE_MASK ((u32)0xC0000000)

#define FRAME_VM 0
#define FRAME_HOST 1
#define FRAME_DYN 2

// [Construct values]
#define VALUE_BOOLEAN(b) (b ? TRUE_MASK : FALSE_MASK)
#define VALUE_VOID VOID_MASK
#define VALUE_FLOAT(n) ((ValueUnion){ .d = n }.u)
#define VALUE_ENUM(tag, val) ((Value)(ENUM_MASK | tag << 8 | val ))
#define VALUE_CALLINFO(retFlag, callInstOff, stack_size) ((Value)(retFlag | ((u32)callInstOff << 1) | ((u32)stack_size << 8)))
#define VALUE_TRUE TRUE_MASK
#define VALUE_FALSE FALSE_MASK
#define VALUE_INTERRUPT (ERROR_MASK | 0xffff) 
#define VALUE_RAW(u) u
#define VALUE_NOCYC_PTR(ptr) (NOCYC_POINTER_MASK | ((size_t)ptr & POINTER_PAYLOAD_MASK))
#define VALUE_CYC_PTR(ptr) (CYC_POINTER_MASK | ((size_t)ptr & POINTER_PAYLOAD_MASK))
#define VALUE_SYMBOL(symId) (SYMBOL_MASK | symId)
#define VALUE_TAGLIT(symId) (TAGLIT_MASK | symId)
#define VALUE_ERROR(symId) (ERROR_MASK | symId)

// [Value ops]
#define VALUE_AS_HEAPOBJECT(v) ((HeapObject*)(v & ~POINTER_MASK))
#define VALUE_AS_INTEGER(v) BITCAST(_BitInt(48), v) // Padding bits returned are undefined.
#define VALUE_AS_UINTEGER(v) BITCAST(unsigned _BitInt(48), v) // Padding bits returned are undefined.
#define VALUE_AS_U32(v) ((u32)(v & 0xffffffff))
#define VALUE_AS_FLOAT(v) ((ValueUnion){ .u = v }.d)
#define VALUE_AS_FLOAT_TO_INT(v) ((int32_t)VALUE_AS_FLOAT(v))
#define VALUE_AS_FLOAT_TO_INT64(v) ((int64_t)VALUE_AS_FLOAT(v))
#define VALUE_AS_BOOLEAN(v) (v == TRUE_MASK)
#define VALUE_GET_TAG(v) ((BITCAST(u32, v >> 32)) & TAG_MASK)
#define VALUE_CALLINFO_RETFLAG(v) (v & 0x1)

#define VALUE_IS_BOOLEAN(v) ((v & (TAGGED_PRIMITIVE_MASK | SIGN_MASK)) == BOOLEAN_MASK)
#define VALUE_IS_POINTER(v) (v >= NOCYC_POINTER_MASK)
#define VALUE_IS_CLOSURE(vm, v) (VALUE_IS_POINTER(v) && (vm->c.typesPtr[OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v))].kind == TYPE_KIND_FUNC_UNION) && (VALUE_AS_HEAPOBJECT(v)->func_union.kind == 2))
#define VALUE_IS_UPVALUE(v) (VALUE_IS_POINTER(v) && (OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v)) == TYPE_UPVALUE))
#define VALUE_IS_FLOAT(v) ((v & TAGGED_VALUE_MASK) != TAGGED_VALUE_MASK)
#define VALUE_IS_ERROR(v) ((v & (TAGGED_PRIMITIVE_MASK | SIGN_MASK)) == ERROR_MASK)

#define VALUE_IS_ARRAY(v) (VALUE_IS_POINTER(v) && (OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v)) == TYPE_ARRAY))
#define VALUE_IS_STRING(v) (VALUE_IS_POINTER(v) && (OBJ_TYPEID(VALUE_AS_HEAPOBJECT(v)) == TYPE_STRING))
#define VALUE_IS_INTEGER(v) ((v & TAGGED_UPPER_VALUE_MASK) == TAGGED_INTEGER_MASK)
#define VALUE_BOTH_INTEGERS(a, b) ((a & b & TAGGED_UPPER_VALUE_MASK) == TAGGED_INTEGER_MASK)
#define VALUE_ALL3_INTEGERS(a, b, c) ((a & b & c & TAGGED_UPPER_VALUE_MASK) == TAGGED_INTEGER_MASK)
#define OBJ_TYPEID(o) (o->head.typeId & TYPE_MASK)

typedef enum {
    FMT_TYPE_CHAR,
    FMT_TYPE_STRING,
    FMT_TYPE_I8,
    FMT_TYPE_U8,
    FMT_TYPE_I16,
    FMT_TYPE_U16,
    FMT_TYPE_U32,
    FMT_TYPE_I32,
    FMT_TYPE_I48,
    FMT_TYPE_U64,
    FMT_TYPE_F64,
    FMT_TYPE_BOOL,
    FMT_TYPE_PTR,
    FMT_TYPE_ENUM,
    FMT_TYPE_ERROR,
} FmtValueType;

typedef struct FmtValue {
    union {
        u8 u8;
        u16 u16;
        u32 u32;
        u64 u64;
        double f64;
        const char* string;
        u8 ch;
        bool b;
        void* ptr;
    } data;
    union {
        u32 string;
    } data2;
    u8 type;
} FmtValue;

#define CALL_OBJ_SYM_INST_LEN 16
#define CALL_SYM_INST_LEN 12
#define CALL_INST_LEN 4
#define INST_COINIT_LEN 7

#define CALL_ARG_START 5
#define CALLEE_START 4

typedef enum {
    CodeConstOp = 0,

    CodeConstRetain,
    
    /// Sets an immediate i8 value as an integer to a dst local.
    CodeConstIntV8,

    CodeConstByte,

    /// Add first two locals and stores result to a dst local.
    CodeAddFloat,

    /// Subtracts second local from first local and stores result to a dst local.
    CodeSubFloat,
    
    CodeTrue,
    CodeFalse,
    CodeNot,
    CodeNone,
    CodeCopy,
    CodeCopyReleaseDst,
    CodeCopyRetainSrc,
    CodeCopyRetainRelease,
    CodeCopyStruct,
    CodeCopyObjDyn,

    /// [listReg] [indexReg] [rightReg]
    /// Releases existing value and retains right.
    CodeSetIndexList,

    CodeSetIndexMap,

    CodeIndexList,
    CodeIndexTuple,
    CodeIndexMap,
    CodeAppendList,
    CodeListDyn,
    CodeList,
    CodeArray,
    CodeMap,
    CodeSliceList,
    CodeJumpNotCond,
    CodeJumpCond,
    CodeJump,
    CodeRelease,
    CodeReleaseN,

    // [ret] [numArgs] [hasRet] [symId] [callSig u16] [metadata]
    // Lower bits of metadata contains info relevant for operator inlining.
    // Most significant bit indicates whether it has attempted to do func ptr inlining.
    CodeCallObjSym,
    // [ret] [numArgs] [hasRet] [symId] [callSig u16] [metadata] [ptr u48] [recvT u16]
    CodeCallObjNativeFuncIC,
    CodeCallObjFuncIC,
    CodeCallSym,
    CodeCallFuncIC,
    CodeCallNativeFuncIC,
    CodeCallTrait,
    CodeCallSymDyn,
    CodeRet1,
    CodeRet0,
    CodeRetDyn,
    CodeCall,
    CodeTypeCheck,
    CodeTypeCheckOption,
    CodeFieldStruct,
    CodeField,
    CodeFieldDyn,
    CodeFieldDynIC,
    CodeLambda,
    CodeClosure,
    CodeCompare,
    CodeLessFloat,
    CodeGreaterFloat,
    CodeLessEqualFloat,
    CodeGreaterEqualFloat,
    CodeLessInt,
    CodeGreaterInt,
    CodeLessEqualInt,
    CodeGreaterEqualInt,
    /// Multiplies first two locals and stores result to a dst local.
    CodeMulFloat,
    /// Divides second local from first local and stores result to a dst local.
    CodeDivFloat,
    /// Raises first local's power to the value of the second local and stores result to a dst local.
    CodePowFloat,
    /// Perform modulus on the two locals and stores result to a dst local.
    CodeModFloat,
    CodeCompareNot,
    CodeStringTemplate,
    CodeNegFloat,
    CodeStructSmall,
    CodeStruct,
    CodeObjectSmall,
    CodeObject,
    CodeTrait,

    CodeBox,
    CodeUnbox,
    CodeAddrLocal,
    CodeAddrConstIndex,
    CodeAddrIndex,
    CodeDeref,
    CodeDerefStruct,
    CodeSetDeref,
    CodeSetDerefStruct,
    CodeUnwrapChoice,

    /// Set field with runtime type check.
    CodeSetFieldDyn,
    CodeSetFieldDynIC,

    /// Set field with predetermined field index.
    CodeSetField,
    
    CodeCatch,
    CodeThrow,
    CodeCoinit,
    CodeCoyield,
    CodeCoresume,
    CodeCoreturn,
    CodeAwait,
    CodeFutureValue,
    CodeRetain,
    CodeUp,
    CodeSetUpValue,
    CodeUpValue,
    CodeCaptured,
    CodeSetCaptured,
    CodeTagLit,
    CodeEnum,
    CodeSymbol,
    CodeCast,
    CodeCastAbstract,
    CodeBitwiseAnd,
    CodeBitwiseOr,
    CodeBitwiseXor,
    CodeBitwiseNot,
    CodeBitwiseLeftShift,
    CodeBitwiseRightShift,
    CodeAddInt,
    CodeSubInt,
    CodeMulInt,
    CodeDivInt,
    CodePowInt,
    CodeModInt,
    CodeNegInt,
    CodeForRangeInit,
    CodeForRange,
    CodeForRangeReverse,
    CodeMatch,
    CodeFuncPtr,
    CodeFuncUnion,
    CodeStaticVar,
    CodeSetStaticVar,
    CodeContext,
    CodeMetatype,
    CodeEnd,
    NumCodes,
} OpCode;

typedef uint32_t TypeId;
enum {
    // The order of the first 9 primitive types are required for the VM.
    TYPE_VOID = 0,
    TYPE_BOOLEAN = 1,
    TYPE_ERROR = 2,
    TYPE_PLACEHOLDER1 = 3,
    TYPE_BYTE = 4,
    TYPE_TAGLIT = 5,
    TYPE_SYMBOL = 6,
    TYPE_INTEGER = 7,
    TYPE_FLOAT = 8,

    // Common types for VM and AOT.
    TYPE_DYN = 9,
    TYPE_ANY = 10,

    // VM exclusive types.
    TYPE_TYPE = 11,

    TYPE_TUPLE = 12,
    TYPE_LIST_DYN = 13,
    TYPE_LIST_ITER_DYN = 14,
    TYPE_MAP = 15,
    TYPE_MAP_ITER = 16,
    TYPE_FUNC = 17,
    TYPE_PLACEHOLDER2 = 18,
    TYPE_PLACEHOLDER3 = 19,
    TYPE_EXTERN_FUNC = 20,
    TYPE_STRING = 21,
    TYPE_PLACEHOLDER4 = 22,
    TYPE_FIBER = 23,
    TYPE_UPVALUE = 24,
    TYPE_TCC_STATE = 25,
    TYPE_METATYPE = 26,
    TYPE_RANGE = 27,
    TYPE_TABLE = 28,
    TYPE_MEMORY = 29,
};

#define PrimitiveEnd 9
#define BuiltinEnd 30

typedef uint8_t Inst;
typedef uint64_t Value;

typedef union ValueUnion {
    double d;
    uint64_t u;
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
typedef u32 NameId;

typedef struct SemaSym {
    struct SemaSym* parent;
    u8 type;
    u16 metadata;
    u16 nameLen;
    char* namePtr;
} SemaSym;

typedef struct Name {
    const char* ptr;
    u32 len;
    bool owned;
} Name;

typedef struct FuncSig {
    SymbolId* paramPtr;
    SymbolId retSymId;
    uint16_t paramLen;
    bool isTyped;
} FuncSig;

typedef enum {
    /// Uncaught thrown error. Error value is in `panicPayload`.
    PANIC_UNCAUGHT_ERROR,

    /// Static msg.
    PANIC_STATIC_MSG,

    /// Msg string is in `panicPayload`. Lower u48 is the pointer, and upper u16 is the length.
    PANIC_MSG,

    /// panicPayload contains error value thrown from native function.
    PANIC_NATIVE_THROW,

    /// Out of memory during panic. Masks underlying error.
    PANIC_INFLIGHT_OOM,

    PANIC_NONE,
} PanicType;

/// Minimal stack frame to reconstruct a `StackFrame`.
typedef struct CompactFrame {
    // If `pcOffset == NULL_U32`, then this is a host frame.
    u32 pcOffset;
    u32 fpOffset;
} CompactFrame;

typedef struct Fiber {
    TypeId typeId;
    uint32_t rc;

    struct Fiber* prevFiber;
    Value* stackPtr;
    uint32_t stackLen;

    /// If pcOffset == NullId, the fiber is done.
    uint32_t pcOffset;
    uint32_t stackOffset;

    uint32_t throwTraceCap;
    CompactFrame* throwTracePtr;
    uint32_t throwTraceLen;

    /// Points to the first inst of the fiber.
    /// This is used to find end locals pc if any.
    uint32_t initialPcOffset;

    u64 panicPayload;
    u8 panicType;

    /// Where coyield and coreturn should copy the return value to.
    /// If this is the NullByteId, no value is copied and instead released.
    u8 parentDstLocal;

    u8 argStart;
    u8 numArgs;
    u8 stack_size;
} Fiber;

/// One data structure for astring/ustring slice it can fit into a pool object
/// and use the same layout.
typedef struct StringSlice {
    TypeId typeId;
    u32 rc;
    const char* buf;
    u32 len;

    u32 uCharLen;
    u32 uMruIdx;
    u32 uMruCharIdx;

    /// A Ustring slice may have a null or 0 parentPtr if it's sliced from StaticUstring.
    /// The lower 63 bits contains the parentPtr.
    /// The last bit contains an isAscii flag.
    u64 extra;
} StringSlice;

typedef struct ZCyList {
    void* buf;
    size_t cap;
    size_t len;
} ZCyList;

typedef struct Object {
    TypeId typeId;
    uint32_t rc;
    Value firstValue;
} Object;

typedef struct Trait {
    TypeId typeId;
    uint32_t rc;
    Value impl;
    u32 vtable;
} Trait;

typedef struct ValueMap {
    u64* metadata;
    void* entries;
    u32 size;
    u32 cap;
    u32 available;
    u32 padding;
} ValueMap;

typedef struct UpValue {
    TypeId typeId;
    uint32_t rc;
    Value val;
} UpValue;

#define FUNC_PTR_BC 0
#define FUNC_PTR_HOST 1

typedef union FuncData {
    struct {
        void* ptr;
        Value tcc_state;
        bool has_tcc_state;
    } host;
    struct {
        u32 pc;
        u16 stack_size;
    } bc;
    struct {
        u32 pc;
        u16 stack_size;
        u8 numCaptured;
        u8 local;
        Value firstCapturedVal;
    } closure;
} FuncData;

typedef struct FuncPtr {
    TypeId typeId;
    u32 rc;

    u16 numParams;
    u8 kind;
    bool reqCallTypeCheck;
    u32 sig;

    FuncData data;
} FuncPtr;

typedef struct FuncUnion {
    TypeId typeId;
    u32 rc;

    u16 numParams;
    u8 kind;
    bool reqCallTypeCheck;
    u32 sig;

    FuncData data;
} FuncUnion;

typedef struct MetaType {
    TypeId typeId;
    uint32_t rc;
    uint32_t type;
    uint32_t symId;
} MetaType;

typedef struct Map {
    TypeId typeId;
    uint32_t rc;
    ValueMap inner;
} Map;

typedef struct Tuple {
    TypeId typeId;
    u32 rc;
    u32 len;
    u32 padding;
    Value firstValue;
} Tuple;

typedef struct List {
    TypeId typeId;
    u32 rc;
    ZCyList list;
} List;

typedef struct Int {
    TypeId typeId;
    u32 rc;
    i64 val;
} Int;

typedef struct Pointer {
    TypeId typeId;
    u32 rc;
    void* ptr;
} Pointer;

typedef struct Range {
    TypeId typeId;
    u32 rc;
    i64 start;
    i64 end;
} Range;

typedef union HeapObject {
    struct {
        u32 typeId;
        u32 rc;
    } head;
    Fiber fiber;
    Object object;
    Trait trait;
    Range range;
    MetaType metatype;
    FuncPtr func_ptr;
    FuncUnion func_union;
    UpValue up;
    Map map;
    List list;
    Tuple tuple;
    Pointer pointer;
    Int integer;
} HeapObject;

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

#define TYPE_KIND_STRUCT 8
#define TYPE_KIND_OPTION 9
#define TYPE_KIND_FUNC_UNION 18

typedef struct TypeEntry {
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
        } struct_t;
        struct {
            void* getChildrenFn;
            void* finalizerFn;
        } hostObject;
    } data;
} TypeEntry;

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

typedef struct TypeField {
    TypeId type_id;
    uint16_t offset;
    bool boxed;
} TypeField;

typedef struct Field {
    uint32_t name_id;
} Field;

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
    u32 numRetains;
    u32 numRetainAttempts;
    u32 numReleases;
    u32 numReleaseAttempts;

    // Number cycle objects freed by gc.
    u32 numCycFrees;
} TraceInfo;

typedef enum {
    FUNC_SYM_FUNC,
    FUNC_SYM_HOSTFUNC,
    FUNC_SYM_NONE,
} FuncSymbolType;

typedef struct FuncSymbol {
    u8 type;
    bool is_method;
    bool req_type_check;
    u8 nparams;
    u32 sig;
    union {
        void* host_func;
        struct {
            u32 pc;
            u16 stackSize;
        } func;
    } data;
} FuncSymbol;

typedef struct StaticVar {
    Value value;
} StaticVar;

typedef struct ContextVar {
    Value value;
} ContextVar;

typedef struct EvalConfig {
    bool single_run;
    bool file_modules;
    bool gen_all_debug_syms;
    u8 backend;
    bool reload;
    bool spawn_exe;
} EvalConfig;

typedef struct VMC {
    Inst* curPc;
    Value* curStack;

    Value* stackPtr;
    size_t stackLen;

    Value* stackEndPtr;

    Inst* instPtr;
    size_t instLen;

    Const* constPtr;
    size_t constLen;

    Fiber* curFiber;
    Fiber mainFiber;

    ZCyList fields;

    ZCyList varSyms; // StaticVar

    ZCyList context_vars; // ContextVar

    TypeEntry* typesPtr;
    size_t typesLen;

    TraceInfo* trace;
    u32 debugPc;
    u32 trace_indent;

#if TRACK_GLOBAL_RC
    size_t refCounts;
#endif
} VMC;

typedef struct VM {
    ZAllocator alloc;

    VMC c;
} VM;

typedef int ResultCode;

enum {
    RES_CODE_SUCCESS = 0,
    RES_CODE_AWAIT,
    RES_CODE_PANIC,
    RES_CODE_STACK_OVERFLOW,
    RES_CODE_UNKNOWN,
};

typedef struct BufferResult {
    void* buf;
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

typedef struct CallObjSymResult {
    Inst* pc;
    Value* stack;
    ResultCode code;
} CallObjSymResult;

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

typedef Value (*HostFuncFn)(VM* vm);

// C API.
ResultCode execBytecode(VM* vm);

// Zig vars.
extern bool clVerbose;

// Zig functions.
void zFatal();
BufferResult zAlloc(ZAllocator alloc, size_t n);
char* zOpCodeName(OpCode code);
PcFpResult zCallSym(VM* vm, Inst* pc, Value* stack, u16 symId, u8 ret);
PcFpResult zCallTrait(VM* vm, Inst* pc, Value* stack, u16 vtable_idx, u8 ret);
PcFpResult zCallSymDyn(VM* vm, Inst* pc, Value* stack, u16 symId, u8 ret, u8 nargs);
void zDumpEvalOp(VM* vm, Inst* pc, Value* fp);
void zDumpValue(VM* vm, Value val);
void zFreeObject(VM* vm, HeapObject* obj);
void zEnd(VM* vm, Inst* pc);
ValueResult zAllocList(VM* vm, TypeId type_id, Value* elemStart, uint8_t nelems);
ValueResult zAllocListDyn(VM* vm, Value* elemStart, uint8_t nelems);
ValueResult zAllocArray(VM* vm, TypeId type_id, Value* elemStart, uint8_t nelems);
double zOtherToF64(Value val);
CallObjSymResult zCallObjSym(VM* vm, Inst* pc, Value* stack, Value recv, TypeId typeId, u16 method, u8 startLocal, u8 numArgs);
ValueResult zAllocFiber(VM* vm, uint32_t pc, Value* args, uint8_t nargs, uint8_t argDst, uint8_t initialStackSize);
PcFp zPushFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Fiber* fiber, uint8_t parentDstLocal);
PcFpOff zPopFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Value retValue);
ResultCode zAwait(VM* vm, Value value);
Value zFutureValue(VM* vm, Value mb_future);
TypeField zGetTypeField(VM* vm, TypeId typeId, uint32_t field_id);
Value zEvalCompare(Value left, Value right);
Value zEvalCompareNot(Value left, Value right);
PcFpResult zCall(VM* vm, Inst* pc, Value* stack, Value callee, uint8_t startLocal, uint8_t numArgs);
HeapObjectResult zAllocPoolObject(VM* vm);
HeapObjectResult zAllocExternalObject(VM* vm, size_t size);
HeapObjectResult zAllocExternalCycObject(VM* vm, size_t size);
ValueResult zAllocStringTemplate(VM* vm, Inst* strs, u8 strCount, Value* vals, u8 valCount);
ValueResult zAllocStringTemplate2(VM* vm, Value* strs, u8 strCount, Value* vals, u8 valCount);
ValueResult zAllocFuncPtr(VM* vm, TypeId ptr_t, u16 id);
Value zGetFieldFallback(VM* vm, HeapObject* obj, uint32_t field_id);
ResultCode zSetFieldFallback(VM* vm, HeapObject* obj, uint32_t field_id, Value val);
u16 zOpMatch(const Inst* pc, Value* framePtr);
void zLog(const char* fmt, const FmtValue* vals, size_t len);
void zCheckDoubleFree(VM* vm, HeapObject* obj);
void zCheckRetainDanglingPointer(VM* vm, HeapObject* obj);
void zPanicFmt(VM* vm, const char* format, FmtValue* args, size_t numArgs);
Value zValueMapGet(ValueMap* map, Value key, bool* found);
ResultCode zMapSet(VM* vm, Map* map, Value key, Value val);
Str zGetTypeName(VM* vm, TypeId id);
ResultCode zEnsureListCap(VM* vm, ZCyList* list, size_t cap);
void zTraceRetain(VM* vm, Value v);
Value zBox(VM* vm, Value v, TypeId type_id);
Value zUnbox(VM* vm, Value v, TypeId type_id);
ValueResult zCopyStruct(VM* vm, HeapObject* obj);