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

// 0000000000000001
#define INTEGER_MASK ((u64)1 << 48)

// 0111111111111101 0000000000000111
#define TAGGED_PRIMITIVE_MASK (TAGGED_VALUE_MASK | PRIMITIVE_MASK)
// 0000000000000001 0000000000000111: Bits relevant to the primitive's type.
#define PRIMITIVE_MASK (TAGGED_VALUE_MASK | ((u64)TAG_MASK << 32) | INTEGER_MASK)

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

// 0001111111111111
#define TYPE_MASK ((u32)0x1fffffff)

// 0010000000000000: Pool object bit. VM checks this bit to free host objects (allocated from embedded API).
#define POOL_TYPE_MASK ((u32)0x20000000)

// 0100000000000000: Cyclable type bit. Currently has two purposes.
//                   1. Since heap pages don't segregate non-cyc from cyc, it's used
//                      to check which objects to visit.
//                   2. Re-encapsulate an object pointer back to a Value.
#define CYC_TYPE_MASK ((u32)0x40000000)

// 1000000000000000: Mark bit.
#define GC_MARK_MASK ((u32)0x80000000)

// 1100000000000000
#define GC_MARK_CYC_TYPE_MASK ((u32)0xC0000000)

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

typedef enum {
    CodeConstOp = 0,
    
    /// Sets an immediate i8 value as an integer to a dst local.
    CodeConstI8,

    /// Add first two locals and stores result to a dst local.
    CodeAddFloat,

    /// Subtracts second local from first local and stores result to a dst local.
    CodeSubFloat,
    
    CodeTrue,
    CodeFalse,
    CodeNone,
    CodeNot,
    CodeCopy,
    CodeCopyReleaseDst,

    /// [listReg] [indexReg] [rightReg]
    /// Releases existing value and retains right.
    CodeSetIndexList,

    CodeSetIndexMap,

    CodeCopyRetainSrc,
    CodeIndexList,
    CodeIndexMap,
    CodeList,
    CodeMap,
    CodeMapEmpty,
    CodeSliceList,
    CodeJumpNotCond,
    CodeJumpCond,
    CodeJump,
    CodeRelease,
    CodeReleaseN,
    CodeCallObjSym,
    CodeCallObjNativeFuncIC,
    CodeCallObjFuncIC,
    CodeCallTypeCheck,
    CodeCallSym,
    CodeCallFuncIC,
    CodeCallNativeFuncIC,
    CodeRet1,
    CodeRet0,
    CodeCall,
    CodeField,
    CodeFieldIC,
    CodeFieldRetain,
    CodeFieldRetainIC,
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
    CodeInit,
    CodeObjectTypeCheck,
    CodeObjectSmall,
    CodeObject,
    CodeSetField,
    CodeSetFieldRelease,
    CodeSetFieldReleaseIC,
    CodeSetCheckFieldRelease,
    CodePushTry,
    CodePopTry,
    CodeThrow,
    CodeCoinit,
    CodeCoyield,
    CodeCoresume,
    CodeCoreturn,
    CodeRetain,
    CodeCopyRetainRelease,
    CodeBox,
    CodeSetBoxValue,
    CodeSetBoxValueRelease,
    CodeBoxValue,
    CodeBoxValueRetain,
    CodeCaptured,
    CodeTag,
    CodeTagLiteral,
    CodeCast,
    CodeCastAbstract,
    CodeBitwiseAnd,
    CodeBitwiseOr,
    CodeBitwiseXor,
    CodeBitwiseNot,
    CodeBitwiseLeftShift,
    CodeBitwiseRightShift,
    CodeJumpNone,
    CodeJumpNotNone,
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
    CodeSeqDestructure,
    CodeMatch,
    CodeStaticFunc,
    CodeStaticVar,
    CodeSetStaticVar,
    CodeSetStaticFunc,
    CodeSym,
    CodeEnd,
    NumCodes,
} OpCode;

typedef uint32_t TypeId;
enum {
    TYPE_NONE = 0,
    TYPE_BOOLEAN = 1,
    TYPE_ERROR = 2,
    TYPE_STATIC_ASTRING = 3,
    TYPE_STATIC_USTRING = 4,
    TYPE_ENUM = 5,
    TYPE_SYMBOL = 6,
    TYPE_INTEGER = 7,
    TYPE_FLOAT = 8,
    TYPE_TUPLE = 9,
    TYPE_LIST = 10,
    TYPE_LIST_ITER = 11,
    TYPE_MAP = 12,
    TYPE_MAP_ITER = 13,
    TYPE_CLOSURE = 14,
    TYPE_LAMBDA = 15,
    TYPE_ASTRING = 16,
    TYPE_USTRING = 17,
    TYPE_STRING_SLICE = 18,
    TYPE_RAWSTRING = 19,
    TYPE_RAWSTRING_SLICE = 20,
    TYPE_FIBER = 21,
    TYPE_BOX = 22,
    TYPE_NATIVE_FUNC = 23,
    TYPE_TCC_STATE = 24,
    TYPE_POINTER = 25,
    TYPE_METATYPE = 26,
};

typedef uint32_t SemaTypeId;
typedef enum {
    SEMA_TYPE_ANY = 0,
    SEMA_TYPE_BOOLEAN = 1,
    SEMA_TYPE_FLOAT = 2,
    SEMA_TYPE_INTEGER = 3,
    SEMA_TYPE_STRING = 4,
    SEMA_TYPE_RAWSTRING = 5,
    SEMA_TYPE_SYMBOL = 6,
    SEMA_TYPE_TUPLE = 7,
    SEMA_TYPE_LIST = 8,
    SEMA_TYPE_LIST_ITER = 9,
    SEMA_TYPE_MAP = 10,
    SEMA_TYPE_MAP_ITER = 11,
    SEMA_TYPE_POINTER = 12,
    SEMA_TYPE_NONE = 13,
    SEMA_TYPE_ERROR = 14,
    SEMA_TYPE_FIBER = 15,
    SEMA_TYPE_METATYPE = 16,

    SEMA_TYPE_UNDEFINED = 17,
    SEMA_TYPE_STATICSTRING = 18,

    SEMA_TYPE_DYNAMIC = 19,
    NumSemaTypes = 20,
} SemaType;

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
typedef u32 MethodGroupId;
typedef u32 TypeMethodGroupId;
typedef u32 SymbolId;
typedef u32 FuncSigId;
typedef u32 ModuleId;
typedef u32 NameId;

typedef struct Name {
    char* ptr;
    u32 len;
    bool owned;
} Name;

typedef struct AbsResolvedSymKey {
    u32 rParentSymId;
    u32 nameId;
} AbsResolvedSymKey;

typedef struct Symbol {
    AbsResolvedSymKey key;
    u64 data;
    u32 padding2;
    u8 symT;
    u8 exported;
    u8 genStaticInitVisited;
} Symbol;

typedef struct FuncSig {
    SymbolId* paramPtr;
    SymbolId retSymId;
    uint16_t paramLen;
    bool isTyped;
} FuncSig;

typedef struct HostFunc {
    TypeId typeId;
    u32 rc;
    void* func;
    u32 numParams;
    u32 rFuncSigId;
    Value tccState;
    bool hasTccState;
} HostFunc;

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

/// Holds info about a runtime try block.
typedef struct TryFrame {
    Value* fp;
    u32 catchPc;
    u8 catchErrDst;
} TryFrame;

/// Minimal stack frame to reconstruct a `StackFrame`.
typedef struct CompactFrame {
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

    uint32_t tryStackCap;
    TryFrame* tryStackPtr;
    uint32_t tryStackLen;

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

typedef struct Box {
    TypeId typeId;
    uint32_t rc;
    Value val;
} Box;

typedef struct Closure {
    TypeId typeId;
    u32 rc;
    u32 funcPc;
    u8 numParams;
    u8 numCaptured;
    u8 stackSize;
    u8 local;
    u64 rFuncSigId;
    Value firstCapturedVal;
} Closure;

typedef struct Lambda {
    TypeId typeId;
    uint32_t rc;
    uint32_t funcPc;
    uint8_t numParams;
    uint8_t stackSize;
    uint16_t padding;
    uint64_t rFuncSigId;
} Lambda;

typedef struct MetaType {
    TypeId typeId;
    uint32_t rc;
    uint32_t type;
    uint32_t symId;
} MetaType;

typedef struct ValueMap {
    u64* metadata;
    void* entries;
    u32 size;
    u32 cap;
    u32 available;
    u32 padding;
} ValueMap;

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

typedef union HeapObject {
    struct {
        u32 typeId;
        u32 rc;
    } head;
    Fiber fiber;
    Object object;
    MetaType metatype;
    Lambda lambda;
    Closure closure;
    Box box;
    Map map;
    List list;
    Tuple tuple;
    HostFunc hostFunc;
} HeapObject;

typedef struct ZAllocator {
    void* ptr;
    void* vtable;
} ZAllocator;

typedef struct ZHashMap {
    void* metadata;
    uint32_t size;
    uint32_t available;
} ZHashMap;

typedef struct ZList {
    void* buf;
    size_t len;
    size_t cap;
} ZList;

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

typedef struct FieldSymbolMap {
    uint32_t mruTypeId;
    uint32_t mruOffset;
    uint32_t mruFieldTypeSymId;
    uint32_t nameId;
} FieldSymbolMap;

// TODO: One idea to make the `Type` array more compact is to make @host objects take
// up two slots, thereby skipping a typeId.
typedef struct Type {
    const char* namePtr;
    SemaTypeId semaTypeId;
    uint16_t nameLen;
    bool isHostObject;
    union {
        struct {
            const void* getChildren;
            const void* finalizer;
        } hostObject;
        struct {
            u32 numFields;
        } object;
    } data;
} Type;

typedef struct ByteCodeBuffer {
    ZAllocator alloc;
    ZList ops;
    ZList consts;

    void* mconsts_buf;
    size_t mconsts_len;

    ZList strBuf;
    ZHashMap strMap;

    ZList debugTable;
    ZList debugReleaseTable;

    ZList debugMarkers;

    ZList unwindReleaseRegs;
    ZList unwindReleaseBacklinks;

    u32 mainStackSize;
} ByteCodeBuffer;

typedef struct VM VM;
typedef struct Compiler Compiler;

typedef struct SemaModel {
    ZAllocator alloc;
    Compiler* compiler;

    ZList nameSyms;
    ZHashMap nameSymMap;

    ZList resolvedSyms;
    ZHashMap resolvedSymMap;

    ZList resolvedFuncSyms;
    ZHashMap resolvedFuncSymMap;

    ZList resolvedFuncSigs;
    ZHashMap resolvedFuncSigMap;

    ZList resolvedUntypedFuncSigs;

    ZList modules;
    ZHashMap moduleMap;
} SemaModel;

typedef struct Compiler {
    ZAllocator alloc;
    VM* vm;
    ByteCodeBuffer buf;

    char* lastErrPtr;
    size_t lastErrLen;

    SemaModel sema;

    NodeId lastErrNode;
    ChunkId lastErrChunk;

    NodeId errorPayload;
} Compiler;

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
    FUNC_SYM_NATIVEFUNC1,
    FUNC_SYM_FUNC,
    FUNC_SYM_CLOSURE,
} FuncSymbolType;

typedef struct FuncSymbol {
    u32 entryT;
    union {
        struct {
            u16 typedFlagNumParams;
            u16 rFuncSigId;
        } nativeFunc1;
        struct {
            u32 rFuncSigId;
        } none;
        struct {
            u32 rFuncSigId;
        } func;
    } innerExtra;
    union {
        void* nativeFunc1;
        struct {
            u32 pc;
            u16 stackSize;
            u16 numParams;
        } func;
        void* closure;
    } inner;
} FuncSymbol;

typedef struct StaticVar {
    Value value;
} StaticVar;

typedef struct VM {
#if IS_32BIT
    Fiber mainFiber;
#endif
    ZAllocator alloc;

    Inst* curPc;
    Value* curStack;

    Value* stackPtr;
    size_t stackLen;

    Value* stackEndPtr;

    Inst* instPtr;
    size_t instLen;

    Const* constPtr;
    size_t constLen;

    char* strBufPtr;
    size_t strBufLen;

    ZHashMap strInterns;

    ZCyList heapPages;
    HeapObject* heapFreeHead;
#if TRACE
    HeapObject* heapFreeTail;
#endif
#if HAS_GC
    void* cyclableHead;
#endif

    ZCyList tryStack;

#if TRACK_GLOBAL_RC
    size_t refCounts;
#endif

    ZCyList methods;
    ZCyList methodGroups;
    ZHashMap methodGroupKeys;
    ZCyList typeMethodGroups;
    ZHashMap typeMethodGroupKeys;

    ZCyList funcSyms; // FuncSymbol
    ZHashMap funcSymSigs;
    ZCyList funcSymDetails;

    ZCyList varSyms; // StaticVar
    ZHashMap varSymSigs;

    ZCyList fieldSyms;
    ZHashMap fieldTable;
    ZHashMap fieldSymSignatures;

    ZCyList types; // Type
    ZHashMap typeSignatures;

    ZCyList enums;
    ZHashMap enumSignatures;

    ZCyList syms;
    ZHashMap symSignatures;

    ZCyList u8Buf;
    ZCyList u8Buf2;

    StackTrace stackTrace;

    ZHashMap funcSymDeps;
    ZCyList methodGroupExts;
    ZCyList methodExts;
    DebugSym* debugTablePtr;
    size_t debugTableLen;
    u32* debugTempIndexTablePtr;
    size_t debugTempIndexTableLen;
    u8* unwindTempRegsPtr;
    size_t unwindTempRegsLen;
    u8* unwindTempPrevIndexesPtr;
    size_t unwindTempPrevIndexesLen;

    Fiber* curFiber;
#if !IS_32BIT
    Fiber mainFiber;
#endif

    ZCyList throwTrace;
#if TRACE
    TraceInfo* trace;
#endif
    Compiler* compiler;
    void* userData;
    void* print;
#if TRACE
    ZHashMap objectTraceMap;
#endif

#if IS_32BIT
    #if TRACE
    u32 debugPc;
    #endif

    struct {
        void* ptr;
        void* vtable;
    } httpClient;
    void* stdHttpClient;
    MethodGroupId padding[24];
    size_t expGlobalRC;
    ZHashMap varSymExtras;
#else
    struct {
        void* ptr;
        void* vtable;
    } httpClient;
    void* stdHttpClient;
    size_t expGlobalRC;
    ZHashMap varSymExtras;

    #if TRACE
    u32 debugPc;
    #endif
    MethodGroupId padding[24];
#endif

} VM;

typedef struct EvalConfig {
    bool singleRun;
    bool enableFileModules;
    bool reload;
    bool genAllDebugSyms;
} EvalConfig;

typedef enum {
    RES_CODE_SUCCESS = 0,
    RES_CODE_PANIC,
    RES_CODE_STACK_OVERFLOW,
    RES_CODE_UNKNOWN,
} ResultCode;

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

typedef struct PcSp {
    Inst* pc;
    Value* sp;
} PcSp;

typedef struct PcSpResult {
    Inst* pc;
    Value* sp;
    ResultCode code;
} PcSpResult;

typedef Value (*HostFuncFn)(VM* vm, const Value* args, uint8_t nargs);

// C API.
ResultCode execBytecode(VM* vm);

// Zig vars.
extern bool verbose;

// Zig functions.
void zFatal();
BufferResult zAlloc(ZAllocator alloc, size_t n);
char* zOpCodeName(OpCode code);
PcSpResult zCallSym(VM* vm, Inst* pc, Value* stack, u16 symId, u8 startLocal, u8 numArgs, u8 reqNumRetVals);
void zDumpEvalOp(VM* vm, Inst* pc);
void zDumpValue(Value val);
void zFreeObject(VM* vm, HeapObject* obj);
void zEnd(VM* vm, Inst* pc);
ValueResult zAllocList(VM* vm, Value* elemStart, uint8_t nelems);
double zOtherToF64(Value val);
CallObjSymResult zCallObjSym(VM* vm, Inst* pc, Value* stack, Value recv, TypeId typeId, uint8_t mgId, u8 startLocal, u8 numArgs, u8 numRet, u16 anySelfFuncSigId);
ValueResult zAllocFiber(VM* vm, uint32_t pc, Value* args, uint8_t nargs, uint8_t initialStackSize);
PcSp zPushFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Fiber* fiber, uint8_t parentDstLocal);
PcSp zPopFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Value retValue);
ValueResult zAllocObjectSmall(VM* vm, TypeId typeId, Value* fields, uint8_t nfields);
uint8_t zGetFieldOffsetFromTable(VM* vm, TypeId typeId, uint32_t symId);
Value zEvalCompare(VM* vm, Value left, Value right);
Value zEvalCompareNot(VM* vm, Value left, Value right);
PcSpResult zCall(VM* vm, Inst* pc, Value* stack, Value callee, uint8_t startLocal, uint8_t numArgs, Value retInfo);
HeapObjectResult zAllocPoolObject(VM* vm);
HeapObjectResult zAllocExternalObject(VM* vm, size_t size);
HeapObjectResult zAllocExternalCycObject(VM* vm, size_t size);
ValueResult zAllocStringTemplate(VM* vm, Inst* strs, u8 strCount, Value* vals, u8 valCount);
ValueResult zAllocMap(VM* vm, u16* keyIdxs, Value* vals, u32 numEntries);
Value zGetFieldFallback(VM* vm, HeapObject* obj, NameId nameId);
void zPanicIncompatibleFuncSig(VM* vm, FuncId funcId, Value* args, size_t numArgs, FuncSigId targetFuncSigId);
ResultCode zSetStaticFunc(VM* vm, FuncId funcId, Value val);
ResultCode zGrowTryStackTotalCapacity(ZCyList* list, ZAllocator alloc, size_t minCap);
PcSpResult zThrow(VM* vm, Value* startFp, const Inst* pc, Value err);
u16 zOpMatch(VM* vm, const Inst* pc, Value* framePtr);
void zPrintStderr(const char* fmt, const FmtValue* vals, size_t len);
void zCheckDoubleFree(VM* vm, HeapObject* obj);
void zCheckRetainDanglingPointer(VM* vm, HeapObject* obj);
void zPanicFmt(VM* vm, const char* format, FmtValue* args, size_t numArgs);
Value zValueMapGet(VM* vm, ValueMap* map, Value key, bool* found);
ResultCode zMapSet(VM* vm, Map* map, Value key, Value val);