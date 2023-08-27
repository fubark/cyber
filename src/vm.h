#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <math.h>

typedef uint8_t u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef uint64_t u64;

typedef struct Str {
    const char* ptr;
    size_t len;
} Str;

typedef struct IndexSlice {
    u32 start;
    u32 len;
} IndexSlice;

#define CALL_OBJ_SYM_INST_LEN 16
#define CALL_SYM_INST_LEN 12
#define CALL_INST_LEN 3

typedef enum {
    CodeConstOp = 0,
    CodeConstI8,
    CodeConstI8Int,
    CodeAdd,
    CodeSub,
    CodeTrue,
    CodeFalse,
    CodeNone,
    CodeNot,
    CodeCopy,
    CodeCopyReleaseDst,
    CodeSetIndex,
    CodeSetIndexRelease,
    CodeCopyRetainSrc,
    CodeIndex,
    CodeReverseIndex,
    CodeList,
    CodeMap,
    CodeMapEmpty,
    CodeSlice,
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
    CodeCall0,
    CodeCall1,
    CodeField,
    CodeFieldIC,
    CodeFieldRetain,
    CodeFieldRetainIC,
    CodeLambda,
    CodeClosure,
    CodeCompare,
    CodeLess,
    CodeGreater,
    CodeLessEqual,
    CodeGreaterEqual,
    CodeMul,
    CodeDiv,
    CodePow,
    CodeMod,
    CodeCompareNot,
    CodeStringTemplate,
    CodeNeg,
    CodeInit,
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
    CodeJumpNotNone,
    CodeAddInt,
    CodeSubInt,
    CodeLessInt,
    CodeForRangeInit,
    CodeForRange,
    CodeForRangeReverse,
    CodeMatch,
    CodeStaticFunc,
    CodeStaticVar,
    CodeSetStaticVar,
    CodeSetStaticFunc,
    CodeSym,
    CodeEnd,
} OpCode;

typedef uint32_t TypeId;
typedef enum {
    TYPE_NONE = 0,
    TYPE_BOOLEAN = 1,
    TYPE_ERROR = 2,
    TYPE_STATIC_ASTRING = 3,
    TYPE_STATIC_USTRING = 4,
    TYPE_ENUM = 5,
    TYPE_SYMBOL = 6,
    TYPE_INTEGER = 7,
    TYPE_NUMBER = 8,
    TYPE_LIST,
    TYPE_LIST_ITER,
    TYPE_MAP,
    TYPE_MAP_ITER,
    TYPE_CLOSURE,
    TYPE_LAMBDA,
    TYPE_ASTRING,
    TYPE_USTRING,
    TYPE_STRING_SLICE,
    TYPE_RAWSTRING,
    TYPE_RAWSTRING_SLICE,
    TYPE_FIBER,
    TYPE_BOX,
    TYPE_NATIVE_FUNC,
    TYPE_TCC_STATE,
    TYPE_POINTER,
    TYPE_FILE,
    TYPE_DIR,
    TYPE_DIR_ITER,
    TYPE_METATYPE,
} Type;

typedef uint32_t SemaTypeId;
typedef enum {
    SEMA_TYPE_ANY = 0,
    SEMA_TYPE_BOOLEAN,
    SEMA_TYPE_NUMBER,
    SEMA_TYPE_INTEGER,
    SEMA_TYPE_STRING,
    SEMA_TYPE_RAWSTRING,
    SEMA_TYPE_SYMBOL,
    SEMA_TYPE_LIST,
    SEMA_TYPE_MAP,
    SEMA_TYPE_POINTER,
    SEMA_TYPE_NONE,
    SEMA_TYPE_ERROR,
    SEMA_TYPE_FIBER,
    SEMA_TYPE_METATYPE,

    SEMA_TYPE_NUMBERLIT = 14,
    SEMA_TYPE_UNDEFINED,
    SEMA_TYPE_STATICSTRING,
    SEMA_TYPE_FILE,
    SEMA_TYPE_DYNAMIC,
} SemaType;

typedef uint8_t Inst;
typedef uint64_t Value;

typedef union ValueUnion {
    double d;
    uint64_t u;
} ValueUnion;
typedef uint64_t Const;

typedef u32 NodeId;
typedef u32 ChunkId;
typedef u32 MethodId;
typedef u32 ResolvedSymId;
typedef u32 ResolvedFuncSigId;
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

typedef struct ResolvedSym {
    AbsResolvedSymKey key;
    u64 padding1;
    u32 padding2;
    u8 symT;
    u8 exported;
    u8 genStaticInitVisited;
} ResolvedSym;

typedef struct ResolvedFuncSig {
    ResolvedSymId* paramPtr;
    ResolvedSymId retSymId;
    uint16_t paramLen;
    bool isTyped;
} ResolvedFuncSig;

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

typedef struct Fiber {
    TypeId typeId;
    uint32_t rc;
    struct Fiber* prevFiber;
    Value* stackPtr;
    uint32_t stackLen;
    uint32_t pcOffset;
    uint32_t stackOffset;

    uint32_t tryStackCap;
    void* tryStackPtr;
    uint32_t tryStackLen;

    uint32_t throwTraceCap;
    void* throwTracePtr;
    uint32_t throwTraceLen;

    uint32_t initialPcOffset;

    u64 panicPayload;
    u8 panicType;
    u8 parentDstLocal;
} Fiber;

typedef struct Object {
    TypeId typeId;
    uint32_t rc;
    Value firstValue;
} Object;

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

typedef struct Map {
    TypeId typeId;
    uint32_t rc;
    struct {
        u64* metadata;
        void* entries;
        u32 size;
        u32 cap;
        u32 available;
        u32 padding;
    } inner;
} Map;

typedef union HeapObject {
    struct {
        uint32_t typeId;
        uint32_t rc;
    } head;
    Fiber fiber;
    Object object;
    MetaType metatype;
    Lambda lambda;
    Map map;
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

typedef struct ZCyList {
    void* buf;
    size_t cap;
    size_t len;
} ZCyList;

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

typedef struct VmType {
    char* namePtr;
    size_t nameLen;
    uint32_t numFields;
    uint32_t typeSymId;
} VmType;

typedef struct ByteCodeBuffer {
    ZAllocator alloc;
    uint32_t mainStackSize;
    ZList ops;
    ZList consts;

    void* mconsts_buf;
    size_t mconsts_len;

    ZList strBuf;
    ZHashMap strMap;

    ZList debugTable;

    ZList debugLabels;
} ByteCodeBuffer;

typedef struct VM VM;

typedef struct SemaModel {
    ZAllocator alloc;

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

    ZHashMap objectMembers;
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

typedef struct TraceInfo {
    void* opCountsBuf;
    size_t opCountsLen;
    u32 totalOpCounts;
    u32 numRetains;
    u32 numRetainAttempts;
    u32 numReleases;
    u32 numReleaseAttempts;
    u32 numForceReleases;
    u32 numRetainCycles;
    u32 numRetainCycleRoots;
} TraceInfo;

typedef struct VM {
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
#if DEBUG
    HeapObject* heapFreeTail;
#endif

    ZCyList tryStack;

#if TRACK_GLOBAL_RC
    size_t refCounts;
#endif

    ZCyList methodSyms;
    ZHashMap methodTable;

    ZHashMap methodSymSigs;

    ZCyList funcSyms;
    ZHashMap funcSymSigs;
    ZCyList funcSymDetails;

    ZCyList varSyms;
    ZHashMap varSymSigs;

    ZCyList fieldSyms;
    ZHashMap fieldTable;
    ZHashMap fieldSymSignatures;

    ZCyList types; // VmType
    ZHashMap typeSignatures;

    ZCyList enums;
    ZHashMap enumSignatures;

    ZCyList syms;
    ZHashMap symSignatures;

    ZCyList u8Buf;
    ZCyList u8Buf2;

    StackTrace stackTrace;

    ZHashMap funcSymDeps;
    ZCyList methodSymExtras;
    DebugSym* debugTablePtr;
    size_t debugTableLen;

    Fiber* curFiber;
    Fiber mainFiber;

    ZCyList throwTrace;

#if TRACE_ENABLED
    TraceInfo* trace;
#endif

    Compiler compiler;
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

typedef Value (*FuncPtr)(VM* vm, Value* args, uint8_t nargs);
typedef Value (*MethodPtr)(VM* vm, Value recv, Value* args, uint8_t nargs);

// C API.
ResultCode execBytecode(VM* vm);

// Zig vars.
extern bool verbose;

// Zig functions.
void zFatal();
BufferResult zAlloc(ZAllocator alloc, size_t n);
char* zOpCodeName(OpCode code);
PcSp zCallSym(VM* vm, Inst* pc, Value* stack, uint16_t symId, uint8_t startLocal, uint8_t numArgs, uint8_t reqNumRetVals);
void zDumpEvalOp(VM* vm, Inst* pc);
void zDumpValue(Value val);
void zFreeObject(VM* vm, HeapObject* obj);
void zEnd(VM* vm, Inst* pc);
ValueResult zAllocList(VM* vm, Value* elemStart, uint8_t nelems);
double zOtherToF64(Value val);
CallObjSymResult zCallObjSym(VM* vm, Inst* pc, Value* stack, Value recv, TypeId typeId, uint8_t symId, uint16_t rFuncSigId, uint8_t startLocal, uint8_t numArgs, uint8_t numRet);
ValueResult zAllocFiber(VM* vm, uint32_t pc, Value* args, uint8_t nargs, uint8_t initialStackSize);
PcSp zPushFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Fiber* fiber, uint8_t parentDstLocal);
PcSp zPopFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Value retValue);
ValueResult zAllocObjectSmall(VM* vm, TypeId typeId, Value* fields, uint8_t nfields);
uint8_t zGetFieldOffsetFromTable(VM* vm, TypeId typeId, uint32_t symId);
Value zEvalCompare(VM* vm, Value left, Value right);
Value zEvalCompareNot(VM* vm, Value left, Value right);
ValueResult zGetIndex(VM* vm, Value* recv, Value indexv); 
PcSpResult zCall(VM* vm, Inst* pc, Value* stack, Value callee, uint8_t startLocal, uint8_t numArgs, Value retInfo);
HeapObjectResult zAllocPoolObject(VM* vm);
ValueResult zAllocStringTemplate(VM* vm, Inst* strs, u8 strCount, Value* vals, u8 valCount);
ValueResult zAllocMap(VM* vm, u16* keyIdxs, Value* vals, u32 numEntries);
Value zGetFieldFallback(VM* vm, HeapObject* obj, NameId nameId);
ResultCode zSetIndexRelease(VM* vm, Value left, Value index, Value right);
ResultCode zSetIndex(VM* vm, Value left, Value index, Value right);