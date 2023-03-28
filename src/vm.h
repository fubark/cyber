#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <math.h>

typedef enum {
    CodeConstOp,
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
    CodeFieldRelease,
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
    CodeSetInitN,
    CodeObjectSmall,
    CodeObject,
    CodeSetField,
    CodeSetFieldRelease,
    CodeSetFieldReleaseIC,
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
    CodeTag,
    CodeTagLiteral,
    CodeTryValue,
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
    TYPE_TYPESYM,
} Type;

typedef uint8_t Inst;
typedef uint64_t Value;
typedef union ValueUnion {
    double d;
    uint64_t u;
} ValueUnion;
typedef uint64_t Const;

typedef uint32_t TypeId;

typedef struct Fiber {
    TypeId typeId;
    uint32_t rc;
    struct Fiber* prevFiber;
    Value* stackPtr;
    uint32_t stackLen;
    uint32_t pc;
    uint64_t extra;
} Fiber;

typedef struct Object {
    TypeId typeId;
    uint32_t rc;
    Value firstValue;
} Object;

typedef union HeapObject {
    struct {
        uint32_t typeId;
        uint32_t rc;
    } retainedCommon;
    Fiber fiber;
    Object object;
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

typedef struct ZCyList {
    void* bufPtr;
    size_t bufLen;
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
    uint16_t mruOffset;
    uint16_t nameLen;
    const char* namePtr;
} FieldSymbolMap;

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

    ZCyList structs;
    ZHashMap structSignatures;

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
} VM;

typedef enum {
    RES_CODE_SUCCESS = 0,
    RES_CODE_PANIC_EXPECTED_NUMBER,
    RES_CODE_UNKNOWN,
} ResultCode;

typedef struct ValueResult {
    Value val;
    ResultCode code;
} ValueResult;

typedef struct CallObjSymResult {
    Inst* pc;
    Value* stack;
    ResultCode code;
} CallObjSymResult;

typedef struct PcStackResult {
    Inst* pc;
    Value* stack;
} PcStackResult;

typedef Value (*FuncPtr)(VM* vm, Value* args, uint8_t nargs);
typedef Value (*MethodPtr)(VM* vm, Value recv, Value* args, uint8_t nargs);

// C API.
ResultCode execBytecode(VM* vm);

// Calling into Zig.
void zFatal();
char* zOpCodeName(OpCode code);
PcStackResult zCallSym(VM* vm, Inst* pc, Value* stack, uint8_t symId, uint8_t startLocal, uint8_t numArgs, uint8_t reqNumRetVals);
void zDumpEvalOp(VM* vm, Inst* pc);
extern bool verbose;
void zFreeObject(VM* vm, HeapObject* obj);
void zEnd(VM* vm, Inst* pc);
ValueResult zAllocList(VM* vm, Value* elemStart, uint8_t nelems);
double zOtherToF64(Value val);
CallObjSymResult zCallObjSym(VM* vm, Inst* pc, Value* stack, Value recv, TypeId typeId, uint8_t symId, uint16_t rFuncSigId, uint8_t startLocal, uint8_t numArgs, uint8_t numRet);
ValueResult zAllocFiber(VM* vm, uint32_t pc, Value* args, uint8_t nargs, uint8_t initialStackSize);
PcStackResult zPushFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Fiber* fiber, uint8_t parentDstLocal);
PcStackResult zPopFiber(VM* vm, size_t curFiberEndPc, Value* curStack, Value retValue);
ValueResult zAllocObjectSmall(VM* vm, TypeId typeId, Value* fields, uint8_t nfields);
uint8_t zGetFieldOffsetFromTable(VM* vm, TypeId typeId, uint32_t symId);
Value zEvalCompare(VM* vm, Value left, Value right);
Value zEvalCompareNot(VM* vm, Value left, Value right);