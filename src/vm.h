#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

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
    TYPE_TAG = 5,
    TYPE_TAGLIT = 6,
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
    TYPE_SYMBOL,
} Type;

typedef uint8_t Inst;
typedef uint64_t Value;
typedef union ValueUnion {
    double d;
    uint64_t u;
} ValueUnion;
typedef uint64_t Const;

typedef uint32_t TypeId;

typedef union HeapObject {
    struct {
        uint32_t typeId;
        uint32_t rc;
    } retainedCommon;
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
} VM;

typedef enum {
    RES_CODE_SUCCESS = 0,
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

typedef struct CallSymResult {
    Inst* pc;
    Value* stack;
} CallSymResult;

typedef Value (*FuncPtr)(VM* vm, Value* args, uint8_t nargs);
typedef Value (*MethodPtr)(VM* vm, Value recv, Value* args, uint8_t nargs);

// C API.
ResultCode execBytecode(VM* vm);

// Calling into Zig.
void zFatal();
char* zOpCodeName(OpCode code);
CallSymResult zCallSym(VM* vm, Inst* pc, Value* stack, uint8_t symId, uint8_t startLocal, uint8_t numArgs, uint8_t reqNumRetVals);
void zDumpEvalOp(VM* vm, Inst* pc);
extern bool verbose;
void zFreeObject(VM* vm, HeapObject* obj);
void zEnd(VM* vm, Inst* pc);
ValueResult zAllocList(VM* vm, Value* elemStart, uint8_t nelems);
double zOtherToF64(Value val);
ValueResult zEvalAddFallback(VM* vm, Value left, Value right);
ValueResult zEvalSubFallback(VM* vm, Value left, Value right);
CallObjSymResult zCallObjSym(VM* vm, Inst* pc, Value* stack, Value recv, TypeId typeId, uint8_t symId, uint8_t startLocal, uint8_t numArgs, uint8_t numRet);