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

typedef uint8_t Inst;
typedef uint64_t Value;

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

typedef struct VM {
    ZAllocator alloc;
    Inst* curPc;
    Value* curStack;
    Value* stackPtr;
    size_t stackLen;
    Value* stackEndPtr;
} VM;

typedef enum {
    EXEC_RESULT_SUCCESS,
} ExecResult;

typedef struct CallSymResult {
    Inst* pc;
    Value* stack;
} CallSymResult;

typedef Value (*FuncPtr)(VM* vm, Value* args, uint8_t nargs);

// C API.
ExecResult execBytecode(VM* vm);

// Calling into Zig.
void zFatal();
char* zOpCodeName(OpCode code);
CallSymResult zCallSym(VM* vm, Inst* pc, Value* stack, uint8_t symId, uint8_t startLocal, uint8_t numArgs, uint8_t reqNumRetVals);
void zDumpEvalOp(VM* vm, Inst* pc);
extern bool verbose;
void zFreeObject(VM* vm, HeapObject* obj);
void zEnd(VM* vm, Inst* pc);