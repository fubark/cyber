#include <stdint.h>
#include "vm.h"

// clang: clang -c stencils.c -o stencils.o -I../ -O2
// clang-wasm: clang -c stencil.c -I../ -O2 --target=wasm32 -mtail-call -o stencils.o
// zig: zig cc -c stencils.c -fdouble-square-bracket-attributes -O2 -I../ -o stencils.o

// For x86_64 only the linux target seems to omit the function call prelude:
// clang -c stencils.c -o stencils.o -I../ -O2 -target x86_64-linux 

Value hostFunc(VM* vm, const Value* args, u8 nargs);
void zDumpJitSection(VM* vm, Value* fp, u64 chunkId, u64 irIdx, u8* startPc, u8* endPc);
void cont(Value* fp);
void cont2(VM* vm, Value* fp);
void cont3(VM* vm, Value* fp, u64 a);
void cont4(VM* vm, Value* fp, u64 a, u64 b);
void cont5(VM* vm, Value* fp, u64 a, u64 b, u64 c);
void cont6(VM* vm, Value* fp, u64 a, u64 b, u64 c, u64 d);
void interrupt(VM* vm, Value* fp);
void interrupt4(VM* vm, Value* fp, u64 a, u64 b);
void interrupt5(VM* vm, Value* fp, u64 a, u64 b, u64 c);
void divByZero(VM* vm, Value* fp, u64 a, u64 b);
void branchTrue(Value* fp);
void branchFalse(Value* fp);

void addFloat(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_FLOAT(VALUE_AS_FLOAT(left) + VALUE_AS_FLOAT(right)), right);
}

void subFloat(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_FLOAT(VALUE_AS_FLOAT(left) - VALUE_AS_FLOAT(right)), right);
}

void mulFloat(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_FLOAT(VALUE_AS_FLOAT(left) * VALUE_AS_FLOAT(right)), right);
}

void divFloat(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_FLOAT(VALUE_AS_FLOAT(left) / VALUE_AS_FLOAT(right)), right);
}

void addInt(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_INTEGER(VALUE_AS_INTEGER(left) + VALUE_AS_INTEGER(right)), right);
}

void subInt(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_INTEGER(VALUE_AS_INTEGER(left) - VALUE_AS_INTEGER(right)), right);
}

void mulInt(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_INTEGER(VALUE_AS_INTEGER(left) * VALUE_AS_INTEGER(right)), right);
}

void divInt(VM* vm, Value* fp, Value left, Value right) {
    _BitInt(48) rightInt = VALUE_AS_INTEGER(right);
    if (rightInt == 0) {
        [[clang::musttail]] return divByZero(vm, fp, left, right);
    }
    [[clang::musttail]] return cont4(vm, fp, VALUE_INTEGER(VALUE_AS_INTEGER(left) / VALUE_AS_INTEGER(right)), right);
}

void lessInt(VM* vm, Value* fp, Value left, Value right) {
    // Result saved in `left` register.
    [[clang::musttail]] return cont4(vm, fp, VALUE_BOOLEAN(VALUE_AS_INTEGER(left) < VALUE_AS_INTEGER(right)), right);
}

void intPair(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_AS_INTEGER(left), VALUE_AS_INTEGER(right));
}

void isTrue(VM* vm, Value* fp, Value cond) {
    [[clang::musttail]] return cont3(vm, fp, VALUE_IS_BOOLEAN(cond) ? VALUE_AS_BOOLEAN(cond) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(cond));
}

void lessIntCFlag(VM* vm, Value* fp, Value left, Value right) {
    if ((int64_t)left < (int64_t)right) {
        [[clang::musttail]] return cont4(vm, fp, left, right);
    }
}

void call(VM* vm, Value* fp) {
    [[clang::musttail]] return cont2(vm, fp + CALL_ARG_START);
}

// TODO: Mark with GHC calling convention in LLVM so callee save registers aren't spilled.
void callHost(VM* vm, Value* fp, u64 args, u64 numArgs, Value res) {
    vm->stackPtr = fp;
    res = hostFunc(vm, BITCAST(Value*, args), (u8)numArgs);
    if (res == VALUE_INTERRUPT) {
        // return error.Panic;
        [[clang::musttail]] return interrupt5(vm, fp, args, numArgs, res);
    }
    [[clang::musttail]] return cont5(vm, fp, args, numArgs, res);
}

void end(VM* vm, Value* fp, u64 retSlot) {
    vm->endLocal = (u8)retSlot;
    // vm.curFiber.pcOffset = @intCast(getInstOffset(vm, pc + 2));
    [[clang::musttail]] return cont3(vm, fp, retSlot);
}

void stringTemplate(VM* vm, Value* fp, u64 strs, u64 exprs, u64 exprCount) {
    ValueResult res = zAllocStringTemplate2(vm, BITCAST(Value*, strs), exprCount+1, BITCAST(Value*, exprs), exprCount);
    if (res.code != RES_CODE_SUCCESS) {
        [[clang::musttail]] return interrupt5(vm, fp, strs, exprs, res.code);
    }
    [[clang::musttail]] return cont5(vm, fp, res.val, exprs, exprCount);
}

void dumpJitSection(VM* vm, Value* fp, u64 chunkId, u64 irIdx, u64 startPc, u64 endPc) {
    zDumpJitSection(vm, fp, chunkId, irIdx, BITCAST(u8*, startPc), BITCAST(u8*, endPc));
    [[clang::musttail]] return cont6(vm, fp, chunkId, irIdx, startPc, endPc);
}

void release(VM* vm, Value* fp, Value val) {
    if (VALUE_IS_POINTER(val)) {
        HeapObject* obj = VALUE_AS_HEAPOBJECT(val);
        obj->head.rc -= 1;
        if (obj->head.rc == 0) {
            zFreeObject(vm, obj);
        }
    }
    [[clang::musttail]] return cont3(vm, fp, val);
}

// void test(VM* vm, Value* fp, Value a, Value b) {
//     // [[clang::musttail]] return cont4(vm+1, fp, a, b);
// }