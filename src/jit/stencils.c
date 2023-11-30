#include <stdint.h>
#include "vm.h"

// clang: clang -c stencils.c -o stencils.o -I../ -O2
// clang-wasm: clang -c stencil.c -I../ -O2 --target=wasm32 -mtail-call -o stencils.o
// zig: zig cc -c stencils.c -fdouble-square-bracket-attributes -O2 -I../ -o stencils.o

Value hostFunc(VM* vm, const Value* args, u8 nargs);
void cont(Value* fp);
void cont2(VM* vm, Value* fp);
void cont3(VM* vm, Value* fp, u64 a);
void cont4(VM* vm, Value* fp, u64 a, u64 b);
void cont5(VM* vm, Value* fp, Value* args, u64 numArgs, Value res);
void interrupt(VM* vm, Value* fp);
void interrupt5(VM* vm, Value* fp, Value* args, u64 numArgs, Value res);
void branchTrue(Value* fp);
void branchFalse(Value* fp);

void addInt(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_INTEGER(VALUE_AS_INTEGER(left) + VALUE_AS_INTEGER(right)), right);
}

void subInt(VM* vm, Value* fp, Value left, Value right) {
    [[clang::musttail]] return cont4(vm, fp, VALUE_INTEGER(VALUE_AS_INTEGER(left) - VALUE_AS_INTEGER(right)), right);
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

void call(VM* vm, Value *fp) {
    [[clang::musttail]] return cont2(vm, fp + 5);
}

// TODO: Mark with GHC calling convention in LLVM so callee save registers aren't spilled.
void callHost(VM* vm, Value* fp, Value* args, u64 numArgs, Value res) {
    vm->stackPtr = fp;
    res = hostFunc(vm, args, (u8)numArgs);
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