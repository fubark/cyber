#include <stdint.h>
#include "vm.h"

// Avoiding compound literal to suggest compiler not to use constant pool.
#define RETURN(pc_, code_) \
    do { \
        PcFpResult res; \
        res.pc = pc_; \
        res.fp = fp; \
        res.code = code_; \
        return res; \
    } while (false)

// clang: clang -c stencils.c -o stencils.o -I../ -O2
// clang-wasm: clang -c stencil.c -I../ -O2 --target=wasm32 -mtail-call -o stencils.o
// zig: zig cc -c stencils.c -fdouble-square-bracket-attributes -O2 -I../ -o stencils.o

// For x86_64 only the linux target seems to omit the function call prelude:
// clang -c stencils.c -o stencils.o -I../ -O2 -target x86_64-linux 

Value hostFunc(VM* vm, const Value* args, u8 nargs);
void zDumpJitSection(VM* vm, Value* fp, u64 chunkId, u64 irIdx, u8* startPc, u8* endPc);
void cont(Value* fp);
PcFpResult cont2(ZThread* t, Value* fp) __attribute__((preserve_none));
PcFpResult cont3(ZThread* t, Value* fp, u64 a) __attribute__((preserve_none));
PcFpResult cont4(ZThread* t, Value* fp, u64 a, u64 b) __attribute__((preserve_none));
PcFpResult cont5(ZThread* t, Value* fp, u64 a, u64 b, u64 c) __attribute__((preserve_none));
void cont6(VM* vm, Value* fp, u64 a, u64 b, u64 c, u64 d);
PcFpResult br3(ZThread* t, Value* fp, u64 a) __attribute__((preserve_none));
PcFpResult br4(ZThread* t, Value* fp, u64 a, u64 b) __attribute__((preserve_none));
void interrupt(VM* vm, Value* fp);
void interrupt4(VM* vm, Value* fp, u64 a, u64 b);
void interrupt5(VM* vm, Value* fp, u64 a, u64 b, u64 c);
PcFpResult div_by_zero(ZThread* t, Value* fp, u64 a, u64 b, u64 c) __attribute__((preserve_none));
void branchTrue(Value* fp);
void branchFalse(Value* fp);

// Suggest `cond` is expected to be true to generate continuation branch last.
#define GUARD_COND(cond) __builtin_expect(cond, true)

PcFpResult pre_ret(ZThread* t, Value* fp) __attribute__((preserve_none)) {
    u8 frame_t = (fp[1] >> 16) & 0xff;

    // Restore x30 when returning to VM or JIT caller. 
    __asm__ volatile (
        "mov x30, %0"
        :                 
        : "r" (fp[2])
        : 
    );
    fp = (Value*)fp[3];
    if (GUARD_COND(frame_t != FRAME_JIT)) {
        RETURN(NULL, RES_SUCCESS);
    }
    [[clang::musttail]] return cont2(t, fp);
}

PcFpResult mov(ZThread* t, Value* fp, u64 dst, u64 src) __attribute__((preserve_none)) {
    fp[dst] = fp[src];
    [[clang::musttail]] return cont4(t, fp, dst, src);
}

PcFpResult lt(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    i64 left_i = BITCAST(i64, left);
    i64 right_i = BITCAST(i64, right);
    fp[dst] = left_i < right_i;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult le(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    i64 left_i = BITCAST(i64, left);
    i64 right_i = BITCAST(i64, right);
    fp[dst] = left_i <= right_i;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult gt(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    i64 left_i = BITCAST(i64, left);
    i64 right_i = BITCAST(i64, right);
    fp[dst] = left_i > right_i;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult ge(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    i64 left_i = BITCAST(i64, left);
    i64 right_i = BITCAST(i64, right);
    fp[dst] = left_i >= right_i;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult jump_f(ZThread* t, Value* fp, u64 cond) __attribute__((preserve_none)) {
    if (GUARD_COND(!*(bool*)&fp[cond])) {
        [[clang::musttail]] return br3(t, fp, cond);
    }
    [[clang::musttail]] return cont3(t, fp, cond);
}

PcFpResult store_const(ZThread* t, Value* fp, u64 dst, u64 value) __attribute__((preserve_none)) {
    fp[dst] = value;
    [[clang::musttail]] return cont4(t, fp, dst, value);
}

PcFpResult chk_stk(ZThread* t, Value* fp, u64 ret_size, u64 frame_size) __attribute__((preserve_none)) {
    fp -= ret_size;
    if (GUARD_COND(fp + frame_size >= t->c.stack_end)) {
        RETURN(NULL, RES_STACK_OVERFLOW);
    }
    [[clang::musttail]] return cont4(t, fp, ret_size, frame_size);
}

PcFpResult fadd(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = VALUE_FLOAT(VALUE_AS_FLOAT(left) + VALUE_AS_FLOAT(right));
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult fsub(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = VALUE_FLOAT(VALUE_AS_FLOAT(left) - VALUE_AS_FLOAT(right));
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult fmul(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = VALUE_FLOAT(VALUE_AS_FLOAT(left) * VALUE_AS_FLOAT(right));
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult fdiv(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = VALUE_FLOAT(VALUE_AS_FLOAT(left) / VALUE_AS_FLOAT(right));
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult add(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = left + right;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult sub(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = left - right;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult mul(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    fp[dst] = left * right;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

PcFpResult div(ZThread* t, Value* fp, u64 dst, u64 left, u64 right) __attribute__((preserve_none)) {
    if (GUARD_COND(right == 0)) {
        [[clang::musttail]] return div_by_zero(t, fp, dst, left, right);
    }
    fp[dst] = left / right;
    [[clang::musttail]] return cont5(t, fp, dst, left, right);
}

#define JIT_CALLINFO(callInstOff, stack_size) ((Value)(((u32)callInstOff << 1) | ((u32)stack_size << 8)) | ((u32)FRAME_JIT << 16))

// Prologue for VM/JIT -> JIT.
PcFpResult func_prologue(ZThread* t, Value* fp) __attribute__((preserve_none)) {
    // Persist return addr reg into stack.
#if defined(__aarch64__)
    __asm__ volatile (
        "str x30, [%0]"
        :
        : "r" (fp + 1)
        : "memory"
    );
#else
#endif
    [[clang::musttail]] return cont2(t, fp);
}

// Omits the actual branch call inst.
PcFpResult pre_call(ZThread* t, Value* fp, u64 base) __attribute__((preserve_none)) {
    uintptr_t ret_fp = (uintptr_t)fp;
    fp += base;
    fp[0] = JIT_CALLINFO(0, 0);
    // return addr saved in callee `func_prologue`.
    fp[2] = ret_fp;
    [[clang::musttail]] return cont3(t, fp, base);
}

PcFpResult jit_log(ZThread* t, Value* fp, u64 msg, u64 msg_len) __attribute__((preserve_none)) {
    z_log(t, (const char*)msg, msg_len);
    [[clang::musttail]] return cont4(t, fp, msg, msg_len);
}

PcFpResult jump_ge(ZThread* t, Value* fp, u64 left, u64 right) __attribute__((preserve_none)) {
    if (GUARD_COND(BITCAST(i64, left) < BITCAST(i64, right))) {
        [[clang::musttail]] return br4(t, fp, left, right);
    }
    [[clang::musttail]] return cont4(t, fp, left, right);
}

PcFpResult jump_gt(ZThread* t, Value* fp, u64 left, u64 right) __attribute__((preserve_none)) {
    if (GUARD_COND(BITCAST(i64, left) <= BITCAST(i64, right))) {
        [[clang::musttail]] return br4(t, fp, left, right);
    }
    [[clang::musttail]] return cont4(t, fp, left, right);
}

PcFpResult jump_le(ZThread* t, Value* fp, u64 left, u64 right) __attribute__((preserve_none)) {
    if (GUARD_COND(BITCAST(i64, left) > BITCAST(i64, right))) {
        [[clang::musttail]] return br4(t, fp, left, right);
    }
    [[clang::musttail]] return cont4(t, fp, left, right);
}

PcFpResult jump_lt(ZThread* t, Value* fp, u64 left, u64 right) __attribute__((preserve_none)) {
    if (GUARD_COND(BITCAST(i64, left) >= BITCAST(i64, right))) {
        [[clang::musttail]] return br4(t, fp, left, right);
    }
    [[clang::musttail]] return cont4(t, fp, left, right);
}

// void isTrue(VM* vm, Value* fp, Value cond) {
//     [[clang::musttail]] return cont3(vm, fp, VALUE_IS_BOOLEAN(cond) ? VALUE_AS_BOOLEAN(cond) : VALUE_ASSUME_NOT_BOOL_TO_BOOL(cond));
// }

// void lessIntCFlag(VM* vm, Value* fp, Value left, Value right) {
//     if ((int64_t)left < (int64_t)right) {
//         [[clang::musttail]] return cont4(vm, fp, left, right);
//     }
// }

// // TODO: Mark with GHC calling convention in LLVM so callee save registers aren't spilled.
// void callHost(VM* vm, Value* fp, u64 args, u64 numArgs, Value res) {
//     vm->stackPtr = fp;
//     res = hostFunc(vm, BITCAST(Value*, args), (u8)numArgs);
//     if (res == VALUE_INTERRUPT) {
//         // return error.Panic;
//         [[clang::musttail]] return interrupt5(vm, fp, args, numArgs, res);
//     }
//     [[clang::musttail]] return cont5(vm, fp, args, numArgs, res);
// }

// void end(VM* vm, Value* fp, u64 retSlot) {
//     vm->endLocal = (u8)retSlot;
//     // vm.curFiber.pcOffset = @intCast(getInstOffset(vm, pc + 2));
//     [[clang::musttail]] return cont3(vm, fp, retSlot);
// }

// void dumpJitSection(VM* vm, Value* fp, u64 chunkId, u64 irIdx, u64 startPc, u64 endPc) {
//     zDumpJitSection(vm, fp, chunkId, irIdx, BITCAST(u8*, startPc), BITCAST(u8*, endPc));
//     [[clang::musttail]] return cont6(vm, fp, chunkId, irIdx, startPc, endPc);
// }

// void release(VM* vm, Value* fp, Value val) {
//     if (VALUE_IS_POINTER(val)) {
//         HeapObject* obj = VALUE_AS_HEAPOBJECT(val);
//         obj->head.rc -= 1;
//         if (obj->head.rc == 0) {
//             zFreeObject(vm, obj);
//         }
//     }
//     [[clang::musttail]] return cont3(vm, fp, val);
// }