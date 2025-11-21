#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <execinfo.h>
#include <stdarg.h>
#include <inttypes.h>
#include <math.h>

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

#ifdef __TINYC__
#else
#endif

typedef struct CBI_TypeTable CBI_TypeTable;

// typedef u64 CB_Symbol;
// typedef u64 CB_Error;
typedef bool CB_bool;
typedef double CB_float;
typedef i64 CB_int;
typedef i64 CB_MetaType;
typedef struct CB_Thread CB_Thread;

enum {
    CBI_TYPE_NULL = 0,
    CBI_TYPE_VOID = 1,
    CBI_TYPE_BOOL = 2,
    CBI_TYPE_ERROR = 3,
    CBI_TYPE_BYTE = 4,
    CBI_TYPE_IR_EXPR = 5,
    CBI_TYPE_SYMBOL = 6,
    CBI_TYPE_INT = 7,
    CBI_TYPE_FLOAT = 8,
    CBI_TYPE_PLACEHOLDER7 = 9,
    CBI_TYPE_OBJECT = 10,
    CBI_TYPE_TYPE = 11,
    CBI_TYPE_PLACEHOLDER4 = 12,
    CBI_TYPE_PLACEHOLDER5 = 13,
    CBI_TYPE_PLACEHOLDER6 = 14,
    CBI_TYPE_GENERIC = 15,
    CBI_TYPE_CODE = 16,
    CBI_TYPE_PLACEHOLDER8 = 17,
    CBI_TYPE_FUNC_SIG = 18,
    CBI_TYPE_PLACEHOLDER2 = 19,
    CBI_TYPE_EXTERN_FUNC = 20,
    CBI_TYPE_STR = 21,
    CBI_TYPE_STR_BUFFER = 22,
    CBI_TYPE_FIBER = 23,
    CBI_TYPE_PLACEHOLDER1 = 24,
    CBI_TYPE_TCC_STATE = 25,
    CBI_TYPE_PLACEHOLDER3 = 26,
    CBI_TYPE_RANGE = 27,
    CBI_TYPE_TABLE = 28,
    CBI_TYPE_MEMORY = 29,
};

typedef struct CB_FuncUnion {
    u64 kind;
    void* ptr;
    u64 num_cap;
    void* captures[];
} CB_FuncUnion;

typedef struct CB_Fiber {
} CB_Fiber;

// typedef struct CB_Buffer CB_Buffer;

typedef struct CB_StrBuffer CB_StrBuffer;
typedef struct CB_str CB_str;
typedef void* CB_Object;

typedef u8 CBI_TypeKind;
enum {
    CBI_TypeObject = 0,
    CBI_TypeStruct = 1,
};

typedef void (*CBI_DeinitObjectFn)(CB_Thread*, void*);

typedef struct CBI_TypeTable {
    // Size in bytes.
    size_t size;
    const char* name;
    CBI_DeinitObjectFn deinitObject;
    void (*destruct)(CB_Thread*, void* ptr);
    CBI_TypeKind kind;
} CBI_TypeTable;

typedef struct CB_Thread {
    u64 panicPayload;
    u8 panicType;
    size_t trace_num_objs; 
} CB_Thread;

typedef struct CBI_Slice {
    void* ptr;
    size_t len;
} CBI_Slice;

#define CB_NumberFormat_asc 0
#define CB_NumberFormat_bin 1
#define CB_NumberFormat_dec 2
#define CB_NumberFormat_hex 3
#define CB_NumberFormat_oct 4

typedef struct CBI_ObjectHeader {
    u32 meta;
    u32 rc;
    u8 data[];
} CBI_ObjectHeader;

typedef struct CBI_Buffer {
    u64 len;
    u8 data[];
} CBI_Buffer;

typedef union CBI_HeapObject {
    CB_int int_;
    CBI_Buffer buffer;
    CB_FuncUnion func_union;
} CBI_HeapObject;

// C macros.
#define BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)
#define TRUNC_I64_I8(x) ((u8)(x >> 56) | (x & 0x7F))

#define CB_MAX_POOL_OBJECT_SIZE 32

CB_Thread cb_ctx;

const char* cbi_object_typename(CB_Thread* ctx, CB_Object o);
void* cbi_cast(CB_Thread* ctx, void* obj, u32 type);
void* cbi_retain(CB_Thread* ctx, void* obj);
void cbi_main_pre(CB_Thread* ctx);
void cbi_main_post(CB_Thread* ctx);
CB_Object cbi_main_user(CB_Thread* ctx);
void* cbi_closure(CB_Thread* ctx, CB_int type, void* ptr, void** captures, size_t num_captures);
void* cbi_func_union(CB_Thread* ctx, CB_int type, void* ptr);
void* cbi_unwrap_addr(CB_Thread* ctx, void* ptr, u64 tag);
void cbi_await(CB_Thread* ctx, void* val);
CB_str cbi_astr_static(CB_Thread* ctx, const char* str, CB_int len);
CB_str cbi_ustr_static(CB_Thread* ctx, const char* str, CB_int len);

void cb_eprint(CB_Thread* ctx, CB_Object obj);
void cb_panic(CB_Thread* ctx, CB_str msg);
void cbi_panic_fmt(const char *format, ...);
CB_str cb_astr(CB_Thread* ctx, const char* str, i64 len);
CB_str cb_ustr(CB_Thread* ctx, const char* str, i64 len);
void cb_release(CB_Thread* ctx, void* ptr);
void cb_release_opt(CB_Thread* ctx, void* obj);
CB_int cb_func_union_size(CB_Thread* ctx, void* ptr);
void cb_memcpy(CB_Thread* ctx, u8* dst, u8* src, i64 len);
CB_Object cb_box_int(CB_Thread* ctx, CB_int a);
CB_bool cb_cmp_str(CB_str a, CB_str b);
void* cbi_lift(CB_Thread* ctx, void* src, CB_int type, CB_int size);