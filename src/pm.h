#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

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

typedef struct CbTypeTable CbTypeTable;

typedef struct CbAny {
    u64 val;
    CbTypeTable* type;
} CbAny;
typedef u64 CbSymbol;
typedef u64 CbError;
typedef u8 CbNone;
typedef bool CbBool;
typedef u8 CbPlaceholder1;
typedef u8 CbPlaceholder2;
typedef u8 CbPlaceholder3;
typedef double CbFloat;
typedef i64 CbInt;
typedef CbAny CbDynamic;
typedef void* CbType;

typedef struct CbFiber {
} CbFiber;

typedef struct CbBuffer CbBuffer;

// This should match the builtin String.
typedef struct CbString {
    const char* ptr;
    u64 len;
    CbBuffer* buf;
    bool ascii;
} CbString;

typedef u8 CbTypeKind;
const CbTypeKind CbTypeObject = 0;
const CbTypeKind CbTypeStruct = 1;

typedef struct CbTypeTable {
    // Size in bytes.
    size_t size;
    const char* name;
    CbString (*toPrintString)(CbFiber*, CbAny);
    CbTypeKind kind;
} CbTypeTable;

typedef struct CbRT {
    u64 panicPayload;
    u8 panicType;
} CbRT;

// C macros.
#define BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)

#define CB_MAX_POOL_OBJECT_SIZE 32

// Convert to CbAny values.
#define BOX_INT(v) ((CbAny){ .val = v, .type = &CbIntTypeTable })
#define BOX_OBJ(t, v) ((CbAny){ .val = (u64)v, .type = &t ## TypeTable })

#define CB_NEW(t, ...) (new ##t(rt, (t)__VA_ARGS__))
#define CB_SYM(tag, len) (((u64)tag & 0xffffffffffff) | ((u64)len << 48))

#define TRY_PANIC(...) ({ CbAny tmp = __VA_ARGS__; (tmp == VALUE_INTERRUPT) ? cb_panic() : tmp; })
#define TRY_UNBOX_BOOL(...) ({ CbAny tmp = __VA_ARGS__; (tmp.type == &CbBoolTypeTable) ? *(bool*)&tmp.val : cb_panic(); })

CbAny cbCallMethDyn(CbRT* rt, CbAny rec, const char* name, size_t name_len, const CbAny* args, u8 nargs) {
    cb_panic_msg("TODO: cbCallMethDyn");
}

CbAny cbCallDyn(CbRT* rt, CbAny callee, const CbAny* args, u8 nargs) {
    cb_panic_msg("TODO: cbCallDyn");
}

void* cbNewPoolObj(CbRT* rt) {
    cb_panic_msg("TODO: cbNewPoolObj");
}

void* cbNewObj(CbRT* rt, size_t size) {
    cb_panic_msg("TODO: cbNewObj");
}

CbString cbUstr(CbRT* rt, const char* str, i64 len) {
    // return (CbString){ . }
}

CbString cbAstr(CbRT* rt, const char* str, i64 len) {
    // return (CbString){ . }
}
