#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

typedef uint8_t u8;
typedef int8_t i8;
typedef uint16_t u16;
typedef int16_t i16;
typedef uint32_t u32;
typedef int32_t i32;
typedef uint64_t u64;
typedef int64_t i64;

#ifdef __TINYC__
typedef int64_t i48;
#else
typedef _BitInt(48) i48;
#endif

typedef uint64_t Value;

typedef struct Str {
    const char* buf;
    size_t len;
} Str;

typedef struct Symbol {
    Str name;
} Symbol;

typedef struct PM PM;

typedef struct Fiber {
    PM* pm;
    u64 panicPayload;
    u8 panicType;
} Fiber;

typedef struct PM {
    Symbol* syms;
} PM;

// Macros.
#define VALUE_INTERRUPT (ERROR_MASK | 0xffff) 
#define BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)
#define TAGGED_VALUE_MASK ((u64)0x7ffc000000000000)
#define INTEGER_MASK ((u64)1 << 49)
#define TAG_ERROR ((uint8_t)2)
#define TAG_SYMBOL ((uint8_t)6)
#define ERROR_MASK (TAGGED_VALUE_MASK | ((u64)TAG_ERROR << 32))
#define SYMBOL_MASK (TAGGED_VALUE_MASK | ((u64)TAG_SYMBOL << 32))
#define TAGGED_INTEGER_MASK (TAGGED_VALUE_MASK | INTEGER_MASK)

#define STR_INIT(str, len_) (Str){ .buf = str, .len = len_ }

// Convert to NaN boxed values.
#define BOX_INT48(n) (TAGGED_INTEGER_MASK | BITCAST(unsigned _BitInt(48), n))
#define BOX_INT(n) (TAGGED_INTEGER_MASK | BITCAST(unsigned _BitInt(48), (_BitInt(48))n))
#define BOX_SYM(symId) (SYMBOL_MASK | symId)

#define TRY_PANIC(...) ({ Value tmp = __VA_ARGS__; (tmp == VALUE_INTERRUPT) ? cy_panic() : tmp; })

