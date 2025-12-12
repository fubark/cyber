#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

#ifndef CYBER_H
#define CYBER_H

#ifdef __cplusplus
extern "C" {
#endif
typedef struct CLVM CLVM;
typedef struct CLThread CLThread;
typedef struct CLHeap CLHeap;

typedef uint64_t CLValue;
typedef uint8_t CLRet;

enum {
    CL_RET_OK = 0,
    CL_RET_INTERRUPT = 1,
};

typedef struct CLValueSlice {
    const CLValue* ptr;
    size_t len;
} CLValueSlice;

typedef struct CLResolverParams CLResolverParams;
typedef struct CLUnit CLUnit;
typedef struct CLExprResult CLExprResult;
typedef struct CLNode CLNode;
typedef struct CLFuncSig CLFuncSig;
typedef struct CLFunc CLFunc;
typedef struct CLSym CLSym;

#define CL_NULLID UINT32_MAX

typedef int CLResultCode;

enum {
    CL_SUCCESS = 0,
    CL_AWAIT,
    CL_ERROR_COMPILE,
    CL_ERROR_PANIC,
    CL_ERROR_UNKNOWN,
};

enum {
    CL_TYPE_NULL = 0,
    CL_TYPE_VOID,
    CL_TYPE_BOOL,
    CL_TYPE_I8,
    CL_TYPE_I16,
    CL_TYPE_I32,
    CL_TYPE_I64,
    CL_TYPE_INT_LIT,
    CL_TYPE_R8,
    CL_TYPE_R16,
    CL_TYPE_R32,
    CL_TYPE_R64,
    CL_TYPE_F32,
    CL_TYPE_F64,
    CL_TYPE_ERROR,
    CL_TYPE_SYMBOL,
    CL_TYPE_OBJECT,
    CL_TYPE_ANY,
    CL_TYPE_TYPE,
    CL_TYPE_THREAD,
    CL_TYPE_CODE,
    CL_TYPE_FUNC_SIG,
    CL_TYPE_PARTIAL_STRUCT_LAYOUT,
    CL_TYPE_STR,
    CL_TYPE_STR_BUFFER,
    CL_TYPE_STR_LIT,
    CL_TYPE_NEVER,
    CL_TYPE_INFER,
    CL_TYPE_DEPENDENT,
    CL_TYPE_TCC_STATE,
    CL_TYPE_RANGE,
    CL_TYPE_TABLE,
    CL_TYPE_NO_COPY,
    CL_TYPE_MUT_STR,
};
typedef uint32_t CLTypeId;
typedef struct CLType CLType;
typedef struct CLTypeValue {
    CLType* type;
    CLValue value;
} CLTypeValue;

// Some API functions may require string slices rather than a null terminated string.
// Creating a CLBytes can be simplified with a macro:
// #define bytes(x) ((CLBytes){ x, strlen(x) })
// NOTE: Returned `CLBytes`s are not always null terminated.
typedef struct CLBytes {
    char* ptr;
    size_t len;
} CLBytes;

// Type layout of a Cyber `str` at runtime.
typedef struct CLstr {
    void* buf;
    uint8_t* ptr;
    uint64_t header;
} CLstr;

typedef struct CLSlice {
    void* buf;
    void* ptr;
    size_t len;
    size_t header;
} CLSlice;

// A host function is bound to a runtime function symbol declared with `#[bind]`.
typedef CLRet (*CLHostFn)(CLThread* t);

typedef struct CLConstEvalContext {
    CLFunc* func;
    CLValue* args;
    CLNode* node;
} CLConstEvalContext;

typedef CLTypeValue (*CLConstEvalFn)(CLUnit* unit, CLConstEvalContext* ctx);

typedef struct CLBuiltinContext {
    CLFunc* func;
    CLNode* args;
    CLNode* node;
} CLBuiltinContext;

typedef bool (*CLBuiltinFn)(CLUnit* unit, CLBuiltinContext* ctx, CLExprResult* res);
typedef CLTypeValue (*CLBuiltinEvalFn)(CLUnit* unit, CLBuiltinContext* ctx);

typedef struct CLResolverParams {
    /// Chunk that invoked the resolver.
    /// If `CL_NULLID`, then the invoker did not originate from another chunk module.
    uint32_t chunkId;

    /// The current URI.
    /// If it's the empty string, then the invoker did not originate from another chunk module.
    CLBytes curUri;

    /// The unresolved URI.
    CLBytes uri;

    // Buffer to write a dynamic the result to.
    char* buf;
    size_t bufLen;
} CLResolverParams;

// Given the current module's resolved URI and the "to be" imported module specifier,
// set `params.resUri`, `params.resUriLen`, and return true.
// If the result uri is dynamic, write to `params.buf` to build the uri and set `params.resUri` to `params.buf`.
// If the result uri is a shared reference, `params.buf` does not need to be used.
// Return false if the uri can not be resolved.
// Most embedders do not need a resolver and can rely on the default resolver which
// simply returns `params.spec` without any adjustments.
typedef bool (*CLResolverFn)(CLVM* vm, CLResolverParams params, size_t* res_uri_len);

typedef bool (*CLDlResolverFn)(CLVM* vm, CLBytes uri, CLBytes* out);

// Callback invoked after all symbols in the module's src are loaded.
// This could be used to inject symbols not declared in the module's src.
typedef void (*CLModuleOnLoadFn)(CLVM* vm, CLSym* mod);

// Callback invoked just before the module is destroyed.
// This could be used to cleanup (eg. release) injected symbols from `CLPostLoadModuleFn`,
typedef void (*CLModuleOnDestroyFn)(CLVM* vm, CLSym* mod);

typedef enum {
    // Invokes a runtime VM host function.
    CL_BIND_FUNC_VM,

    // Parameters are const evaluated and returns a const value.
    CL_BIND_FUNC_CONST_EVAL,

    // Matching and sema is done by a compiler hook.
    CL_BIND_FUNC_BUILTIN,

    // Compile the function with the declaration body.
    CL_BIND_FUNC_DECL,
} CLBindFuncKind;

typedef struct CLBindFunc {
    // CLHostFn or CLCtFn
    const void* ptr;
    // CLCtEvalFn
    const void* ptr2;
    CLBindFuncKind kind;
} CLBindFunc;

typedef struct CLBindGlobal {
    void* ptr;
} CLBindGlobal;

typedef enum {
    // Use the provided declaration with a predefined or generated type id.
    CL_BIND_TYPE_DECL,

    CL_BIND_TYPE_CREATE,
    CL_BIND_TYPE_ALIAS,
} CLBindTypeKind;

typedef CLType* (*CLCreateTypeFn)(CLVM* vm, CLSym* ctx_chunk, CLNode* decl);
typedef CLSym* (*CLResolveAliasFn)(CLVM* vm, CLSym* sym);

// Result given to Cyber when binding a `#[bind]` type.
typedef struct CLBindType {
    union {
        struct {
            // If `CL_NULLID`, a new type id is generated.
            CLTypeId type_id;
        } decl;
        struct {
            // Pointer to callback.
            CLCreateTypeFn create_fn;
        } create;     
        struct {
            // Pointer to callback.
            CLResolveAliasFn resolve_fn;
        } alias;     
    } data;
    // `CLBindTypeKind`.
    uint8_t type;
} CLBindType;

size_t cl_strlen(const char* s);

#define CL_BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)
#define CL_BYTES(str) ((CLBytes){ .ptr = str, .len = cl_strlen(str) })
// #define CL_CORE_TYPE(t) ((CLHostType){ .data = { .core_custom = { .type_id = t, .get_children = NULL, .finalizer = NULL }}, .type = CL_BIND_TYPE_CORE_CUSTOM })
// #define CL_CORE_TYPE_EXT(t, gc, f) ((CLHostType){ .data = { .core_custom = { .type_id = t, .get_children = gc, .finalizer = f }}, .type = CL_BIND_TYPE_CORE_CUSTOM })
// #define CL_CORE_TYPE_DECL(t) ((CLHostType){ .data = { .core_decl = { .type_id = t }}, .type = CL_BIND_TYPE_CORE_DECL })
#define CL_CUSTOM_TYPE(name, ot, gc, f) ((CLBindTypeEntry){ CL_STR(name), (CLBindType){ .data = { .custom = { .out_type_id = ot, .get_children = gc, .finalizer = f, .pre = false }}, .type = CL_BIND_TYPE_CUSTOM }})
#define CL_CUSTOM_PRE_TYPE(name, ot, gc, f) ((CLBindTypeEntry){ CL_STR(name), (CLBindType){ .data = { .custom = { .out_type_id = ot, .get_children = gc, .finalizer = f, .pre = true }}, .type = CL_BIND_TYPE_CUSTOM }})
#define CL_CREATE_TYPE(name, fn) ((CLBindTypeEntry){ name, (CLBindType){ .data = { .create = { .create_fn = fn }}, .type = CL_BIND_TYPE_CREATE }})
#define CL_BIND_FUNC(host_ptr) ((CLBindFunc){.kind = CL_BIND_FUNC_VM, .ptr = host_ptr})
#define CL_BIND_GLOBAL(val_ptr) ((CLBindGlobal){.ptr = val_ptr})

typedef void (*CLModuleBindFn)(CLVM* vm, CLSym* mod);

typedef struct CLLoaderResult {
    CLBytes src;

    // Whether `src` should be managed by the compiler (frees automatically). Defaults to false.
    bool manage_src;
} CLLoaderResult;

// Given the resolved import specifier of the module, return the module source code.
typedef bool (*CLModuleLoaderFn)(CLVM* vm, CLSym* mod, CLBytes resolved_uri, CLLoaderResult* res);

// Handler for printing. The builtin `print` would invoke this.
// The default behavior is a no-op.
typedef void (*CLPrintFn)(CLThread* t, CLBytes str);

// Handler for printing errors.
// The default behavior is a no-op.
typedef void (*CLPrintErrorFn)(CLThread* t, CLBytes str);

// Handler for compiler logs as well as `core.log`.
// The default behavior is a no-op.
typedef void (*CLLogFn)(CLVM*, CLBytes str);

typedef void (*CLGlobalLogFn)(CLBytes str);

// Deprecated: temporary used during the transition to VM logger.
extern CLGlobalLogFn cl_logger;

// Whether library was compiled with `Trace`.
extern bool CL_TRACE;

typedef enum {
    CL_VM = 0,
    CL_JIT,
    CL_TCC,
    CL_CC,
    CL_LLVM,
} CLBackend;

typedef struct CLEvalConfig {
    /// Compiler options.
    bool single_run;
    bool gen_all_debug_syms;
    CLBackend backend;

    bool spawn_exe;
    
    /// Persist main locals / use imports into the VM env. For REPL-like behavior.
    bool persist_main;
} CLEvalConfig;

typedef struct CLEvalResult {
    CLValue* res;
    CLTypeId res_t;
} CLEvalResult;

typedef struct CLCompileConfig {
    /// Whether this process intends to perform eval once and exit.
    /// In that scenario, the compiler can skip generating the final release ops for the main block.
    bool single_run;

    bool skip_codegen;

    /// By default, debug syms are only generated for insts that can potentially fail.
    bool gen_all_debug_syms;

    CLBackend backend;

    bool emit_source_map;

    /// Persist main locals / use imports into the VM env. For REPL-like behavior.
    bool persist_main;
} CLCompileConfig;

typedef struct CLValidateConfig {
    /// Compiler options.
    bool stub;
} CLValidateConfig;

typedef struct CLAllocator {
    void* ptr;
    const void* vtable;
} CLAllocator;

typedef struct CLFieldInit {
    CLBytes name;
    CLValue value;
} CLFieldInit;

// -----------------------------------
// [ Top level ]
// -----------------------------------
CLBytes cl_full_version(void);
CLBytes cl_version(void);
CLBytes cl_build(void);
CLBytes cl_commit(void);
CLBytes clResultName(CLResultCode code);

// -----------------------------------
// [ VM ]
// -----------------------------------

// Create a new VM with `malloc` as the allocator.
CLVM* cl_vm_init(void);

CLVM* cl_vm_initx(CLAllocator allocator);

// Deinitializes the VM and frees all memory associated with it. Any operation on `vm` afterwards is undefined.
void cl_vm_deinit(CLVM* vm);

CLAllocator cl_vm_allocator(CLVM* vm);
CLThread* cl_vm_main_thread(CLVM* vm);

CLResolverFn cl_vm_resolver(CLVM* vm);
void cl_vm_set_resolver(CLVM* vm, CLResolverFn resolver);
CLBytes cl_vm_resolve(CLVM* vm, CLBytes uri);

// This is only relevant for binding dynamic libs.
void cl_vm_set_dl_resolver(CLVM* vm, CLDlResolverFn resolver);

// The default module resolver. It returns `spec`.
bool cl_default_resolver(CLVM* vm, CLResolverParams params, size_t* res_uri_len);

CLModuleLoaderFn cl_vm_get_loader(CLVM* vm);
void cl_vm_set_loader(CLVM* vm, CLModuleLoaderFn loader);

// The default module loader. It knows how to load the `builtins` module.
bool cl_default_loader(CLVM* vm, CLSym* mod, CLBytes resolved_uri, CLLoaderResult* res);

CLPrintFn cl_vm_printer(CLVM* vm);
void cl_vm_set_printer(CLVM* vm, CLPrintFn print);
CLPrintErrorFn cl_vm_eprinter(CLVM* vm);
void cl_vm_set_eprinter(CLVM* vm, CLPrintErrorFn print);
CLLogFn cl_vm_logger(CLVM* vm);
void cl_vm_set_logger(CLVM* vm, CLLogFn log);

CLEvalConfig clDefaultEvalConfig(void);
CLCompileConfig clDefaultCompileConfig(void);

// Resets the compiler and runtime state.
void cl_vm_reset(CLVM* vm);

// Evalutes the source code and returns the result code.
// Subsequent evals will reuse the same compiler and runtime state.
// If the last statement of the script is an expression, `outVal` will contain the value.
CLResultCode cl_vm_eval(CLVM* vm, CLBytes src, CLEvalResult* res);

// Accepts a `uri` and EvalConfig.
CLResultCode cl_vm_evalx(CLVM* vm, CLBytes uri, CLBytes src, CLEvalConfig config, CLEvalResult* res);

// Uses a `uri` to load the source code.
CLResultCode cl_vm_eval_path(CLVM* vm, CLBytes uri, CLEvalConfig config, CLEvalResult* res);

// Consumes and evaluates all ready tasks. Returns `CL_SUCCESS` if succesfully emptied the ready queue.
// CLResultCode cl_run_ready_tasks(CLThread* t);

CLResultCode cl_vm_compile(CLVM* vm, CLBytes uri, CLBytes src, CLCompileConfig config);
CLResultCode cl_vm_compile_path(CLVM* vm, CLBytes uri, CLCompileConfig config);
CLResultCode cl_vm_validate(CLVM* vm, CLBytes src);

/// Convenience function to return the last error summary.
/// Returns `cl_new_compile_error_summary` if the last result was a CL_ERROR_COMPILE,
/// or `cl_thread_panic_summary` if the last result was a CL_ERROR_PANIC,
/// or the null string.
CLBytes cl_vm_error_summary(CLVM* vm);

/// Returns first compile-time error summary. Must be freed with `cl_free`.
CLBytes cl_vm_compile_error_summary(CLVM* vm);

// For embedded, Cyber by default uses malloc (it can be configured to use the high-perf mimalloc).
// If the host uses a different allocator than Cyber, use `clAlloc` to allocate memory
// that is handed over to Cyber so it knows how to free it.
// This is also used to manage accessible buffers when embedding WASM.
// Only pointer is returned so wasm callsite can just receive the return value.
void* cl_vm_alloc(CLVM* vm, size_t size);
CLBytes cl_vm_allocb(CLVM* vm, size_t size);

// When using the Zig allocator, you'll need to pass the original memory size.
// For all other allocators, use 1 for `len`.
void cl_vm_free(CLVM* vm, void* ptr, size_t size);
void cl_vm_freeb(CLVM* vm, CLBytes bytes);
void cl_vm_freez(CLVM* vm, const char* str);

/// Some API callbacks use this to report errors.
void clReportApiError(CLVM* vm, CLBytes msg);

// Attach a userdata pointer inside the VM.
void* cl_vm_user_data(CLVM* vm);
void cl_vm_set_user_data(CLVM* vm, void* data);

void cl_vm_dump_bytecode(CLVM* vm);

// Verbose flag. In a debug build, this would print more logs.
extern bool clVerbose;

// Silent flag. Whether to print errors.
extern bool clSilent;

// -----------------------------------
// [ Threads ]
// -----------------------------------

CLVM* cl_thread_vm(CLThread* t);
CLAllocator cl_thread_allocator(CLThread* t);

typedef struct CLStackFrame {
    // Name identifier (e.g. function name, or "main")
    CLBytes name;
    // Starts at 0.
    uint32_t line;
    // Starts at 0.
    uint32_t col;
    // Where the line starts in the source file.
    uint32_t line_pos;
    uint32_t chunk;
} CLStackFrame;

typedef struct CLStackTrace {
    CLStackFrame* frames;
    size_t frames_len;
} CLStackTrace;

CLStackTrace cl_thread_panic_trace(CLThread* t);

/// Returns runtime panic summary. Must be freed with `cl_free`.
CLBytes cl_thread_panic_summary(CLThread* t);

/// Return and parameters for host functions.
void* cl_thread_ret(CLThread* t, size_t size);
CLRet cl_thread_ret_panic(CLThread* t, CLBytes msg);
void* cl_thread_param(CLThread* t, size_t size);
CLstr cl_thread_str(CLThread* t);
void* cl_thread_ptr(CLThread* t);
double cl_thread_float(CLThread* t);
float cl_thread_f32(CLThread* t);
int64_t cl_thread_int(CLThread* t);
int32_t cl_thread_i32(CLThread* t);
int16_t cl_thread_i16(CLThread* t);
int8_t cl_thread_i8(CLThread* t);
uint64_t cl_thread_r64(CLThread* t);
uint32_t cl_thread_r32(CLThread* t);
uint16_t cl_thread_r16(CLThread* t);
uint8_t cl_thread_byte(CLThread* t);
CLSlice cl_thread_slice(CLThread* t);

void cl_thread_release(CLThread* t, CLValue val);
void cl_thread_retain(CLThread* t, CLValue val);

// Get's the current global reference count.
// NOTE: This will panic if the lib was not built with `Trace`.
// RELATED: `cl_thread_count_objects()`
size_t cl_thread_rc(CLThread* t);

// Logs eval stats about the thread.
void cl_thread_dump_stats(CLThread* t);

// Returns the number of live objects.
// This can be used to check if all objects were cleaned up after `clDeinit`.
size_t cl_thread_count_objects(CLThread* t);

// TRACE mode: Dump live objects recorded in global object map.
void cl_thread_dump_live_objects(CLThread* t);

void cl_thread_signal_host_panic(CLThread* t);
void cl_thread_signal_host_segfault(CLThread* t);

typedef struct CLMemoryCheck {
    uint32_t num_cyc_objects;
} CLMemoryCheck;

// Check memory for reference cycles which indicates a bug in the program.
CLMemoryCheck cl_thread_check_memory(CLThread* t);

typedef struct CLTraceInfo {
    uint32_t num_retains;
    uint32_t num_releases;
} CLTraceInfo;

CLTraceInfo cl_thread_trace_info(CLThread* t);

// -----------------------------------
// [ Modules ]
// -----------------------------------

void cl_mod_set_data(CLSym* mod, void* data);
void* cl_mod_get_data(CLSym* mod);
void cl_mod_add_func(CLSym* mod, CLBytes name, CLBindFunc binding);
void cl_mod_add_type(CLSym* mod, CLBytes name, CLBindType binding);
void cl_mod_add_global(CLSym* mod, CLBytes name, CLBindGlobal binding);
void cl_mod_on_destroy(CLSym* mod, CLModuleOnDestroyFn on_destroy);
void cl_mod_on_load(CLSym* mod, CLModuleOnLoadFn on_load);

void cl_mod_bind_core(CLVM* vm, CLSym* mod);
void cl_mod_bind_cy(CLVM* vm, CLSym* mod);
void cl_mod_bind_c(CLVM* vm, CLSym* mod);
void cl_mod_bind_io(CLVM* vm, CLSym* mod);
void cl_mod_bind_meta(CLVM* vm, CLSym* mod);
void cl_mod_bind_math(CLVM* vm, CLSym* mod);
void cl_mod_bind_test(CLVM* vm, CLSym* mod);

// -----------------------------------
// [ Symbols ]
// -----------------------------------

// Expand type template for given arguments.
CLType* clExpandTypeTemplate(CLVM* vm, CLSym* type_t, const CLType* param_types, const CLValue* args, size_t nargs);

// Find and return the type from a given type specifier.
// Returns `NULL` if the symbol could not be found or the symbol is not a type.
// Returns `NULL` if trying to resolve a template expansion that doesn't already exist.
// NOTE: Currently this behaves like `clResolveType`.
// If a type is returned, the type specifier is memoized to return the same result.
CLType* cl_find_type(CLVM* vm, CLBytes spec);

// Similar to `clFindType` and also resolves any missing template expansions.
CLType* clResolveType(CLVM* vm, CLBytes spec);

// -----------------------------------
// [ Values ]
// -----------------------------------

// Returns a short description about a value given their type id.
// Specializes for common types. Other types will only output the type id and the address.
// This is very basic compared to the core API's `to_print_string` and `dump`.
CLBytes cl_value_desc(CLVM* vm, CLTypeId val_t, CLValue* val);

// Symbol value.
int64_t cl_symbol(CLVM* vm, CLBytes str);

// `cl_str` is the recommended way to initialize a new string.
// `cl_astr` or `cl_ustr` asserts an ASCII or UTF-8 string respectively.
CLstr cl_str_init(CLThread* t, CLBytes str);
CLstr cl_astr_init(CLThread* t, CLBytes str);
CLstr cl_ustr_init(CLThread* t, CLBytes str);
void cl_str_deinit(CLThread* t, CLstr* str);
CLBytes cl_str_bytes(CLstr str);
CLBytes cl_object_string(CLThread* t, CLValue val);  // Conversion from value to a basic string description.

// Initializes a new slice with undefined elements.
CLSlice cl_slice_init(CLThread* t, CLTypeId byte_buffer_t, size_t num_elems, size_t elem_size);

// Functions.
CLFuncSig* cl_vm_ensure_func_sig(CLVM* vm, const CLType* params, size_t nparams, CLType* ret_t);
CLValue cl_new_func_union(CLThread* t, CLType* union_t, CLHostFn func);

// Types.
CLType* clGetType(CLValue val);
CLTypeId clTypeId(CLType* type);

// Consumes and lifts a value (given as a pointer to the value) to the heap.
CLValue cl_lift(CLThread* t, CLType* type, void* value);

// Creates an empty array value.
// Expects an array type `[]T` as `array_t` which can be obtained from `clExpandTypeTemplate` or `clResolveType`.
CLValue cl_array_empty_new(CLVM* vm, CLType* array_t);

CLValue cl_map_empty_new(CLThread* t, CLType* map_t);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif // !CYBER_H