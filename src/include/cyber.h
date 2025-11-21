#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

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
#define CL_VOID 0x7FFC000100000000
#define CL_TRUE 0x7FFC000200000001
#define CL_FALSE 0x7FFC000200000000
#define CL_INTERRUPT 0x7ffc00030000ffff

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
    CL_TYPE_ERROR,
    CL_TYPE_BYTE,
    CL_TYPE_TAGLIT,
    CL_TYPE_SYMBOL,
    CL_TYPE_INT,
    CL_TYPE_FLOAT,
    CL_TYPE_PLACEHOLDER7,
    CL_TYPE_ANY,
    CL_TYPE_TYPE,
    CL_TYPE_PLACEHOLDER4,
    CL_TYPE_PLACEHOLDER5,
    CL_TYPE_PLACEHOLDER6,
    CL_TYPE_GENERIC,
    CL_TYPE_CODE,
    CL_TYPE_FUNC,
    CL_TYPE_FUNC_SIG,
    CL_TYPE_PLACEHOLDER2,
    CL_TYPE_EXTERN_FUNC,
    CL_TYPE_STR,
    CL_TYPE_STRING_BUFFER,
    CL_TYPE_FIBER,
    CL_TYPE_PLACEHOLDER1,
    CL_TYPE_TCCSTATE,
    CL_TYPE_PLACEHOLDER3,
    CL_TYPE_RANGE,
    CL_TYPE_TABLE,
    CL_TYPE_MEMORY,
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
    const char* ptr;
    size_t len;
} CLBytes;

// Type layout of a Cyber `str` at runtime.
typedef struct CL_str {
    void* buf;
    const char* ptr;
    uint64_t header;
} CL_str;

// A host function is bound to a runtime function symbol declared with `#[bind]`.
typedef CLRet (*CLHostFn)(CLThread* t);

typedef struct CLSemaFuncContext {
    CLFunc* func;
    size_t expr_start;
    CLNode* node;
} CLSemaFuncContext;

typedef bool (*CLSemaFn)(CLUnit* unit, CLSemaFuncContext* ctx, CLExprResult* res);

typedef struct CLCtFuncContext {
    CLFunc* func;
    CLValue* args;
    CLNode* node;
} CLCtFuncContext;

typedef bool (*CLCtFn)(CLUnit* unit, CLCtFuncContext* ctx, CLExprResult* res);
typedef CLTypeValue (*CLCtEvalFn)(CLUnit* unit, CLCtFuncContext* ctx);

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
} LResolverParams;

// Given the current module's resolved URI and the "to be" imported module specifier,
// set `params.resUri`, `params.resUriLen`, and return true.
// If the result uri is dynamic, write to `params.buf` to build the uri and set `params.resUri` to `params.buf`.
// If the result uri is a shared reference, `params.buf` does not need to be used.
// Return false if the uri can not be resolved.
// Most embedders do not need a resolver and can rely on the default resolver which
// simply returns `params.spec` without any adjustments.
typedef bool (*CLResolverFn)(CLVM* vm, CLResolverParams params, size_t* res_uri_len);

// Callback invoked after all symbols in the module's src are loaded.
// This could be used to inject symbols not declared in the module's src.
typedef void (*CLModuleOnLoadFn)(CLVM* vm, CLSym* mod);

// Callback invoked just before the module is destroyed.
// This could be used to cleanup (eg. release) injected symbols from `CLPostLoadModuleFn`,
typedef void (*CLModuleOnDestroyFn)(CLVM* vm, CLSym* mod);

typedef enum {
    CL_BIND_FUNC_VM,
    CL_BIND_FUNC_CT,
    CL_BIND_FUNC_SEMA,

    // Compile the function with the declaration body.
    CL_BIND_FUNC_DECL,
} CLBindFuncKind;

typedef struct CLBindFunc {
    // CLHostFn or CLCtFn
    void* ptr;
    // CLCtEvalFn
    void* ptr2;
    CLBindFuncKind kind;
} CLBindFunc;

// Returns a compile-time value.
// The value is consumed by the module. 
typedef CLValue (*CLBindGlobal)(CLVM* vm);

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

#define CL_BITCAST(type, x) (((union {typeof(x) src; type dst;})(x)).dst)
#define CL_BYTES(str) ((CLBytes){ .ptr = str, .len = strlen(str) })
// #define CL_CORE_TYPE(t) ((CLHostType){ .data = { .core_custom = { .type_id = t, .get_children = NULL, .finalizer = NULL }}, .type = CL_BIND_TYPE_CORE_CUSTOM })
// #define CL_CORE_TYPE_EXT(t, gc, f) ((CLHostType){ .data = { .core_custom = { .type_id = t, .get_children = gc, .finalizer = f }}, .type = CL_BIND_TYPE_CORE_CUSTOM })
// #define CL_CORE_TYPE_DECL(t) ((CLHostType){ .data = { .core_decl = { .type_id = t }}, .type = CL_BIND_TYPE_CORE_DECL })
#define CL_CUSTOM_TYPE(name, ot, gc, f) ((CLBindTypeEntry){ CL_STR(name), (CLBindType){ .data = { .custom = { .out_type_id = ot, .get_children = gc, .finalizer = f, .pre = false }}, .type = CL_BIND_TYPE_CUSTOM }})
#define CL_CUSTOM_PRE_TYPE(name, ot, gc, f) ((CLBindTypeEntry){ CL_STR(name), (CLBindType){ .data = { .custom = { .out_type_id = ot, .get_children = gc, .finalizer = f, .pre = true }}, .type = CL_BIND_TYPE_CUSTOM }})
#define CL_CREATE_TYPE(name, fn) ((CLBindTypeEntry){ name, (CLBindType){ .data = { .create = { .create_fn = fn }}, .type = CL_BIND_TYPE_CREATE }})
#define CL_BIND_FUNC(host_ptr) ((CLBindFunc){.kind = CL_BIND_FUNC_VM, .ptr = host_ptr})

typedef void (*CLModuleBindFn)(CLVM* vm, CLSym* mod);

// Given the resolved import specifier of the module, return the module source code.
typedef bool (*CLModuleLoaderFn)(CLVM* vm, CLSym* mod, CLBytes resolved_uri, CLBytes* out_src);

// Handler for printing. The builtin `print` would invoke this.
// The default behavior is a no-op.
typedef void (*CLPrintFn)(CLThread* t, CLBytes str);

// Handler for printing errors.
// The default behavior is a no-op.
typedef void (*CLPrintErrorFn)(CLThread* t, CLBytes str);

// Handler for compiler and runtime logs.
// The default behavior is a no-op.
typedef void (*CLLogFn)(CLVM*, CLBytes str);

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
    bool file_modules;
    bool gen_all_debug_syms;
    CLBackend backend;

    /// Whether url imports and cached assets should be reloaded.
    /// TODO: This should be part of CLI config.
    bool reload;

    bool spawn_exe;
    
    /// Persist main locals into the VM env. For REPL-like behavior.
    bool persist_main_locals;
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

    bool file_modules;

    /// By default, debug syms are only generated for insts that can potentially fail.
    bool gen_all_debug_syms;

    CLBackend backend;

    bool emit_source_map;

    /// Persist main locals into the VM env. For REPL-like behavior.
    bool persist_main_locals;
} CLCompileConfig;

typedef struct CLValidateConfig {
    /// Compiler options.
    bool file_modules;
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
CLBytes clGetFullVersion(void);
CLBytes clGetVersion(void);
CLBytes clGetBuild(void);
CLBytes clGetCommit(void);
CLBytes clResultName(CLResultCode code);

// -----------------------------------
// [ VM ]
// -----------------------------------

// Create a new VM.
CLVM* cl_vm_init(void);

// Deinitializes the VM and frees all memory associated with it. Any operation on `vm` afterwards is undefined.
void cl_vm_deinit(CLVM* vm);

CLThread* cl_vm_main_thread(CLVM* vm);

CLResolverFn cl_get_resolver(CLVM* vm);
void cl_set_resolver(CLVM* vm, CLResolverFn resolver);
CLBytes cl_resolve(CLVM* vm, CLBytes uri);

// The default module resolver. It returns `spec`.
bool cl_default_resolver(CLVM* vm, CLResolverParams params, size_t* res_uri_len);

CLModuleLoaderFn cl_vm_get_loader(CLVM* vm);
void cl_vm_set_loader(CLVM* vm, CLModuleLoaderFn loader);

// The default module loader. It knows how to load the `builtins` module.
bool cl_default_loader(CLVM* vm, CLSym* mod, CLBytes resolved_uri, CLBytes* out);

CLPrintFn cl_vm_printer(CLVM* vm);
void cl_vm_set_printer(CLVM* vm, CLPrintFn print);
CLPrintErrorFn cl_vm_eprinter(CLVM* vm);
void cl_vm_set_eprinter(CLVM* vm, CLPrintErrorFn print);
void cl_vm_set_logger(CLVM* vm, CLLogFn log);

CLEvalConfig clDefaultEvalConfig(void);
CLCompileConfig clDefaultCompileConfig(void);

// Resets the compiler and runtime state.
void cl_vm_reset(CLVM* vm);

// Evalutes the source code and returns the result code.
// Subsequent evals will reuse the same compiler and runtime state.
// If the last statement of the script is an expression, `outVal` will contain the value.
CLResultCode cl_vm_eval(CLVM* vm, CLBytes src, CLEvalResult* res);

// Accepts an eval config. Unlike `clEvalPath`, `uri` does not get resolved and only serves to identify `src` origin.
CLResultCode cl_vm_evalx(CLVM* vm, CLBytes uri, CLBytes src, CLEvalConfig config, CLEvalResult* res);

// Resolves `uri` and evaluates the module.
CLResultCode cl_vm_eval_path(CLVM* vm, CLBytes uri, CLEvalConfig config, CLEvalResult* res);

// Consumes and evaluates all ready tasks. Returns `CL_SUCCESS` if succesfully emptied the ready queue.
// CLResultCode cl_run_ready_tasks(CLThread* t);

CLResultCode clCompile(CLVM* vm, CLBytes uri, CLBytes src, CLCompileConfig config);
CLResultCode clValidate(CLVM* vm, CLBytes src);

/// Get function parameters.
int64_t cl_param_int(CLThread* t, size_t idx);
double cl_param_float(CLThread* t, size_t idx);
CLValue cl_param_value(CLThread* t, size_t idx);

/// Convenience function to return the last error summary.
/// Returns `cl_new_compile_error_summary` if the last result was a CL_ERROR_COMPILE,
/// or `cl_thread_panic_summary` if the last result was a CL_ERROR_PANIC,
/// or the null string.
CLBytes cl_vm_error_summary(CLVM* vm);

/// Returns first compile-time error summary. Must be freed with `cl_free`.
CLBytes cl_vm_compile_error_summary(CLVM* vm);

/// Returns runtime panic summary. Must be freed with `cl_free`.
CLBytes cl_thread_panic_summary(CLThread* t);

/// Some API callbacks use this to report errors.
void clReportApiError(CLVM* vm, CLBytes msg);

// Attach a userdata pointer inside the VM.
void* clGetUserData(CLVM* vm);
void clSetUserData(CLVM* vm, void* userData);

void* clSetNewMockHttp(CLVM* vm);

// Verbose flag. In a debug build, this would print more logs.
extern bool clVerbose;

// Silent flag. Whether to print errors.
extern bool clSilent;

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

void cl_mod_core(CLVM* vm, CLSym* mod);
void cl_mod_cy(CLVM* vm, CLSym* mod);
void cl_mod_c(CLVM* vm, CLSym* mod);
void cl_mod_meta(CLVM* vm, CLSym* mod);
void cl_mod_math(CLVM* vm, CLSym* mod);

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
// [ Memory ]
// -----------------------------------
void cl_thread_release(CLThread* t, CLValue val);
void cl_thread_retain(CLThread* t, CLValue val);

// -----------------------------------
// [ Host function. ]
// -----------------------------------
void* cl_thread_ret(CLThread* t, size_t size);
void* cl_thread_param(CLThread* t, size_t size);
void* cl_thread_ptr(CLThread* t);
double cl_thread_float(CLThread* t);

// Stats of a GC run.
typedef struct CLGCResult {
    // Total number of objects freed.
    uint32_t num_obj_freed;
} CLGCResult;

// Run the reference cycle detector once and return statistics.
CLGCResult cl_thread_detect_cycles(CLThread* t);

// Get's the current global reference count.
// NOTE: This will panic if the lib was not built with `TrackGlobalRC`.
// RELATED: `cl_thread_count_objects()`
size_t cl_thread_rc(CLThread* t);

// Returns the number of live objects.
// This can be used to check if all objects were cleaned up after `clDeinit`.
size_t cl_thread_count_objects(CLThread* t);

// TRACE mode: Dump live objects recorded in global object map.
void cl_thread_dump_live_objects(CLThread* t);

// For embedded, Cyber by default uses malloc (it can be configured to use the high-perf mimalloc).
// If the host uses a different allocator than Cyber, use `clAlloc` to allocate memory
// that is handed over to Cyber so it knows how to free it.
// This is also used to manage accessible buffers when embedding WASM.
// Only pointer is returned so wasm callsite can just receive the return value.
void* cl_alloc(CLVM* vm, size_t size);
CLBytes cl_new_bytes(CLVM* vm, size_t size);

// When using the Zig allocator, you'll need to pass the original memory size.
// For all other allocators, use 1 for `len`.
void cl_vm_free(CLVM* vm, CLBytes bytes);
void cl_vm_freez(CLVM* vm, const char* str);

CLAllocator clGetAllocator(CLVM* vm);

// -----------------------------------
// [ Values ]
//
// Functions that begin with `clNew` instantiate objects on the heap and
// automatically retain +1 refcount.
// -----------------------------------

// Returns a short description about a value given their type id.
// Specializes for common types. Other types will only output the type id and the address.
// This is very basic compared to the core API's `to_print_string` and `dump`.
CLBytes cl_value_desc(CLVM* vm, CLTypeId val_t, CLValue* val);

// void.
CLValue cl_void();

// Booleans.
CLValue clBool(bool b);
bool clAsBool(CLValue val);    // Assumes boolean.
bool clAsRefBool(CLValue val); // Assumes ref boolean.
bool clToBool(CLValue val); // Performs a naive conversion.

// 64-bit integer.
CLValue cl_int(int64_t n);

// CLValue clInt32(int32_t n); // Unboxed.

// 64-bit float.
CLValue cl_float(double f);
double cl_as_float(CLValue val);

// Pointer.
CLValue clPtr(void* ptr);

// Symbols.
CLValue clSymbol(CLVM* vm, CLBytes str);
uint64_t clAsSymbol(CLValue val);

// `cl_str` is the recommended way to initialize a new string.
// `cl_astr` or `cl_ustr` asserts an ASCII or UTF-8 string respectively.
CL_str cl_str_init(CLThread* t, CLBytes str);
CL_str cl_astr_init(CLThread* t, CLBytes str);
CL_str cl_ustr_init(CLThread* t, CLBytes str);
void cl_str_deinit(CLThread* t, CL_str* str);
CLBytes clAsString(CLValue val);
CLBytes cl_object_string(CLThread* t, CLValue val);  // Conversion from value to a basic string description.

// Functions.
CLFuncSig clGetFuncSig(CLVM* vm, const CLType* params, size_t nparams, CLType ret_t);
CLValue cl_new_func_union(CLThread* t, CLType* union_t, CLHostFn func);

// Types.
CLType* clGetType(CLValue val);
CLTypeId clTypeId(CLType* type);

// Consumes and lifts a value (given as a pointer to the value) to the heap.
CLValue cl_lift(CLThread* t, CLType* type, void* value);

// Returns the field value of the receiver object `rec`.
CLValue clGetField(CLVM* vm, CLValue rec, CLBytes name);

// Returns a new option with the some case.
CLValue cl_option(CLThread* t, CLType* option_t, CLValue val);

// Returns a new option with the none case.
CLValue cl_option_none(CLThread* t, CLType* option_t);

// Returns a new result with a resolved value.
CLValue cl_result(CLThread* t, CLType* res_t, CLValue val);

// Returns a new result with an error.
CLValue cl_result_error(CLThread* t, CLType* res_t, CLValue err);

// Returns a new choice of a given case name and value.
CLValue cl_choice(CLThread* t, CLType* choice_t, CLBytes name, CLValue val);

// Returns the unwrapped value of a choice.
CLValue cl_choice_unwrap(CLThread* t, CLType* choice_t, CLValue choice, CLBytes name);

// Creates an empty array value.
// Expects an array type `[]T` as `array_t` which can be obtained from `clExpandTypeTemplate` or `clResolveType`.
CLValue cl_array_empty_new(CLVM* vm, CLType* array_t);

CLValue cl_map_empty_new(CLThread* t, CLType* map_t);

#ifdef __cplusplus
} // extern "C"
#endif // __cplusplus

#endif // !CYBER_H