// #define AOT_DEBUG 1

CBI_Slice cbi_str_slice(CB_Thread* ctx, CB_str str) {
    return (CBI_Slice){str.ptr, cb_str_len(ctx, &str)};
}

CBI_Slice cbi_strz_slice(const char* str) {
    return (CBI_Slice){(void*)str, strlen(str)};
}

CB_MetaType cb_object_type(CB_Thread* ctx, CB_Object o) {
    CB_int id = ((CBI_ObjectHeader*)((size_t)o - sizeof(CBI_ObjectHeader)))->meta;
    return id;
}

const char* cbi_object_typename(CB_Thread* ctx, CB_Object o) {
    CB_int id = ((CBI_ObjectHeader*)((size_t)o - sizeof(CBI_ObjectHeader)))->meta;
    return cbi_type_tables[id].name;
}

CB_str cb_type_name(CB_Thread* ctx, CB_MetaType type) {
    const char* name = cbi_type_tables[type].name;
    return cb_astr(ctx, name, strlen(name));
}

void printStackTrace(int skip_frames) {
    void *frames[20];
    int size = backtrace(frames, 20);
    char **lines = backtrace_symbols(frames + skip_frames, size);
    for (int i = 0; i < size; i++) {
        fprintf(stderr, "  %s\n", lines[i]);
    }
    free(lines);
}

void cb_free(CB_Thread* ctx, void* ptr) {
    free(ptr);
}

CB_byte* cb_alloc(CB_Thread* ctx, CB_int size) {
    return malloc(size);
}

void cb_print(CB_Thread* ctx, CB_Object o) {
    CB_str str = cb_object_string(ctx, o);
    CBI_Slice slice = cbi_str_slice(ctx, str);
    fprintf(stdout, "%.*s\n", (int)slice.len, (char*)slice.ptr);
    cb_release_opt(ctx, str.buf);
}

void cb_eprint(CB_Thread* ctx, CB_Object o) {
    CB_str str = cb_object_string(ctx, o);
    CBI_Slice slice = cbi_str_slice(ctx, str);
    fprintf(stderr, "%.*s\n", (int)slice.len, (char*)slice.ptr);
    cb_release_opt(ctx, str.buf);
}

void cb_panic(CB_Thread* ctx, CB_str msg) {
    CBI_Slice slice = cbi_str_slice(ctx, msg);
    fprintf(stderr, "panic: %.*s\n", (int)slice.len, (char*)slice.ptr);
    printStackTrace(1);
    exit(1);
}

void cbi_panic(const char* msg) {
    fprintf(stderr, "panic: %s\n", msg);
    printStackTrace(1);
    exit(1);
}

void cbi_panic_fmt(const char *format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    printStackTrace(1);
    exit(1);
}

void* cbi_retain(CB_Thread* ctx, void* obj) {
    CBI_ObjectHeader* header = (CBI_ObjectHeader*)((size_t)obj - 8);
    const char* name = cbi_object_typename(ctx, obj);
#ifdef AOT_DEBUG
    fprintf(stderr, "RETAIN %p %s\n", obj, name);
#endif
    header->rc += 1;
    return obj;
}

void* cbi_lift(CB_Thread* ctx, void* src, CB_int type, CB_int size) {
    CBI_HeapObject* obj = (CBI_HeapObject*)cb_object_undef(ctx, type, size);
    memcpy(obj, src, size);
    return obj;
}

void cb_free_object(CB_Thread* ctx, void* obj, CB_int size) {
    CBI_ObjectHeader* header = (CBI_ObjectHeader*)((size_t)obj - 8);
    const char* name = cbi_object_typename(ctx, obj);
#ifdef AOT_DEBUG
    fprintf(stderr, "FREE %p %s\n", obj, name);
#endif
    free(header);
    ctx->trace_num_objs -= 1;
}

void* cb_object_undef(CB_Thread* ctx, CB_int type, CB_int size) {
    CBI_ObjectHeader* obj = (CBI_ObjectHeader*)malloc(size + sizeof(CBI_ObjectHeader));
    obj->meta = type;
    obj->rc = 1;
    ctx->trace_num_objs += 1;
    const char* name = cbi_object_typename(ctx, &obj->data[0]);
#ifdef AOT_DEBUG
    fprintf(stderr, "NEW %p %s\n", &obj->data[0], name);
#endif
    return &obj->data[0];
}

void* cbi_func_union(CB_Thread* ctx, CB_int type, void* ptr) {
    CBI_HeapObject* obj = (CBI_HeapObject*)cb_object_undef(ctx, type, sizeof(CB_FuncUnion));
    obj->func_union.kind = 0;
    obj->func_union.ptr = ptr;
    return obj;
}

void* cbi_closure(CB_Thread* ctx, CB_int type, void* ptr, void** captures, size_t num_captures) {
    CBI_HeapObject* obj = (CBI_HeapObject*)cb_object_undef(ctx, type, num_captures * 8 + sizeof(CB_FuncUnion));
    obj->func_union.kind = 2;
    obj->func_union.num_cap = num_captures;
    obj->func_union.ptr = ptr;
    memcpy(&obj->func_union.captures[0], captures, num_captures * 8);
    for (int i = 0; i < num_captures; i += 1) {
        cbi_retain(ctx, captures[i]);
    }
    return obj;
}

void* cbi_unwrap_addr(CB_Thread* ctx, void* ptr, u64 tag) {
    if (*(u64*)ptr == tag) {
        return (u64*)ptr + 1;
    } else {
        cbi_panic_fmt("panic: Expected active choice tag `%" PRIu64 "`, found `%" PRIu64 "`.\n", tag, *(u64*)ptr);
    }
}

CB_str cb_ustr(CB_Thread* ctx, const char* str, CB_int len) {
    CB_str new = cb_ustr_undef(ctx, len);
    memcpy(new.ptr, str, len);
    return new;
}

CB_str cbi_ustr_static(CB_Thread* ctx, const char* str, CB_int len) {
    return (CB_str){
        .buf = NULL,
        .ptr = (CB_byte*)str,
        .len_ = len,
    };
}

CB_str cb_astr(CB_Thread* ctx, const char* str, CB_int len) {
    CB_str new = cb_astr_undef(ctx, len);
    memcpy(new.ptr, str, len);
    return new;
}

CB_str cbi_astr_static(CB_Thread* ctx, const char* str, CB_int len) {
    return (CB_str){
        .buf = NULL,
        .ptr = (CB_byte*)str,
        .len_ = len | (1ULL << 63),
    };
}

CB_int cb_func_union_size(CB_Thread* ctx, void* ptr) {
    CB_FuncUnion* func = (CB_FuncUnion*)ptr;
    if (func->kind == 2) {
        return 24 + func->num_cap * 8;
    } else {
        return 16;
    }
}

CB_bool cb_release_only(CB_Thread* ctx, void* ptr) {
    CBI_ObjectHeader* header = (CBI_ObjectHeader*)((size_t)ptr - 8);
    header->rc -= 1;
    return header->rc == 0;
}

void cb_release_opt(CB_Thread* ctx, void* obj) {
    if (obj != NULL) {
        cb_release(ctx, obj);
    }
}

void cb_release(CB_Thread* ctx, void* obj) {
    const char* name = cbi_object_typename(ctx, obj);
#ifdef AOT_DEBUG
    fprintf(stderr, "RELEASE %p %s\n", obj, name);
#endif
    CBI_ObjectHeader* header = (CBI_ObjectHeader*)((size_t)obj - 8);
    header->rc -= 1;
    if (header->rc == 0) {
        CBI_DeinitObjectFn deinit = cbi_type_tables[header->meta].deinitObject;
#ifdef TRACE
        if (deinit == NULL) {
            cbi_panic("Missing deinitObject.");
        }
#endif
        deinit(ctx, obj);
    }
}

CBI_DeinitObjectFn cb_get_deinit_object(CB_Thread* ctx, void* obj) {
    CBI_ObjectHeader* header = (CBI_ObjectHeader*)((size_t)obj - 8);
    return cbi_type_tables[header->meta].deinitObject;
}

void cb_memcpy(CB_Thread* ctx, u8* dst, u8* src, i64 len) {
    memcpy(dst, src, len);
}

CB_float cb_float_pow(CB_Thread* ctx, CB_float base, CB_float exp) {
    return pow(base, exp);
}

CB_int cb_int_umul(CB_Thread* ctx, CB_int a, CB_int b) {
    return BITCAST(CB_int, BITCAST(u64, a) * BITCAST(u64, b));
}

CB_bool cb_int_uge(CB_Thread* ctx, CB_int a, CB_int b) {
    return BITCAST(u64, a) >= BITCAST(u64, b);
}

CB_bool cb_int_ugt(CB_Thread* ctx, CB_int a, CB_int b) {
    return BITCAST(u64, a) > BITCAST(u64, b);
}

CB_str cb_float_fmt(CB_Thread* ctx, CB_float self) {
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%.10f", self);
    while (len > 0) {
        if (buf[len-1] == '0') {
            len -= 1;
        } else {
            break;
        }
    }
    if (len > 0 && buf[len-1] == '.') {
        len += 1;
    }
    return cb_astr(ctx, buf, len);
}

CB_int cb_utf8_seq_len(CB_byte first) {
}

CB_str cb_symbol_name(CB_Thread* ctx, CB_symbol self) {
    const char* name = cbi_symbols[self];
    return cb_astr(ctx, name, strlen(name));
}

CB_str cb_i32_fmt(CB_Thread* ctx, CB_i32 self, CB_NumberFormat format) {
    if (format != 2) {
        cbi_panic("Unsupported.");
    }
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%d", self);
    return cb_astr(ctx, buf, len);
}

CB_str cb_int_fmt(CB_Thread* ctx, CB_int self, CB_NumberFormat format) {
    if (format != 2) {
        cbi_panic("Unsupported.");
    }
    char buf[32];
    int len = snprintf(buf, sizeof(buf), "%" PRId64, self);
    return cb_astr(ctx, buf, len);
}

CB_str cb_byte_fmt(CB_Thread* ctx, CB_byte self, CB_NumberFormat format) {
    char buf[32];
    switch (format) {
    case CB_NumberFormat_hex: {
        int len = snprintf(buf, sizeof(buf), "%x", self);
        return cb_astr(ctx, buf, len);
    }
    case CB_NumberFormat_oct: {
        int len = snprintf(buf, sizeof(buf), "%o", self);
        return cb_astr(ctx, buf, len);
    }
    case CB_NumberFormat_dec: {
        int len = snprintf(buf, sizeof(buf), "%u", self);
        return cb_astr(ctx, buf, len);
    }
    case CB_NumberFormat_bin: {
        i64 len = 0;
        bool found_msb = false;
        for (int i = 0; i < 8; i += 1) {
            bool bit = self & (1 << (7-i));
            if (!found_msb && bit) {
                found_msb = true;
            }
            if (found_msb) {
                buf[len] = bit ? '1' : '0';
                len += 1;
            }
        }
        return cb_astr(ctx, buf, len);
    }
    default:
        cbi_panic("Unsupported.");
    }
}

CB_bool cb_cmp_str(CB_str a, CB_str b) {
    i64 a_len = a.len_ & ~(1ULL << 63);
    i64 b_len = b.len_ & ~(1ULL << 63);
    if (a_len != b_len) {
        return false;
    }
    return strncmp((const char*)a.ptr, (const char*)b.ptr, a_len) == 0;
}

void* cbi_cast(CB_Thread* ctx, void* obj, u32 type) {
    u32 id = ((CBI_ObjectHeader*)((size_t)obj - sizeof(CBI_ObjectHeader)))->meta;
    if (id != type) {
        const char* actual_t = cbi_type_tables[id].name;
        const char* target_t = cbi_type_tables[type].name;
        cbi_panic_fmt("panic: Can not cast `^%s` to `^%s`.\n", actual_t, target_t);
    }
    return obj;
}

void cbi_await(CB_Thread* ctx, void* val) {
    cbi_panic("Unsupported await.");
}

void cbi_segfault_handler(int signum) {
    fprintf(stderr, "abort: Segmentation fault. signal=%d\n", signum);
    printStackTrace(1);
    exit(1);
}

void cbi_bus_handler(int signum) {
    fprintf(stderr, "abort: Bus error. signal=%d\n", signum);
    printStackTrace(1);
    exit(1);
}

void cbi_main_pre(CB_Thread* ctx) {
    if (signal(SIGSEGV, cbi_segfault_handler) == SIG_ERR) {
        fprintf(stderr, "Failed to set SIGSEGV handler");
        exit(1);
    }
    if (signal(SIGBUS, cbi_bus_handler) == SIG_ERR) {
        fprintf(stderr, "Failed to set SIGBUS handler");
        exit(1);
    }
    ctx->trace_num_objs = 0;
}

void cbi_main_post(CB_Thread* ctx) {
    if (ctx->trace_num_objs > 0) {
        cbi_panic_fmt("panic: Unfreed objects=%zu\n", ctx->trace_num_objs);
    }
}

int main() {
    CB_Thread* ctx = &cb_ctx;
    cbi_main_pre(ctx);
    CB_Object res = cbi_main_user(ctx);
    if (res != NULL) {
        // For now, main return is consumed.
        cb_release(ctx, res);
    }
    cbi_main_post(ctx);
    return 0;
}