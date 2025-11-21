pub const Engine = enum {
    /// Bytecode executed using Zig VM.
    zig,
    /// Bytecode executed using C VM.
    c,
};

pub const TestBackend = enum {
    vm,
    jit,
    tcc,
    cc,
};

pub const Allocator = enum {
    zig,
    malloc,
    mimalloc,
};