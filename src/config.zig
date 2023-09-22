pub const Engine = enum {
    /// Bytecode executed using Zig VM.
    zig,
    /// Bytecode executed using C VM.
    c,
};

pub const Allocator = enum {
    zig,
    malloc,
    mimalloc,
};