pub const Engine = enum {
    /// Bytecode executed using Zig VM.
    zig,
    /// Bytecode executed using C VM.
    c,
};