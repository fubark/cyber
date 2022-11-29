pub const Engine = enum {
    /// Compiled to byte code and evaluated from VM.
    vm,
    /// JIT-enabled, this is a placeholder and work has not been started.
    jit,
};