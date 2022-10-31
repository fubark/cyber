pub const Engine = enum {
    /// Compiled to byte code and evaluated from VM.
    vm,
    /// Compiled to js and evaluated from qjs.
    qjs,
    /// Compiled to js and evaluated from the browser.
    webjs,
};