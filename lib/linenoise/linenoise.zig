const std = @import("std");

const c = @cImport({
    @cInclude("linenoise.h");
});

pub usingnamespace c;