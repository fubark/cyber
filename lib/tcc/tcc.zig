const std = @import("std");
const log = std.log.scoped(.tcc);

const c = @cImport({
    @cInclude("libtcc.h");
});

pub usingnamespace c;