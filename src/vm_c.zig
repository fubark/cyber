const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const cy = @import("cyber.zig");

const c = @cImport({
    @cDefine("DEBUG", if (builtin.mode == .Debug) "1" else "0");
    @cDefine("TRACK_GLOBAL_RC", if (build_options.trackGlobalRC) "1" else "0");
    @cDefine("TRACE", if (build_options.trace) "1" else "0");
    @cDefine("IS_32BIT", if (cy.is32Bit) "1" else "0");
    @cDefine("HAS_GC", if (cy.hasGC) "1" else "0");
    @cInclude("vm.h");
});

pub usingnamespace c;