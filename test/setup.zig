const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = cy.fatal;
const all = @import("all");
const c = all.cy.C;
const cy = all.cy;
const vmc = cy.vmc;
const log = cy.log.scoped(.setup);

pub const Config = struct {
    uri: []const u8 = "main",

    /// Don't print panic errors.
    silent: bool = false,

    enableFileModules: bool = false,

    checkGlobalRc: bool = true,
    check_object_count: bool = true,

    // Whether to performGC at end of eval.
    cleanupGC: bool = false,

    preEval: ?*const fn (run: *VMrunner) void = null,

    debug: bool = false,

    reload: bool = false,

    ctx: ?*anyopaque = null,

    chdir: ?[]const u8 = null,

    pub fn withReload(self: Config) Config {
        var new = self;
        new.reload = true;
        return new;
    }

    pub fn withSilent(self: Config) Config {
        var new = self;
        new.silent = true;
        return new;
    }

    pub fn withDebug(self: Config) Config {
        var new = self;
        new.debug = true;
        return new;
    }

    pub fn withChdir(self: Config, dir: []const u8) Config {
        var new = self;
        new.chdir = dir;
        return new;
    }

    pub fn initFileModules(uri: []const u8) Config {
        return .{
            .enableFileModules = true,
            .uri = uri,
        };
    }
};

extern fn clInitCLI(vm: *c.VM) void;
extern fn clDeinitCLI(vm: *c.VM) void;

pub const VMrunner = struct {
    vm: *c.VM,
    ctx: ?*anyopaque = null,

    pub fn init() VMrunner {
        const vm = c.create();
        clInitCLI(@ptrCast(vm));
        return .{
            .vm = @ptrCast(vm),
        };
    }

    pub fn deinit(self: *VMrunner) void {
        clDeinitCLI(self.vm);
        c.deinit(self.vm);
        c.destroy(self.vm);
    }

    pub fn internal(self: *VMrunner) *cy.VM {
        return @ptrCast(@alignCast(self.vm));
    }

    fn checkMemory(self: *VMrunner) !bool {
        return self.vm.checkMemory();
    }

    fn compile(self: *VMrunner, src: []const u8) !cy.ByteCodeBuffer {
        return self.vm.compile(src);
    }

    pub fn getStackTrace(self: *VMrunner) *const cy.StackTrace {
        return self.internal().getStackTrace();
    }

    pub fn getTrace(self: *VMrunner) *vmc.TraceInfo {
        return self.internal().c.trace;
    }

    pub fn expectErrorReport(self: *VMrunner, res: EvalResult, expErr: c.ResultCode, expReport: []const u8) !void {
        var errorMismatch = false;
        if (res.code == c.Success) {
            const val_dump = c.newValueDump(self.vm, res.value);
            defer c.free(self.vm, val_dump);
            std.debug.print("expected error.{s}, found: {s}\n", .{
                c.fromStr(c.resultName(expErr)), c.fromStr(val_dump),
            });
            return error.TestUnexpectedError;
        } else {
            if (res.code != expErr) {
                std.debug.print("expected error.{s}, found error.{s}\n", .{ c.fromStr(c.resultName(expErr)), c.fromStr(c.resultName(res.code)) });
                errorMismatch = true;
                // Continue to compare report.
            }
        }
        const report = try self.getErrorSummary(res.code);
        defer c.free(self.vm, report);
        try eqUserError(t.alloc, c.fromStr(report), expReport);

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    pub fn expectErrorReport2(self: *VMrunner, res: EvalResult, expReport: []const u8) !void {
        const errorMismatch = false;
        if (res.code == c.Success) {
            const val_dump = c.newValueDump(self.vm, res.value);
            defer c.free(self.vm, val_dump);
            std.debug.print("expected error, found: {s}\n", .{ c.fromStr(val_dump) });
            return error.TestUnexpectedError;
        }
        // Continue to compare report.
        const report = try self.getErrorSummary(res.code);
        defer c.free(self.vm, report);
        try eqUserError(t.alloc, c.fromStr(report), expReport);

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    fn getErrorSummary(self: *VMrunner, code: c.ResultCode) !c.Str {
        if (code == c.ErrorCompile) {
            return c.newErrorReportSummary(self.vm);
        } else if (code == c.ErrorPanic) {
            return c.newPanicSummary(self.vm);
        }
        return error.Unsupported;
    }

    pub fn evalPass(run: *VMrunner, config: Config, src: []const u8) !void {
        return run.eval(config, src, null);
    }

    pub fn eval(run: *VMrunner, config: Config, src: []const u8, optCb: ?*const fn (*VMrunner, EvalResult) anyerror!void) !void {
        const vm = run.vm;
        defer {
            if (config.silent) {
                c.setSilent(false);
            }
            if (config.debug) {
                c.setVerbose(false);
                t.setLogLevel(.warn);
            }
        }
        if (config.silent) {
            c.setSilent(true);
        }
        if (config.debug) {
            c.setVerbose(true);
            t.setLogLevel(.debug);
        }

        // Set and restore cwd.
        var cwdBuf: [1024]u8 = undefined;
        const cwd = try std.posix.getcwd(&cwdBuf);
        if (config.chdir) |chdir| {
            try std.posix.chdir(chdir);
        }
        defer {
            if (config.chdir != null) {
                std.posix.chdir(cwd) catch @panic("error");
            }
        }

        run.ctx = config.ctx;
        if (config.preEval) |preEval| {
            preEval(run);
        }

        var r_uri = config.uri;
        if (config.enableFileModules) {
            r_uri = c.fromStr(c.resolve(vm, c.toStr(config.uri)));
        }
        defer if (config.enableFileModules) {
            c.free(vm, c.toStr(r_uri));
        };

        var resv: c.Value = undefined;
        const c_config = c.EvalConfig{
            .single_run = false,
            .file_modules = config.enableFileModules,
            .gen_all_debug_syms = config.debug,
            .backend = cy.fromTestBackend(build_options.testBackend),
            .spawn_exe = false,
            .reload = config.reload,
        };
        c.reset(vm);
        const res_code = c.evalExt(vm, c.toStr(r_uri), c.toStr(src), c_config, @ptrCast(&resv));

        if (optCb) |cb| {
            const res = EvalResult{
                .code = res_code,
                .value = resv,
            };
            cb(run, res) catch |err| {
                if (err == error.EvalError) {
                    errReport(vm, res_code);
                }
                return err;
            };
        } else {
            if (res_code == c.Await) {
                // Consume all ready tasks.
                var cont_code = res_code;
                while (cont_code == c.Await) {
                    cont_code = c.runReadyTasks(vm);
                }
                if (cont_code != c.Success) {
                    errReport(vm, cont_code);
                    return error.EvalError;
                }
            } else {
                if (res_code != c.Success) {
                    errReport(vm, res_code);
                    return error.EvalError;
                }
            }
        }

        // Deinit, so global objects from builtins are released.
        c.deinit(vm);

        // Run GC after runtime syms are released.
        if (config.cleanupGC) {
            _ = c.performGC(vm);
        }

        if (config.checkGlobalRc) {
            const grc = c.getGlobalRC(vm);
            if (grc != 0) {
                c.traceDumpLiveObjects(vm);
                cy.panicFmt("unreleased refcount: {}", .{grc});
            }
        }

        if (config.check_object_count) {
            const count = c.countObjects(vm);
            if (count != 0) {
                c.traceDumpLiveObjects(vm);
                cy.panicFmt("unfreed objects: {}", .{count});
            }
        }
    }

    pub fn valueIsString(_: *VMrunner, val: cy.Value, exp: []const u8) !void {
        if (val.isString()) {
            try t.eqStr(val.asString(), exp);
        } else {
            return error.NotAString;
        }
    }

    pub fn valueIsF64(self: *VMrunner, act: cy.Value, exp: f64) !void {
        _ = self;
        if (act.isFloat()) {
            try t.eq(act.asF64(), exp);
            return;
        }
        return error.NotF64;
    }

    pub fn valueIsI32(self: *VMrunner, act: cy.Value, exp: i32) !void {
        _ = self;
        if (act.isFloat()) {
            const actf = act.asF64();
            if (cy.Value.floatCanBeInteger(actf)) {
                try t.eq(act.asF64toI32(), exp);
                return;
            }
        }
        return error.NotI32;
    }

    pub fn assertValueString(_: *VMrunner, val: cy.Value) ![]const u8 {
        if (val.isString()) {
            return val.asString();
        } else {
            return error.NotAString;
        }
    }

    pub fn valueToIntSlice(self: *VMrunner, val: cy.Value) ![]const i32 {
        _ = self;
        const obj = cy.ptrAlignCast(*cy.HeapObject, val.asPointer());
        const list = cy.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
        const dupe = try t.alloc.alloc(i32, list.len);
        for (list.items(), 0..) |it, i| {
            dupe[i] = @intFromFloat(it.toF64());
        }
        return dupe;
    }
};

pub fn compile(config: Config, src: []const u8) !void {
    var run = VMrunner.init();
    defer run.deinit();
    
    defer {
        if (config.silent) {
            c.setSilent(false);
        }
        if (config.debug) {
            c.setVerbose(false);
            t.setLogLevel(.warn);
        }
    }

    if (config.silent) {
        c.setSilent(true);
    }
    if (config.debug) {
        c.setVerbose(true);
        t.setLogLevel(.debug);
    }

    var compile_c = c.defaultCompileConfig();
    compile_c.single_run = false;
    compile_c.file_modules = config.enableFileModules;
    // .genAllDebugSyms = config.debug,
    compile_c.backend = cy.fromTestBackend(build_options.testBackend);

    const res_code = c.compile(run.vm, c.toStr(config.uri), c.toStr(src), compile_c);
    if (res_code != c.Success) {
        errReport(run.vm, res_code);
        return error.CompileError;
    }
}

pub const EvalResult = struct {
    code: c.ResultCode,
    value: c.Value,

    pub fn getValueC(self: EvalResult) !c.Value {
        if (self.code != c.Success) {
            return error.EvalError;
        }
        return self.value;
    }

    pub fn getValue(self: EvalResult) !cy.Value {
        if (self.code != c.Success) {
            return error.EvalError;
        }
        return @as(cy.Value, @bitCast(self.value));
    }
};

pub fn eval(config: Config, src: []const u8, optCb: ?*const fn (*VMrunner, EvalResult) anyerror!void) !void {
    var run = VMrunner.init();
    defer run.deinit();
    try run.eval(config, src, optCb);
}

pub fn errReport(vm: *c.VM, code: c.ResultCode) void {
    if (c.silent()) {
        return;
    }
    switch (code) {
        c.ErrorPanic => {
            const summary = c.newPanicSummary(vm);
            defer c.free(vm, summary);
            std.debug.print("{s}", .{c.fromStr(summary)});
        },
        c.ErrorCompile => {
            const summary = c.newErrorReportSummary(vm);
            defer c.free(vm, summary);
            std.debug.print("{s}", .{c.fromStr(summary)});
        },
        c.ErrorUnknown => {
            std.debug.print("Unknown error.\n", .{});
        },
        else => {},
    }
}

pub fn evalPass(config: Config, src: []const u8) !void {
    return eval(config, src, null);
}

/// relPath does not have to physically exist.
pub fn eqUserError(alloc: std.mem.Allocator, act: []const u8, expTmpl: []const u8) !void {
    var exp: std.ArrayListUnmanaged(u8) = .{};
    defer exp.deinit(alloc);
    var curTmpl = expTmpl;
    while (true) {
        var found = false;
        const pos: usize = 0;
        if (!cy.isWasm) {
            if (std.mem.indexOfPos(u8, curTmpl, pos, "@AbsPath(")) |idx| {
                if (std.mem.indexOfScalarPos(u8, curTmpl, idx, ')')) |endIdx| {
                    const copy = try alloc.dupe(u8, curTmpl);
                    exp.clearRetainingCapacity();

                    try exp.appendSlice(alloc, copy[pos..idx]);
                    const basePath = try std.fs.realpathAlloc(alloc, ".");
                    defer t.alloc.free(basePath);
                    try exp.appendSlice(alloc, basePath);
                    try exp.append(alloc, std.fs.path.sep);

                    const relPathStart = exp.items.len;
                    try exp.appendSlice(alloc, copy[idx+9..endIdx]);
                    if (builtin.os.tag == .windows) {
                        _ = std.mem.replaceScalar(u8, exp.items[relPathStart..], '/', '\\');
                    }

                    try exp.appendSlice(alloc, copy[endIdx+1..]);
                    alloc.free(copy);
                    curTmpl = exp.items;
                    found = true;
                }
            }
        }

        if (!found) {
            break;
        }
    }
    try t.eqStr(act, curTmpl);
}