const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = cy.fatal;
const cy = @import("../src/cyber.zig");
const cli = @import("../src/cli.zig");
const vmc = cy.vmc;
const log = cy.log.scoped(.setup);

const UserError = error{Panic, CompileError, TokenError, ParseError};

pub const Config = struct {
    uri: []const u8 = "main",

    /// Don't print panic errors.
    silent: bool = false,

    enableFileModules: bool = false,

    checkGlobalRc: bool = true,

    // Whether to performGC at end of eval.
    cleanupGC: bool = false,

    preJit: bool = false,

    preEval: ?*const fn (run: *VMrunner) void = null,

    debug: bool = false,

    ctx: ?*anyopaque = null,

    chdir: ?[]const u8 = null,

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

var testVm: cy.VM = undefined;

pub const VMrunner = struct {
    vm: *cy.VM,
    ctx: ?*anyopaque = null,

    pub fn create() *VMrunner {
        var new = t.alloc.create(VMrunner) catch fatal();
        new.init() catch fatal();
        return new;
    }

    pub fn destroy(self: *VMrunner) void {
        self.deinit();
        t.alloc.destroy(self);
    }

    fn init(self: *VMrunner) !void {
        self.* = .{
            .vm = @ptrCast(&testVm),
        };
        self.vm.init(t.alloc) catch fatal();
        cli.setupVMForCLI(self.vm);
    }

    pub fn deinit(self: *VMrunner) void {
        self.deinitExt(true);
    }

    pub fn deinitExt(self: *VMrunner, checkGlobalRC: bool) void {
        self.vm.deinit(false);
        if (checkGlobalRC) {
            cy.arc.checkGlobalRC(self.vm) catch cy.panic("unreleased refcount");
        }
    }

    fn deinitValue(self: *VMrunner, val: cy.Value) void {
        self.vm.release(val);
    }

    fn checkMemory(self: *VMrunner) !bool {
        return self.vm.checkMemory();
    }

    fn compile(self: *VMrunner, src: []const u8) !cy.ByteCodeBuffer {
        return self.vm.compile(src);
    }

    pub fn getStackTrace(self: *VMrunner) *const cy.StackTrace {
        return self.vm.getStackTrace();
    }

    pub fn getTrace(self: *VMrunner) *vmc.TraceInfo {
        return self.vm.trace;
    }

    pub fn assertPanicMsg(self: *VMrunner, exp: []const u8) !void {
        const msg = try self.allocPanicMsg();
        defer self.vm.alloc.free(msg);
        try t.eqStr(msg, exp);
    }

    fn allocPanicMsg(self: *VMrunner) ![]const u8 {
        return cy.debug.allocPanicMsg(self.vm);
    }

    pub fn expectErrorReport(self: *VMrunner, val: anytype, expErr: UserError, expReport: []const u8) !void {
        var errorMismatch = false;
        if (val) |actual_payload| {
            std.debug.print("expected error.{s}, found {any}\n", .{ @errorName(expErr), actual_payload });
            return error.TestUnexpectedError;
        } else |actual_error| {
            if (actual_error != expErr) {
                std.debug.print("expected error.{s}, found error.{s}\n", .{ @errorName(expErr), @errorName(actual_error) });
                errorMismatch = true;
                // Continue to compare report.
            }
        }
        const report = try self.vm.allocLastErrorReport();
        try eqUserError(t.alloc, report, expReport);

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    pub fn expectErrorReport2(self: *VMrunner, val: anytype, expReport: []const u8) !void {
        var errorMismatch = false;
        if (val) |actual_payload| {
            std.debug.print("expected error, found {any}\n", .{ actual_payload });
            return error.TestUnexpectedError;
        } else |_| {
            // Continue to compare report.
        }
        const report = try self.vm.allocLastErrorReport();
        try eqUserError(t.alloc, report, expReport);

        if (errorMismatch) {
            return error.TestUnexpectedError;
        }
    }

    pub fn evalExt(self: *VMrunner, config: Config, src: []const u8) !cy.Value {
        if (config.silent) {
            cy.silentError = true;
        }
        defer {
            if (config.silent) {
                cy.silentError = false;
            }
        }
        try self.resetEnv();    
        return self.vm.eval(config.uri, src, .{
            .singleRun = false,
            .enableFileModules = config.enableFileModules,
        }) catch |err| {
            switch (err) {
                error.Panic,
                error.TokenError,
                error.ParseError,
                error.CompileError => {
                    if (!cy.silentError) {
                        const report = try self.vm.allocLastErrorReport();
                        defer t.alloc.free(report);
                        std.debug.print("{s}", .{report});
                    }
                },
                else => {},
            }
            return err;
        };
    }

    pub fn evalExtNoReset(self: *VMrunner, config: Config, src: []const u8) !cy.Value {
        if (config.silent) {
            cy.silentError = true;
        }
        defer {
            if (config.silent) {
                cy.silentError = false;
            }
        }
        return self.vm.eval(config.uri, src, .{ .singleRun = false, .enableFileModules = config.enableFileModules, .reload = true }) catch |err| {
            switch (err) {
                error.Panic,
                error.TokenError,
                error.ParseError,
                error.CompileError => {
                    if (!cy.silentError) {
                        const report = try self.vm.allocLastErrorReport();
                        defer t.alloc.free(report);
                        std.debug.print("{s}", .{report});
                    }
                },
                else => {},
            }
            return err;
        };
    }

    pub fn eval(self: *VMrunner, src: []const u8) !cy.Value {
        return self.evalExt(.{ .uri = "main" }, src);
    }

    pub fn resetEnv(self: *VMrunner) !void {
        self.vm.deinit(false);
        const rc = cy.arc.getGlobalRC(self.vm);
        if (rc != 0) {
            log.debug("{} unreleased refcount from previous eval", .{rc});
            return error.UnreleasedObjects;
        }
        try self.vm.init(t.alloc);
        cli.setupVMForCLI(self.vm);
    }

    fn evalNoReset(self: *VMrunner, src: []const u8) !cy.Value {
        return self.evalExtNoReset(.{ .uri = "main" }, src);
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

    fn parse(self: *VMrunner, src: []const u8) !cy.ParseResultView {
        _ = self;
        _ = src;
        return undefined;
    }
};

pub fn compile(config: Config, src: []const u8) !void {
    const run = VMrunner.create();
    defer t.alloc.destroy(run);
    defer {
        if (config.silent) {
            cy.silentError = false;
            cy.silentInternal = false;
        }
        if (config.debug) {
            cy.verbose = false;
            t.setLogLevel(.warn);
        }
    }
    errdefer run.vm.deinit(false);
    var checkGlobalRC = true;

    if (config.silent) {
        cy.silentError = true;
        cy.silentInternal = true;
    }
    if (config.debug) {
        cy.verbose = true;
        t.setLogLevel(.debug);
    }

    const res = try run.vm.compile(config.uri, src, .{ 
        .singleRun = false,
        .enableFileModules = config.enableFileModules,
        // .genAllDebugSyms = config.debug,
        .preJit = config.preJit,
    });
    if (res.err) |_| {
        try printErrorReport(run.vm, error.CompileError);
        checkGlobalRC = false;
        return error.CompileError;
    }

    run.vm.deinit(false);
}

pub const EvalResult = anyerror!cy.Value;

pub fn eval(config: Config, src: []const u8, optCb: ?*const fn (*VMrunner, EvalResult) anyerror!void) !void {
    const run = VMrunner.create();
    defer t.alloc.destroy(run);
    defer {
        if (config.silent) {
            cy.silentError = false;
            cy.silentInternal = false;
        }
        if (config.debug) {
            cy.verbose = false;
            t.setLogLevel(.warn);
        }
    }
    errdefer run.vm.deinit(false);
    var checkGlobalRC = true;

    if (config.silent) {
        cy.silentError = true;
        cy.silentInternal = true;
    }
    if (config.debug) {
        cy.verbose = true;
        t.setLogLevel(.debug);
    }

    // Set and restore cwd.
    var cwdBuf: [1024]u8 = undefined;
    const cwd = try std.os.getcwd(&cwdBuf);
    if (config.chdir) |chdir| {
        try std.os.chdir(chdir);
    }
    defer {
        if (config.chdir != null) {
            std.os.chdir(cwd) catch @panic("error");
        }
    }

    run.ctx = config.ctx;
    if (config.preEval) |preEval| {
        preEval(run);
    }

    const res = run.vm.eval(config.uri, src, .{ 
        .singleRun = false,
        .enableFileModules = config.enableFileModules,
        .genAllDebugSyms = config.debug,
        .preJit = config.preJit,
    });

    if (optCb) |cb| {
        cb(run, res) catch |err| {
            try printErrorReport(run.vm, err);
            checkGlobalRC = false;
            return err;
        };
    }  else {
        _ = res catch |err| {
            try printErrorReport(run.vm, err);
            checkGlobalRC = false;
            return err;
        };
    }

    // Deinit, so global objects from builtins are released.
    run.vm.deinitRtObjects();
    run.vm.compiler.deinitModRetained();

    // Run GC after runtime syms are released.
    if (config.cleanupGC) {
        _ = try cy.arc.performGC(run.vm);
    }

    if (config.checkGlobalRc) {
        try cy.arc.checkGlobalRC(run.vm);
    }
    run.vm.deinit(false);
}

fn printErrorReport(vm: *cy.VM, err: anyerror) !void {
    switch (err) {
        error.Panic,
        error.TokenError,
        error.ParseError,
        error.CompileError => {
            if (!cy.silentError) {
                const report = try vm.allocLastErrorReport();
                defer t.alloc.free(report);
                std.debug.print("{s}", .{report});
            }
        },
        else => {},
    }
}

pub fn evalPass(config: Config, src: []const u8) !void {
    return eval(config, src, null);
}

/// relPath does not have to physically exist.
pub fn eqUserError(alloc: std.mem.Allocator, act: [:0]const u8, expTmpl: []const u8) !void {
    defer alloc.free(act);
    var exp: std.ArrayListUnmanaged(u8) = .{};
    defer exp.deinit(alloc);
    var curTmpl = expTmpl;
    while (true) {
        var found = false;
        var pos: usize = 0;
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