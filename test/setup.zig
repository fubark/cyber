const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const fatal = stdx.fatal;
const cy = @import("../src/cyber.zig");
const log = stdx.log.scoped(.setup);

const UserError = error{Panic, CompileError, TokenError, ParseError};

pub const Config = struct {
    uri: []const u8 = "main",

    /// Don't print panic errors.
    silent: bool = false,

    enableFileModules: bool = false,

    checkGlobalRc: bool = true,

    preEval: ?*const fn (run: *VMrunner) void = null,

    debug: bool = false,

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

    pub fn initFileModules(uri: []const u8) Config {
        return .{
            .enableFileModules = true,
            .uri = uri,
        };
    }
};

var testVm: cy.VM = undefined;

pub const VMrunner = struct {
    vm: *cy.UserVM,
    trace: cy.TraceInfo,

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
            .trace = .{},
        };
        self.vm.init(t.alloc) catch fatal();
        self.vm.internal().setTrace(&self.trace);
    }

    pub fn deinit(self: *VMrunner) void {
        self.deinitExt(true);
    }

    pub fn deinitExt(self: *VMrunner, checkGlobalRC: bool) void {
        self.vm.deinit();
        self.trace.deinit(t.alloc);
        if (checkGlobalRC) {
            const rc = self.vm.getGlobalRC();
            if (rc != self.vm.internal().expGlobalRC) {
                stdx.panicFmt("unreleased refcount from previous eval: {}, exp: {}", .{rc, self.vm.internal().expGlobalRC});
            }
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

    pub fn assertPanicMsg(self: *VMrunner, exp: []const u8) !void {
        const msg = try self.allocPanicMsg();
        defer self.vm.allocator().free(msg);
        try t.eqStr(msg, exp);
    }

    fn allocPanicMsg(self: *VMrunner) ![]const u8 {
        return self.vm.allocPanicMsg();
    }

    pub fn expectErrorReport(self: *VMrunner, val: anytype, expErr: UserError, expReport: []const u8) !void {
        try t.expectError(val, expErr);
        const report = try self.vm.allocLastErrorReport();
        try eqUserError(t.alloc, report, expReport);
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
        return self.vm.eval(config.uri, src, .{ .singleRun = false, .enableFileModules = config.enableFileModules }) catch |err| {
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
        self.vm.deinit();
        const rc = self.vm.getGlobalRC();
        if (rc != 0) {
            log.debug("{} unreleased refcount from previous eval", .{rc});
            return error.UnreleasedObjects;
        }
        try self.vm.init(t.alloc);
        self.vm.internal().setTrace(&self.trace);
    }

    fn evalNoReset(self: *VMrunner, src: []const u8) !cy.Value {
        return self.evalExtNoReset(.{ .uri = "main" }, src);
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

    pub fn assertValueString(self: *VMrunner, val: cy.Value) ![]const u8 {
        if (val.isString()) {
            return self.vm.valueAsString(val);
        } else {
            return error.NotAString;
        }
    }

    pub fn valueToIntSlice(self: *VMrunner, val: cy.Value) ![]const i32 {
        _ = self;
        const obj = stdx.ptrAlignCast(*cy.HeapObject, val.asPointer());
        const list = stdx.ptrAlignCast(*cy.List(cy.Value), &obj.list.list);
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

pub const EvalResult = anyerror!cy.Value;

pub fn eval(config: Config, src: []const u8, optCb: ?*const fn (*VMrunner, EvalResult) anyerror!void) !void {
    const run = VMrunner.create();
    var checkGlobalRC = true;
    defer {
        run.deinitExt(checkGlobalRC);
        t.alloc.destroy(run);
    }

    if (config.silent) {
        cy.silentError = true;
    }
    if (config.debug) {
        cy.verbose = true;
        cy.collectDumpInfo = true;
        t.setLogLevel(.debug);
    }
    defer {
        if (config.silent) {
            cy.silentError = false;
        }
        if (config.debug) {
            cy.verbose = false;
            cy.collectDumpInfo = false;
            t.setLogLevel(.warn);
        }
    }

    if (config.preEval) |preEval| {
        preEval(run);
    }

    const res = run.vm.eval(config.uri, src, .{ 
        .singleRun = false,
        .enableFileModules = config.enableFileModules,
        .genAllDebugSyms = config.debug,
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
    run.vm.internal().compiler.deinitRtObjects();
    run.vm.internal().deinitRtObjects();

    if (config.checkGlobalRc) {
        try cy.arc.checkGlobalRC(run.vm.internal());
    }
}

fn printErrorReport(vm: *cy.UserVM, err: anyerror) !void {
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
pub fn eqUserError(alloc: std.mem.Allocator, act: []const u8, expTmpl: []const u8) !void {
    defer alloc.free(act);
    var exp: std.ArrayListUnmanaged(u8) = .{};
    defer exp.deinit(alloc);
    var pos: usize = 0;
    while (true) {
        if (std.mem.indexOfPos(u8, expTmpl, pos, "@AbsPath(")) |idx| {
            if (std.mem.indexOfScalarPos(u8, expTmpl, idx, ')')) |endIdx| {
                try exp.appendSlice(alloc, expTmpl[pos..idx]);
                const basePath = try std.fs.realpathAlloc(alloc, ".");
                defer t.alloc.free(basePath);
                try exp.appendSlice(alloc, basePath);
                try exp.append(alloc, std.fs.path.sep);

                const relPathStart = exp.items.len;
                try exp.appendSlice(alloc, expTmpl[idx+9..endIdx]);
                if (builtin.os.tag == .windows) {
                    _ = std.mem.replaceScalar(u8, exp.items[relPathStart..], '/', '\\');
                }

                pos = endIdx + 1;
            }
        }
        try exp.appendSlice(alloc, expTmpl[pos..]);
        break;
    }
    try t.eqStr(act, exp.items);
}