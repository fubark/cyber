// Copyright (c) 2023 Cyber (See LICENSE)

/// Simplified API for user binded functions and Zig embedded projects.
/// For the C API, see `lib.zig` which exports declarations defined by `cyber.h`.

const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const vmc = cy.vmc;
const rt = cy.rt;
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const VM = cy.VM;
const Value = cy.Value;
const fs = @import("std/fs.zig");

/// A simplified VM handle.
pub const UserVM = struct {
    dummy: u64 = undefined,

    pub fn init(self: *UserVM, alloc: std.mem.Allocator) !void {
        try self.internal().init(alloc);
    }

    pub fn deinit(self: *UserVM) void {
        self.internal().deinit(false);
    }

    pub inline fn internal(self: *UserVM) *cy.VM {
        return @ptrCast(self);
    }

    pub inline fn constInternal(self: *const UserVM) *const cy.VM {
        return @ptrCast(self);
    }

    pub fn setApiError(self: *const UserVM, str: []const u8) void {
        const vm = self.constInternal();
        vm.compiler.hasApiError = true;
        vm.alloc.free(vm.compiler.apiError);
        vm.compiler.apiError = vm.alloc.dupe(u8, str) catch cy.fatal();
    }

    pub fn getPrint(self: *UserVM) cy.PrintFn {
        return self.internal().print;
    }

    pub fn setPrint(self: *UserVM, print: cy.PrintFn) void {
        self.internal().print = print;
    }

    pub fn getModuleLoader(self: *const UserVM) cy.ModuleLoaderFn {
        return self.constInternal().compiler.moduleLoader;
    }

    pub fn setModuleLoader(self: *const UserVM, loader: cy.ModuleLoaderFn) void {
        self.constInternal().compiler.moduleLoader = loader;
    }

    pub fn getModuleResolver(self: *const UserVM) cy.ModuleResolverFn {
        return self.constInternal().compiler.moduleResolver;
    }

    pub fn setModuleResolver(self: *const UserVM, resolver: cy.ModuleResolverFn) void {
        self.constInternal().compiler.moduleResolver = resolver;
    }

    pub fn getUserData(self: *UserVM) ?*anyopaque {
        return self.internal().userData;
    }

    pub fn setUserData(self: *UserVM, userData: ?*anyopaque) void {
        self.internal().userData = userData;
    }

    pub fn getStackTrace(self: *UserVM) *const cy.StackTrace {
        return @as(*const VM, @ptrCast(self)).getStackTrace();
    }

    pub fn getParserErrorMsg(self: *const UserVM) []const u8 {
        const vm: *const VM = @ptrCast(self);
        const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
        return chunk.parser.last_err;
    }

    pub fn allocLastUserParseError(self: *const UserVM) ![]const u8 {
        return debug.allocLastUserParseError(self.constInternal());
    }

    pub fn getParserErrorPos(self: *const UserVM) u32 {
        const vm = self.internal();
        const chunk = vm.chunks.items[vm.compiler.lastErrChunk];
        return chunk.parser.last_err_pos;
    }

    pub fn allocLastErrorReport(self: *UserVM) ![]const u8 {
        return self.internal().allocLastErrorReport();
    }

    pub fn allocLastUserCompileError(self: *const UserVM) ![]const u8 {
        return debug.allocLastUserCompileError(self.constInternal());
    }

    pub fn getCompileErrorMsg(self: *const UserVM) []const u8 {
        return self.constInternal().compiler.lastErr;
    }

    pub fn allocPanicMsg(self: *const UserVM) ![]const u8 {
        return cy.debug.allocPanicMsg(self.constInternal());
    }

    pub fn allocLastUserPanicError(self: *const UserVM) ![]const u8 {
        return cy.debug.allocLastUserPanicError(self.constInternal());
    }

    pub fn printLastUserPanicError(self: *const UserVM) !void {
        try cy.debug.printLastUserPanicError(self.constInternal());
    }

    pub fn dumpInfo(self: *UserVM) !void {
        try self.internal().dumpInfo();
    }

    pub fn dumpStats(self: *UserVM) void {
        self.internal().dumpStats();
    }

    pub fn fillUndefinedStackSpace(self: *UserVM, val: Value) void {
        @memset(self.internal().stack, val);
    }

    pub inline fn releaseObject(self: *UserVM, obj: *cy.HeapObject) void {
        cy.arc.releaseObject(self.internal(), obj);
    }

    pub inline fn release(self: *UserVM, val: Value) void {
        cy.arc.release(self.internal(), val);
    }

    pub inline fn retain(self: *UserVM, val: Value) void {
        cy.arc.retain(self.internal(), val);
    }

    pub inline fn retainObject(self: *UserVM, obj: *cy.HeapObject) void {
        cy.arc.retainObject(self.internal(), obj);
    }

    pub inline fn getGlobalRC(self: *const UserVM) usize {
        return cy.arc.getGlobalRC(@ptrCast(self));
    }

    pub inline fn validate(self: *UserVM, srcUri: []const u8, src: []const u8) !cy.ValidateResult {
        return self.internal().validate(srcUri, src, .{ .enableFileModules = true });
    }

    pub inline fn compile(self: *UserVM, srcUri: []const u8, src: []const u8) !cy.CompileResultView {
        return self.internal().compile(srcUri, src, .{ .enableFileModules = true });
    }

    pub inline fn eval(self: *UserVM, srcUri: []const u8, src: []const u8, config: cy.EvalConfig) !Value {
        return self.internal().eval(srcUri, src, config);
    }

    pub inline fn allocator(self: *const UserVM) std.mem.Allocator {
        return @as(*const VM, @ptrCast(self)).alloc;
    }

    pub inline fn allocEmptyList(self: *UserVM) !Value {
        return cy.heap.allocEmptyList(self.internal());
    }

    pub inline fn allocEmptyMap(self: *UserVM) !Value {
        return cy.heap.allocEmptyMap(self.internal());
    }

    pub inline fn allocList(self: *UserVM, elems: []const Value) !Value {
        return cy.heap.allocList(self.internal(), elems);
    }

    pub inline fn allocListFill(self: *UserVM, val: Value, n: u32) !Value {
        return cy.heap.allocListFill(self.internal(), val, n);
    }

    pub inline fn allocUnsetUstringObject(self: *UserVM, len: usize, charLen: u32) !*cy.HeapObject {
        return cy.heap.allocUnsetUstringObject(self.internal(), len, charLen);
    }

    pub inline fn allocUnsetAstringObject(self: *UserVM, len: usize) !*cy.HeapObject {
        return cy.heap.allocUnsetAstringObject(self.internal(), len);
    }

    pub inline fn allocUnsetArrayObject(self: *UserVM, len: usize) !*cy.HeapObject {
        return cy.heap.allocUnsetArrayObject(self.internal(), len);
    }

    pub inline fn allocArray(self: *UserVM, str: []const u8) !Value {
        return cy.heap.allocArray(self.internal(), str);
    }

    pub inline fn retainOrAllocAstring(self: *UserVM, str: []const u8) !Value {
        return cy.heap.retainOrAllocAstring(self.internal(), str);
    }

    pub inline fn retainOrAllocUstring(self: *UserVM, str: []const u8, charLen: u32) !Value {
        return cy.heap.retainOrAllocUstring(self.internal(), str, charLen);
    }

    pub inline fn allocStringNoIntern(self: *UserVM, str: []const u8, utf8: bool) !Value {
        return cy.heap.allocString(self.internal(), str, utf8);
    }

    pub inline fn retainOrAllocString(self: *UserVM, str: []const u8) !Value {
        return cy.heap.retainOrAllocPreferString(self.internal(), str);
    }

    pub inline fn allocArraySlice(self: *UserVM, slice: []const u8, parent: *cy.HeapObject) !Value {
        return cy.heap.allocArraySlice(self.internal(), slice, parent);
    }

    pub inline fn allocStringOrByteArray(self: *UserVM, str: []const u8) !Value {
        if (cy.string.validateUtf8(str)) |runeLen| {
            if (runeLen == str.len) {
                return self.retainOrAllocAstring(str);
            } else {
                return self.retainOrAllocUstring(str, @intCast(runeLen));
            }
        } else {
            return self.allocArray(str);
        }
    }

    pub inline fn allocAstringSlice(self: *UserVM, slice: []const u8, parent: *cy.HeapObject) !Value {
        return cy.heap.allocAstringSlice(self.internal(), slice, parent);
    }

    pub inline fn allocUstringSlice(self: *UserVM, slice: []const u8, charLen: u32, parent: ?*cy.HeapObject) !Value {
        return cy.heap.allocUstringSlice(self.internal(), slice, charLen, parent);
    }

    pub inline fn allocOwnedAstring(self: *UserVM, str: *cy.HeapObject) !Value {
        return cy.heap.getOrAllocOwnedAstring(self.internal(), str);
    }

    pub inline fn allocOwnedUstring(self: *UserVM, str: *cy.HeapObject) !Value {
        return cy.heap.getOrAllocOwnedUstring(self.internal(), str);
    }

    pub inline fn allocArrayConcat(self: *UserVM, left: []const u8, right: []const u8) !Value {
        return cy.heap.allocArrayConcat(self.internal(), left, right);
    }

    pub inline fn allocAstringConcat3(self: *UserVM, str1: []const u8, str2: []const u8, str3: []const u8) !Value {
        return cy.heap.getOrAllocAstringConcat3(self.internal(), str1, str2, str3);
    }

    pub inline fn allocUstringConcat3(self: *UserVM, str1: []const u8, str2: []const u8, str3: []const u8, charLen: u32) !Value {
        return cy.heap.getOrAllocUstringConcat3(self.internal(), str1, str2, str3, charLen);
    }

    pub inline fn allocAstringConcat(self: *UserVM, left: []const u8, right: []const u8) !Value {
        return cy.heap.getOrAllocAstringConcat(self.internal(), left, right);
    }

    pub inline fn allocUstringConcat(self: *UserVM, left: []const u8, right: []const u8, charLen: u32) !Value {
        return cy.heap.getOrAllocUstringConcat(self.internal(), left, right, charLen);
    }

    pub inline fn allocObjectSmall(self: *UserVM, sid: cy.TypeId, fields: []const Value) !Value {
        return cy.heap.allocObjectSmall(self.internal(), sid, fields);
    }

    pub inline fn allocObject(self: *UserVM, sid: cy.TypeId, fields: []const Value) !Value {
        return self.internal().allocObject(sid, fields);
    }

    pub inline fn allocListIterator(self: *UserVM, list: *cy.CyList) !Value {
        return cy.heap.allocListIterator(self.internal(), list);
    }

    pub inline fn allocMapIterator(self: *UserVM, map: *cy.Map) !Value {
        return cy.heap.allocMapIterator(self.internal(), map);
    }

    pub inline fn allocDir(self: *UserVM, fd: std.os.fd_t, iterable: bool) !Value {
        return fs.allocDir(self.internal(), fd, iterable);
    }

    pub inline fn allocDirIterator(self: *UserVM, dir: Value, recursive: bool) !Value {
        return fs.allocDirIterator(self.internal(), dir, recursive);
    }

    pub inline fn allocFile(self: *UserVM, fd: std.os.fd_t) !Value {
        return fs.allocFile(self.internal(), fd);
    }

    pub inline fn getOrWriteValueString(self: *UserVM, writer: anytype, val: Value, charLen: *u32) []const u8 {
        return self.internal().getOrWriteValueString(writer, val, charLen, true);
    }

    pub inline fn valueToTempByteArray(self: *const UserVM, val: Value) []const u8 {
        return self.constInternal().valueToTempByteArray(val);
    }

    pub inline fn valueToTempString(self: *UserVM, val: Value) []const u8 {
        return self.constInternal().valueToTempString(val);
    }

    pub inline fn valueToTempString2(self: *UserVM, val: Value, outCharLen: *u32) []const u8 {
        return @as(*const VM, @ptrCast(self)).valueToTempString2(val, outCharLen);
    }

    pub inline fn valueToNextTempString(self: *UserVM, val: Value) []const u8 {
        return @as(*const VM, @ptrCast(self)).valueToNextTempString(val);
    }

    pub inline fn valueToNextTempString2(self: *UserVM, val: Value, outCharLen: *u32) []const u8 {
        return @as(*const VM, @ptrCast(self)).valueToNextTempString2(val, outCharLen);
    }

    pub inline fn valueToString(self: *UserVM, val: Value) ![]const u8 {
        return @as(*const VM, @ptrCast(self)).valueToString(val);
    }

    pub inline fn mapRawSet(self: *UserVM, map: cy.Value, key: cy.Value, value: cy.Value) !void {
        try map.asHeapObject().map.map().put(self.allocator(), key, value);
    }

    pub inline fn listAppend(self: *UserVM, list: cy.Value, value: cy.Value) !void {
        try list.asHeapObject().list.getList().append(self.allocator(), value);
    }

    /// Used to return a panic from a native function body.
    pub fn returnPanic(self: *UserVM, msg: []const u8) Value {
        @setCold(true);
        const vm = self.internal();
        const dupe = vm.alloc.dupe(u8, msg) catch cy.fatal();
        vm.curFiber.panicPayload = @as(u64, @intCast(@intFromPtr(dupe.ptr))) | (@as(u64, dupe.len) << 48);
        vm.curFiber.panicType = vmc.PANIC_MSG;
        return Value.Interrupt;
    }

    pub fn prepareThrowSymbol(self: *UserVM, id: u8) Value {
        const vm = self.internal();
        vm.curFiber.panicPayload = Value.initErrorSymbol(id).val;
        vm.curFiber.panicType = vmc.PANIC_NATIVE_THROW;
        return Value.Interrupt;
    }

    pub fn getNewFramePtrOffset(self: *UserVM, args: [*]const Value, nargs: u8) u32 {
        const vm: *const VM = @ptrCast(self);
        return @intCast(cy.fiber.getStackOffset(vm.stack.ptr, args + nargs));
    }

    pub fn callFunc(self: *UserVM, framePtr: u32, func: Value, args: []const Value) !Value {
        const vm = self.internal();

        try cy.fiber.ensureTotalStackCapacity(vm, framePtr + args.len + 1 + 4);
        vm.framePtr = vm.stack.ptr + framePtr;

        self.retain(func);
        vm.framePtr[4 + args.len] = func;
        for (args, 0..) |arg, i| {
            self.retain(arg);
            vm.framePtr[4 + i] = arg;
        }
        defer {
            self.release(func);
            for (args) |arg| {
                self.release(arg);
            }
        }

        const retInfo = cy.vm.buildReturnInfo(1, false, 0);
        try cy.vm.callNoInline(vm, &vm.pc, &vm.framePtr, func, 0, @intCast(args.len), retInfo);
        try @call(.never_inline, cy.vm.evalLoopGrowStack, .{vm});

        const res = vm.framePtr[0];
        return res;
    }
};

test "api internals." {
    try t.eq(@alignOf(UserVM), @alignOf(UserVM));
}