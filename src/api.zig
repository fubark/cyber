// Copyright (c) 2023 Cyber (See LICENSE)

/// Simplified API for user binded functions and Zig embedded projects.
/// For the C API, see `lib.zig` which exports declarations defined by `cyber.h`.

const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");
const debug = @import("debug.zig");
const VM = cy.VM;
const Value = cy.Value;

const UserVMAlign = 8;

/// A simplified VM handle.
pub const UserVM = struct {
    dummy: u64 align(UserVMAlign) = undefined,

    pub fn init(self: *UserVM, alloc: std.mem.Allocator) !void {
        try self.internal().init(alloc);
    }

    pub fn deinit(self: *UserVM) void {
        self.internal().deinit(false);
    }

    pub inline fn internal(self: *UserVM) *cy.VM {
        return @ptrCast(*cy.VM, self);
    }

    pub inline fn constInternal(self: *const UserVM) *const cy.VM {
        return @ptrCast(*const cy.VM, self);
    }

    pub fn ensureUntypedFuncSig(self: *UserVM, numParams: u32) !cy.sema.ResolvedFuncSigId {
        return cy.sema.ensureResolvedUntypedFuncSig(&self.internal().compiler, numParams);
    }

    pub fn getUserData(self: *UserVM) ?*anyopaque {
        return self.internal().userData;
    }

    pub fn setUserData(self: *UserVM, userData: ?*anyopaque) void {
        self.internal().userData = userData;
    }

    pub fn addModuleLoader(self: *UserVM, absSpec: []const u8, func: cy.ModuleLoaderFunc) !void {
        return self.internal().compiler.addModuleLoader(absSpec, func);
    }

    pub fn setTrace(self: *UserVM, trace: *cy.TraceInfo) void {
        if (!cy.TraceEnabled) {
            return;
        }
        self.internal().trace = trace;
    }

    pub fn getStackTrace(self: *UserVM) *const cy.StackTrace {
        return @ptrCast(*const VM, self).getStackTrace();
    }

    pub fn getParserErrorMsg(self: *const UserVM) []const u8 {
        const vm = @ptrCast(*const VM, self);
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
        std.mem.set(Value, self.internal().stack, val);
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
        return cy.arc.getGlobalRC(@ptrCast(*const VM, self));
    }

    pub inline fn checkMemory(self: *UserVM) !bool {
        return cy.arc.checkMemory(self.internal());
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
        return @ptrCast(*const VM, self).alloc;
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

    pub inline fn allocUnsetRawStringObject(self: *UserVM, len: usize) !*cy.HeapObject {
        return cy.heap.allocUnsetRawStringObject(self.internal(), len);
    }

    pub inline fn allocRawString(self: *UserVM, str: []const u8) !Value {
        return cy.heap.allocRawString(self.internal(), str);
    }

    pub inline fn allocAstring(self: *UserVM, str: []const u8) !Value {
        return cy.heap.getOrAllocAstring(self.internal(), str);
    }

    pub inline fn allocUstring(self: *UserVM, str: []const u8, charLen: u32) !Value {
        return cy.heap.getOrAllocUstring(self.internal(), str, charLen);
    }

    pub inline fn allocStringNoIntern(self: *UserVM, str: []const u8, utf8: bool) !Value {
        return cy.heap.allocString(self.internal(), str, utf8);
    }

    pub inline fn allocStringInfer(self: *UserVM, str: []const u8) !Value {
        return cy.heap.getOrAllocStringInfer(self.internal(), str);
    }

    pub inline fn allocRawStringSlice(self: *UserVM, slice: []const u8, parent: *cy.HeapObject) !Value {
        return cy.heap.allocRawStringSlice(self.internal(), slice, parent);
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

    pub inline fn allocRawStringConcat(self: *UserVM, left: []const u8, right: []const u8) !Value {
        return cy.heap.allocRawStringConcat(self.internal(), left, right);
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
        return cy.heap.allocDir(self.internal(), fd, iterable);
    }

    pub inline fn allocDirIterator(self: *UserVM, dir: *cy.Dir, recursive: bool) !Value {
        return cy.heap.allocDirIterator(self.internal(), dir, recursive);
    }

    pub inline fn allocFile(self: *UserVM, fd: std.os.fd_t) !Value {
        return cy.heap.allocFile(self.internal(), fd);
    }

    pub inline fn valueAsString(self: *UserVM, val: Value) []const u8 {
        return @ptrCast(*const VM, self).valueAsString(val);
    }

    pub inline fn getOrWriteValueString(self: *UserVM, writer: anytype, val: Value, charLen: *u32) []const u8 {
        return self.internal().getOrWriteValueString(writer, val, charLen, true);
    }

    pub inline fn valueToTempRawString(self: *const UserVM, val: Value) []const u8 {
        return self.constInternal().valueToTempRawString(val);
    }

    pub inline fn valueToTempString(self: *UserVM, val: Value) []const u8 {
        return self.constInternal().valueToTempString(val);
    }

    pub inline fn valueToTempString2(self: *UserVM, val: Value, outCharLen: *u32) []const u8 {
        return @ptrCast(*const VM, self).valueToTempString2(val, outCharLen);
    }

    pub inline fn valueToNextTempString(self: *UserVM, val: Value) []const u8 {
        return @ptrCast(*const VM, self).valueToNextTempString(val);
    }

    pub inline fn valueToNextTempString2(self: *UserVM, val: Value, outCharLen: *u32) []const u8 {
        return @ptrCast(*const VM, self).valueToNextTempString2(val, outCharLen);
    }

    pub inline fn valueToString(self: *UserVM, val: Value) ![]const u8 {
        return @ptrCast(*const VM, self).valueToString(val);
    }

    /// Used to return a panic from a native function body.
    pub fn returnPanic(self: *UserVM, msg: []const u8) Value {
        @setCold(true);
        const vm = self.internal();
        const dupe = vm.alloc.dupe(u8, msg) catch stdx.fatal();
        vm.panicPayload = @intCast(u64, @ptrToInt(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        vm.panicType = .msg;
        return Value.Interrupt;
    }

    pub fn prepareThrowSymbol(self: *UserVM, id: u8) Value {
        const vm = self.internal();
        vm.panicPayload = Value.initErrorSymbol(id).val;
        vm.panicType = .nativeThrow;
        return Value.Interrupt;
    }

    pub inline fn getStaticUstringHeader(self: *UserVM, start: u32) *align (1) cy.StaticUstringHeader {
        return cy.string.getStaticUstringHeader(self.internal(), start);
    }

    pub inline fn getStaticString(self: *UserVM, start: u32, end: u32) []const u8 {
        return self.internal().strBuf[start..end];
    }

    pub inline fn getStaticStringChar(self: *UserVM, idx: u32) u8 {
        return self.internal().strBuf[idx];
    }

    pub fn getNewFramePtrOffset(self: *UserVM, args: [*]const Value) u32 {
        const vm = @ptrCast(*const VM, self);
        return @intCast(u32, cy.fiber.getStackOffset(vm.stack.ptr, args));
    }

    pub fn callFunc(self: *UserVM, framePtr: u32, func: Value, args: []const Value) !Value {
        const vm = self.internal();

        try cy.fiber.ensureTotalStackCapacity(vm, framePtr + args.len + 1 + 4);
        const saveFramePtrOffset = cy.fiber.getStackOffset(vm.stack.ptr, vm.framePtr);
        vm.framePtr = vm.stack.ptr + framePtr;

        self.retain(func);
        defer self.release(func);
        vm.framePtr[4 + args.len] = func;
        for (args, 0..) |arg, i| {
            self.retain(arg);
            vm.framePtr[4 + i] = arg;
        }
        const retInfo = cy.vm.buildReturnInfo(1, false);
        try cy.vm.callNoInline(vm, &vm.pc, &vm.framePtr, func, 0, @intCast(u8, args.len), retInfo);
        try @call(.never_inline, cy.vm.evalLoopGrowStack, .{vm});

        const res = vm.framePtr[0];

        // Restore framePtr.
        vm.framePtr = vm.stack.ptr + saveFramePtrOffset;

        return res;
    }
};

test "Internals." {
    try t.eq(@alignOf(UserVM), UserVMAlign);
}