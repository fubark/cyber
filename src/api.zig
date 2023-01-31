// Copyright (c) 2023 Cyber (See LICENSE)

/// Simplified API for user binded functions and Zig embedded projects.
/// For the C API, see `lib.zig` which exports declarations defined by `cyber.h`.

const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const fmt = @import("fmt.zig");
const VM = cy.VM;
const Value = cy.Value;

const UserVMAlign = 8;

/// A simplified VM handle.
pub const UserVM = struct {
    dummy: u64 align(UserVMAlign) = undefined,

    pub fn init(self: *UserVM, alloc: std.mem.Allocator) !void {
        try @ptrCast(*VM, self).init(alloc);
    }

    pub fn deinit(self: *UserVM) void {
        @ptrCast(*VM, self).deinit();
    }

    pub fn setTrace(self: *UserVM, trace: *cy.TraceInfo) void {
        if (!cy.TraceEnabled) {
            return;
        }
        @ptrCast(*VM, self).trace = trace;
    }

    pub fn getStackTrace(self: *UserVM) *const cy.StackTrace {
        return @ptrCast(*const VM, self).getStackTrace();
    }

    pub fn getParserErrorMsg(self: *const UserVM) []const u8 {
        const vm = @ptrCast(*const VM, self);
        const chunk = vm.compiler.chunks.items[vm.compiler.lastErrChunk];
        return chunk.parser.last_err;
    }

    pub fn getCompileErrorMsg(self: *const UserVM) []const u8 {
        return @ptrCast(*const VM, self).compiler.lastErr;
    }

    pub fn allocPanicMsg(self: *const UserVM) ![]const u8 {
        return cy.debug.allocPanicMsg(@ptrCast(*const VM, self));
    }

    pub fn dumpPanicStackTrace(self: *UserVM) !void {
        @setCold(true);
        const vm = @ptrCast(*VM, self);
        const msg = try self.allocPanicMsg();
        defer vm.alloc.free(msg);
        fmt.printStderr("panic: {}\n\n", &.{fmt.v(msg)});
        const trace = vm.getStackTrace();
        try trace.dump(vm);
    }

    pub fn dumpInfo(self: *UserVM) void {
        @ptrCast(*VM, self).dumpInfo();
    }

    pub fn dumpStats(self: *UserVM) void {
        @ptrCast(*VM, self).dumpStats();
    }

    pub fn fillUndefinedStackSpace(self: *UserVM, val: Value) void {
        std.mem.set(Value, @ptrCast(*VM, self).stack, val);
    }

    pub inline fn releaseObject(self: *UserVM, obj: *cy.HeapObject) void {
        cy.arc.releaseObject(@ptrCast(*VM, self), obj);
    }

    pub inline fn release(self: *UserVM, val: Value) void {
        cy.arc.release(@ptrCast(*VM, self), val);
    }

    pub inline fn retain(self: *UserVM, val: Value) void {
        cy.arc.retain(@ptrCast(*VM, self), val);
    }

    pub inline fn retainObject(self: *UserVM, obj: *cy.HeapObject) void {
        cy.arc.retainObject(@ptrCast(*VM, self), obj);
    }

    pub inline fn getGlobalRC(self: *const UserVM) usize {
        return cy.arc.getGlobalRC(@ptrCast(*const VM, self));
    }

    pub inline fn checkMemory(self: *UserVM) !bool {
        return cy.arc.checkMemory(@ptrCast(*VM, self));
    }

    pub inline fn compile(self: *UserVM, srcUri: []const u8, src: []const u8) !cy.ByteCodeBuffer {
        return @ptrCast(*VM, self).compile(srcUri, src);
    }

    pub inline fn eval(self: *UserVM, srcUri: []const u8, src: []const u8, config: cy.EvalConfig) !Value {
        return @ptrCast(*VM, self).eval(srcUri, src, config);
    }

    pub inline fn allocator(self: *const UserVM) std.mem.Allocator {
        return @ptrCast(*const VM, self).alloc;
    }

    pub inline fn allocEmptyList(self: *UserVM) !Value {
        return cy.heap.allocEmptyList(@ptrCast(*VM, self));
    }

    pub inline fn allocEmptyMap(self: *UserVM) !Value {
        return cy.heap.allocEmptyMap(@ptrCast(*VM, self));
    }

    pub inline fn allocList(self: *UserVM, elems: []const Value) !Value {
        return cy.heap.allocList(@ptrCast(*VM, self), elems);
    }

    pub inline fn allocListFill(self: *UserVM, val: Value, n: u32) !Value {
        return cy.heap.allocListFill(@ptrCast(*VM, self), val, n);
    }

    pub inline fn allocUnsetUstringObject(self: *UserVM, len: usize, charLen: u32) !*cy.HeapObject {
        return cy.heap.allocUnsetUstringObject(@ptrCast(*VM, self), len, charLen);
    }

    pub inline fn allocUnsetAstringObject(self: *UserVM, len: usize) !*cy.HeapObject {
        return cy.heap.allocUnsetAstringObject(@ptrCast(*VM, self), len);
    }

    pub inline fn allocUnsetRawStringObject(self: *UserVM, len: usize) !*cy.HeapObject {
        return cy.heap.allocUnsetRawStringObject(@ptrCast(*VM, self), len);
    }

    pub inline fn allocRawString(self: *UserVM, str: []const u8) !Value {
        return cy.heap.allocRawString(@ptrCast(*VM, self), str);
    }

    pub inline fn allocAstring(self: *UserVM, str: []const u8) !Value {
        return cy.heap.getOrAllocAstring(@ptrCast(*VM, self), str);
    }

    pub inline fn allocUstring(self: *UserVM, str: []const u8, charLen: u32) !Value {
        return cy.heap.getOrAllocUstring(@ptrCast(*VM, self), str, charLen);
    }

    pub inline fn allocStringNoIntern(self: *UserVM, str: []const u8, utf8: bool) !Value {
        return cy.heap.allocString(@ptrCast(*VM, self), str, utf8);
    }

    pub inline fn allocStringInfer(self: *UserVM, str: []const u8) !Value {
        return cy.heap.getOrAllocStringInfer(@ptrCast(*VM, self), str);
    }

    pub inline fn allocRawStringSlice(self: *UserVM, slice: []const u8, parent: *cy.HeapObject) !Value {
        return cy.heap.allocRawStringSlice(@ptrCast(*VM, self), slice, parent);
    }

    pub inline fn allocAstringSlice(self: *UserVM, slice: []const u8, parent: *cy.HeapObject) !Value {
        return cy.heap.allocAstringSlice(@ptrCast(*VM, self), slice, parent);
    }

    pub inline fn allocUstringSlice(self: *UserVM, slice: []const u8, charLen: u32, parent: ?*cy.HeapObject) !Value {
        return cy.heap.allocUstringSlice(@ptrCast(*VM, self), slice, charLen, parent);
    }

    pub inline fn allocOwnedAstring(self: *UserVM, str: *cy.HeapObject) !Value {
        return cy.heap.getOrAllocOwnedAstring(@ptrCast(*VM, self), str);
    }

    pub inline fn allocOwnedUstring(self: *UserVM, str: *cy.HeapObject) !Value {
        return cy.heap.getOrAllocOwnedUstring(@ptrCast(*VM, self), str);
    }

    pub inline fn allocRawStringConcat(self: *UserVM, left: []const u8, right: []const u8) !Value {
        return cy.heap.allocRawStringConcat(@ptrCast(*VM, self), left, right);
    }

    pub inline fn allocAstringConcat3(self: *UserVM, str1: []const u8, str2: []const u8, str3: []const u8) !Value {
        return cy.heap.getOrAllocAstringConcat3(@ptrCast(*VM, self), str1, str2, str3);
    }

    pub inline fn allocUstringConcat3(self: *UserVM, str1: []const u8, str2: []const u8, str3: []const u8, charLen: u32) !Value {
        return cy.heap.getOrAllocUstringConcat3(@ptrCast(*VM, self), str1, str2, str3, charLen);
    }

    pub inline fn allocAstringConcat(self: *UserVM, left: []const u8, right: []const u8) !Value {
        return cy.heap.getOrAllocAstringConcat(@ptrCast(*VM, self), left, right);
    }

    pub inline fn allocUstringConcat(self: *UserVM, left: []const u8, right: []const u8, charLen: u32) !Value {
        return cy.heap.getOrAllocUstringConcat(@ptrCast(*VM, self), left, right, charLen);
    }

    pub inline fn allocObjectSmall(self: *UserVM, sid: cy.TypeId, fields: []const Value) !Value {
        return cy.heap.allocObjectSmall(@ptrCast(*VM, self), sid, fields);
    }

    pub inline fn allocObject(self: *UserVM, sid: cy.TypeId, fields: []const Value) !Value {
        return @ptrCast(*VM, self).allocObject(sid, fields);
    }

    pub inline fn allocListIterator(self: *UserVM, list: *cy.CyList) !Value {
        return cy.heap.allocListIterator(@ptrCast(*VM, self), list);
    }

    pub inline fn allocMapIterator(self: *UserVM, map: *cy.Map) !Value {
        return cy.heap.allocMapIterator(@ptrCast(*VM, self), map);
    }

    pub inline fn allocDir(self: *UserVM, fd: std.os.fd_t, iterable: bool) !Value {
        return cy.heap.allocDir(@ptrCast(*VM, self), fd, iterable);
    }

    pub inline fn allocDirIterator(self: *UserVM, dir: *cy.Dir, recursive: bool) !Value {
        return cy.heap.allocDirIterator(@ptrCast(*VM, self), dir, recursive);
    }

    pub inline fn allocFile(self: *UserVM, fd: std.os.fd_t) !Value {
        return cy.heap.allocFile(@ptrCast(*VM, self), fd);
    }

    pub inline fn valueAsString(self: *UserVM, val: Value) []const u8 {
        return @ptrCast(*const VM, self).valueAsString(val);
    }

    pub inline fn getOrWriteValueString(self: *UserVM, writer: anytype, val: Value, charLen: *u32) []const u8 {
        return @ptrCast(*const VM, self).getOrWriteValueString(writer, val, charLen, true);
    }

    pub inline fn valueToTempString(self: *UserVM, val: Value) []const u8 {
        return @ptrCast(*const VM, self).valueToTempString(val);
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
        const vm = @ptrCast(*VM, self);
        const dupe = vm.alloc.dupe(u8, msg) catch stdx.fatal();
        vm.panicPayload = @intCast(u64, @ptrToInt(dupe.ptr)) | (@as(u64, dupe.len) << 48);
        vm.panicType = .msg;
        return Value.Panic;
    }

    pub inline fn getStaticUstringHeader(self: *UserVM, start: u32) *align (1) cy.StaticUstringHeader {
        return cy.string.getStaticUstringHeader(@ptrCast(*VM, self), start);
    }

    pub inline fn getStaticString(self: *UserVM, start: u32, end: u32) []const u8 {
        return @ptrCast(*VM, self).strBuf[start..end];
    }

    pub inline fn getStaticStringChar(self: *UserVM, idx: u32) u8 {
        return @ptrCast(*VM, self).strBuf[idx];
    }

    pub fn getNewFramePtrOffset(self: *UserVM, args: [*]const Value) u32 {
        const vm = @ptrCast(*const VM, self);
        return @intCast(u32, cy.vm.framePtrOffsetFrom(vm.stack.ptr, args));
    }

    pub fn callFunc(self: *UserVM, framePtr: u32, func: Value, args: []const Value) !Value {
        const vm = @ptrCast(*VM, self);

        try vm.ensureTotalStackCapacity(framePtr + args.len + 1 + 4);
        const saveFramePtrOffset = cy.vm.framePtrOffsetFrom(vm.stack.ptr, vm.framePtr);
        vm.framePtr = vm.stack.ptr + framePtr;

        self.retain(func);
        defer self.release(func);
        vm.framePtr[4 + args.len] = func;
        for (args) |arg, i| {
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