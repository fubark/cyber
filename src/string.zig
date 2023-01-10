const std = @import("std");
const cy = @import("cyber.zig");

/// Like `ArrayList` except the buffer is allocated as a `Astring` or `Ustring`.
pub const HeapStringBuilder = struct {
    buf: []u8,
    len: u32,
    isAstring: bool,
    hasObject: bool,
    vm: *cy.VM,

    /// Starts as an Astring heap pool object.
    pub fn init(vm: *cy.VM) !HeapStringBuilder {
        const obj = try vm.allocPoolObject();
        obj.astring = .{
            .structId = cy.AstringT,
            .rc = 1,
            .len = cy.MaxPoolObjectStringByteLen,
            .bufStart = undefined,
        };
        return .{
            .buf = obj.astring.getSlice(),
            .len = 0,
            .isAstring = true,
            .hasObject = true,
            .vm = vm,
        };
    }

    fn getHeapObject(self: *const HeapStringBuilder) *cy.HeapObject {
        return @ptrCast(*cy.HeapObject, @alignCast(@alignOf(cy.HeapObject), self.buf.ptr - @offsetOf(cy.Astring, "bufStart")));
    }

    pub fn deinit(self: *HeapStringBuilder) void {
        if (self.hasObject) {
            const obj = self.getHeapObject();
            obj.astring.len = self.len;
            self.vm.freeObject(obj);
            self.hasObject = false;
        }
    }

    pub fn ownObject(self: *HeapStringBuilder) *cy.HeapObject {
        const obj = self.getHeapObject();
        obj.astring.len = self.len;
        self.hasObject = false;
        return obj;
    }

    pub fn appendString(self: *HeapStringBuilder, alloc: std.mem.Allocator, str: []const u8, utf8: bool) !void {
        try self.ensureTotalCapacity(alloc, self.len + str.len);
        const oldLen = self.len;
        self.len += @intCast(u32, str.len);
        std.mem.copy(u8, self.buf[oldLen..self.len], str);
        if (self.isAstring and utf8) {
            // Upgrade to Ustring.
            const obj = self.getHeapObject();
            obj.common.structId = cy.UstringT;
            self.isAstring = false;
        }
    }

    pub inline fn ensureTotalCapacity(self: *HeapStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (newCap > self.buf.len) {
            try self.growTotalCapacity(alloc, newCap);
        }
    }

    pub fn growTotalCapacityPrecise(self: *HeapStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        if (self.buf.len > cy.MaxPoolObjectStringByteLen) {
            const oldHead = (self.buf.ptr - 16)[0..self.buf.len + 16];
            if (alloc.resize(oldHead, 16 + newCap)) {
                self.buf.len = newCap;
            } else {
                const old = self.buf;
                const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), 16 + newCap);
                const obj = @ptrCast(*cy.HeapObject, objSlice.ptr);
                obj.astring = .{
                    .structId = if (self.isAstring) cy.AstringT else cy.UstringT,
                    .rc = 1,
                    .len = 0,
                    .bufStart = undefined,
                };
                self.buf = objSlice[16..newCap];
                std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);
                alloc.free((old.ptr - 16)[0..old.len + 16]);
            }
        } else {
            const oldObj = self.getHeapObject();
            const old = self.buf;
            const objSlice = try alloc.alignedAlloc(u8, @alignOf(cy.HeapObject), 16 + newCap);
            const obj = @ptrCast(*cy.HeapObject, objSlice.ptr);
            obj.astring = .{
                .structId = if (self.isAstring) cy.AstringT else cy.UstringT,
                .rc = 1,
                .len = 0,
                .bufStart = undefined,
            };
            self.buf = objSlice[16..newCap];
            std.mem.copy(u8, self.buf[0..self.len], old[0..self.len]);

            // Free pool object.
            oldObj.astring.len = self.len;
            self.vm.freeObject(oldObj);
        }
    }

    pub fn growTotalCapacity(self: *HeapStringBuilder, alloc: std.mem.Allocator, newCap: usize) !void {
        var betterCap = self.buf.len;
        while (true) {
            betterCap +|= betterCap / 2 + 8;
            if (betterCap >= newCap) {
                break;
            }
        }
        try self.growTotalCapacityPrecise(alloc, betterCap);
    }
};