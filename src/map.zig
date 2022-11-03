const std = @import("std");
const stdx = @import("stdx");
const builtin = @import("builtin");
const debug = builtin.mode == .Debug;

const cy = @import("cyber.zig");
const log = stdx.log.scoped(.map);

/// The implementation of ValueMap is based on Zig's std.HashMapUnmanaged.
/// Since keys and values are just cy.Values (8 bytes each), the memory layout can be made more compact.
/// This also allows inserting optimizations where needed.
pub const ValueMap = struct {
    metadata: ?[*] align(8) Metadata = null,
    entries: ?[*]ValueMapEntry = null,
    size: u32 = 0,
    cap: u32 = 0,
    available: u32 = 0,

    /// Not used internally.
    extra: u32 = undefined,

    const MaxLoadPercentage = 80;

    pub fn iterator(self: *const ValueMap) Iterator {
        return Iterator{ .map = self };
    }

    pub fn put(self: *ValueMap, alloc: std.mem.Allocator, vm: *const cy.VM, key: cy.Value, value: cy.Value) linksection(".core") std.mem.Allocator.Error!void {
        const res = try self.getOrPut(alloc, vm, key);
        res.valuePtr.* = value;
    }

    pub fn get(self: ValueMap, vm: *const cy.VM, key: cy.Value) linksection(".core") ?cy.Value {
        @setRuntimeSafety(debug);
        if (self.getIndex(vm, key)) |idx| {
            return self.entries.?[idx].value;
        }
        return null;
    }

    pub fn getByString(self: ValueMap, vm: *const cy.VM, key: []const u8) linksection(".core") ?cy.Value {
        @setRuntimeSafety(debug);
        if (self.getIndexByString(vm, key)) |idx| {
            return self.entries.?[idx].value;
        }
        return null;
    }

    // This counts the number of occupied slots (not counting tombstones), which is
    // what has to stay under the max_load_percentage of capacity.
    fn load(self: *const ValueMap) u32 {
        const maxLoad = (self.cap * MaxLoadPercentage) / 100;
        std.debug.assert(maxLoad >= self.available);
        return @truncate(u32, maxLoad - self.available);
    }

    pub fn getOrPut(self: *ValueMap, alloc: std.mem.Allocator, vm: *const cy.VM, key: cy.Value) linksection(".core") std.mem.Allocator.Error!GetOrPutResult {
        const res = try self.getOrPutAdapted(alloc, vm, key);
        if (!res.foundExisting) {
            res.keyPtr.* = key;
        }
        return res;
    }

    pub fn getOrPutAdapted(self: *ValueMap, alloc: std.mem.Allocator, vm: *const cy.VM, key: cy.Value) linksection(".core") std.mem.Allocator.Error!GetOrPutResult {
        if (self.available == 0) {
            try self.grow(alloc, vm, capacityForSize(self.load() + 1));
        }
        return self.getOrPutAssumeCapacityAdapted(vm, key);
    }

    pub fn getOrPutAssumeCapacityAdapted(self: *ValueMap, vm: *const cy.VM, key: cy.Value) linksection(".core") GetOrPutResult {
        const hash = computeHash(vm, key);
        const mask = self.cap - 1;
        const fingerprint = Metadata.takeFingerprint(hash);
        var limit = self.cap;
        var idx = @truncate(usize, hash & mask);

        var first_tombstone_idx: usize = self.cap; // invalid index
        var md = self.metadata.?[idx];
        while (!md.isFree() and limit != 0) {
            if (md.isUsed() and md.fingerprint == fingerprint) {
                const testKey = self.entries.?[idx].key;
                if (keysEqual(vm, key, testKey)) {
                    return GetOrPutResult{
                        .keyPtr = &self.entries.?[idx].key,
                        .valuePtr = &self.entries.?[idx].value,
                        .foundExisting = true,
                    };
                }
            } else if (first_tombstone_idx == self.cap and md.isTombstone()) {
                first_tombstone_idx = idx;
            }

            limit -= 1;
            idx = (idx + 1) & mask;
            md = self.metadata.?[idx];
        }

        if (first_tombstone_idx < self.cap) {
            // Cheap try to lower probing lengths after deletions. Recycle a tombstone.
            idx = first_tombstone_idx;
            md = self.metadata.?[idx];
        }
        // We're using a slot previously free or a tombstone.
        self.available -= 1;

        self.metadata.?[idx].fill(fingerprint);
        self.size += 1;

        return GetOrPutResult{
            .keyPtr = &self.entries.?[idx].key,
            .valuePtr = &self.entries.?[idx].value,
            .foundExisting = false,
        };
    }

    pub fn putAssumeCapacityNoClobber(self: *ValueMap, vm: *const cy.VM, key: cy.Value, value: cy.Value) linksection(".core") void {
        std.debug.assert(!self.contains(vm, key));

        const hash = computeHash(vm, key);
        const mask = self.cap - 1;
        var idx = @truncate(usize, hash & mask);

        var md = self.metadata.?[idx];
        while (md.isUsed()) {
            idx = (idx + 1) & mask;
            md = self.metadata.?[idx];
        }

        std.debug.assert(self.available > 0);
        self.available -= 1;

        const fingerprint = Metadata.takeFingerprint(hash);
        self.metadata.?[idx].fill(fingerprint);
        self.entries.?[idx] = .{
            .key = key,
            .value = value,
        };

        self.size += 1;
    }

    pub fn contains(self: ValueMap, vm: *const cy.VM, key: cy.Value) linksection(".core") bool {
        return self.getIndex(vm, key) != null;
    }

    fn computeStringHash(str: []const u8) linksection(".core") u64 {
        @setRuntimeSafety(debug);
        return std.hash.Wyhash.hash(0, str);
    }

    fn computeHash(vm: *const cy.VM, key: cy.Value) linksection(".core") u64 {
        @setRuntimeSafety(debug);
        if (key.isNumber()) {
            return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.val));
        } else {
            if (key.isPointer()) {
                const obj = stdx.ptrCastAlign(*cy.HeapObject, key.asPointer().?);
                if (obj.common.structId == cy.StringS) {
                    return std.hash.Wyhash.hash(0, obj.string.ptr[0..obj.string.len]);
                } else stdx.unsupported();
            } else {
                switch (key.getTag()) {
                    cy.TagConstString => {
                        const slice = key.asConstStr();
                        return std.hash.Wyhash.hash(0, vm.strBuf[slice.start..slice.end]);
                    },
                    else => stdx.unsupported(),
                }
            }
        }
    }

    fn stringKeyEqual(vm: *const cy.VM, a: []const u8, b: cy.Value) linksection(".core") bool {
        @setRuntimeSafety(debug);
        if (b.isPointer()) {
            const obj = stdx.ptrCastAlign(*cy.HeapObject, b.asPointer().?);
            if (obj.common.structId == cy.StringS) {
                return std.mem.eql(u8, a, obj.string.ptr[0..obj.string.len]);
            } else return false;
        } else {
            const bTag = b.getTag();
            if (bTag == cy.TagConstString) {
                const bSlice = b.asConstStr();
                const bStr = vm.strBuf[bSlice.start..bSlice.end];
                return std.mem.eql(u8, a, bStr);
            } else {
                stdx.unsupported();
            }
        }
    }

    fn keysEqual(vm: *const cy.VM, a: cy.Value, b: cy.Value) linksection(".core") bool {
        @setRuntimeSafety(debug);
        if (a.isNumber()) {
            return a.val == b.val;
        } else {
            switch (a.getTag()) {
                cy.TagConstString => {
                    const bTag = b.getTag();
                    if (bTag == cy.TagConstString) {
                        const aSlice = a.asConstStr();
                        const bSlice = b.asConstStr();
                        const aStr = vm.strBuf[aSlice.start..aSlice.end];
                        const bStr = vm.strBuf[bSlice.start..bSlice.end];
                        return std.mem.eql(u8, aStr, bStr);
                    } else {
                        stdx.unsupported();
                    }
                },
                else => {
                    stdx.unsupported();
                },
            }
        }
    }

    fn capacityForSize(size: u32) linksection(".core") u32 {
        var newCap = @truncate(u32, (@as(u64, size) * 100) / MaxLoadPercentage + 1);
        newCap = std.math.ceilPowerOfTwo(u32, newCap) catch unreachable;
        return newCap;
    }

    inline fn getIndexByString(self: ValueMap, vm: *const cy.VM, key: []const u8) linksection(".core") ?usize {
        @setRuntimeSafety(debug);
        const hash = computeStringHash(key);
        const mask = self.cap - 1;
        const fingerprint = Metadata.takeFingerprint(hash);

        // Don't loop indefinitely when there are no empty slots.
        var limit = self.cap;
        var idx = @truncate(usize, hash & mask);

        var md = self.metadata.?[idx];
        while (!md.isFree() and limit != 0) {
            if (md.isUsed() and md.fingerprint == fingerprint) {
                const testKey = self.entries.?[idx].key;
                if (stringKeyEqual(vm, key, testKey)) {
                    return idx;
                }
            }

            limit -= 1;
            idx = (idx + 1) & mask;
            md = self.metadata.?[idx];
        }
        return null;
    }

    /// Find the index containing the data for the given key.
    /// Whether this function returns null is almost always
    /// branched on after this function returns, and this function
    /// returns null/not null from separate code paths.  We
    /// want the optimizer to remove that branch and instead directly
    /// fuse the basic blocks after the branch to the basic blocks
    /// from this function.  To encourage that, this function is
    /// marked as inline.
    inline fn getIndex(self: ValueMap, vm: *const cy.VM, key: cy.Value) linksection(".core") ?usize {
        @setRuntimeSafety(debug);
        const hash = computeHash(vm, key);
        const mask = self.cap - 1;
        const fingerprint = Metadata.takeFingerprint(hash);

        // Don't loop indefinitely when there are no empty slots.
        var limit = self.cap;
        var idx = @truncate(usize, hash & mask);

        var md = self.metadata.?[idx];
        while (!md.isFree() and limit != 0) {
            if (md.isUsed() and md.fingerprint == fingerprint) {
                const testKey = self.entries.?[idx].key;
                if (keysEqual(vm, key, testKey)) {
                    return idx;
                }
            }

            limit -= 1;
            idx = (idx + 1) & mask;
            md = self.metadata.?[idx];
        }
        return null;
    }

    fn grow(self: *ValueMap, alloc: std.mem.Allocator, vm: *const cy.VM, newCap: u32) std.mem.Allocator.Error!void {
        const finalCap = std.math.max(newCap, 8);
        std.debug.assert(finalCap > self.cap);
        std.debug.assert(std.math.isPowerOfTwo(newCap));

        const metaSize = finalCap;
        const entriesStart = std.mem.alignForward(metaSize, 8);
        const totalSize = entriesStart + finalCap * @sizeOf(ValueMapEntry);

        const newBuf = try alloc.alignedAlloc(u8, 8, totalSize);
        var newMap = ValueMap{
            .metadata = @ptrCast([*] align(8) Metadata, newBuf.ptr),
            .entries = @intToPtr([*]ValueMapEntry, @ptrToInt(newBuf.ptr) + entriesStart),
            .size = 0,
            .cap = finalCap,
            .available = @truncate(u32, (finalCap * MaxLoadPercentage) / 100),
        };
        @memset(@ptrCast([*] align(8) u8, newMap.metadata), 0, finalCap);

        if (self.size != 0) {
            // Rehash into new map.
            var i: u32 = 0;
            while (i < self.cap) : (i += 1) {
                if (self.metadata.?[i].isUsed()) {
                    const entry = self.entries.?[i];
                    newMap.putAssumeCapacityNoClobber(vm, entry.key, entry.value);
                    if (newMap.size == self.size) {
                        break;
                    }
                }
            }
        }

        self.deinit(alloc);       
        self.* = newMap;
    }

    pub fn deinit(self: *ValueMap, alloc: std.mem.Allocator) void {
        if (self.metadata == null) {
            return;
        }

        const metaSize = self.cap;
        const entriesStart = std.mem.alignForward(metaSize, 8);
        const totalSize = entriesStart + self.cap * @sizeOf(ValueMapEntry);

        alloc.free(self.metadata.?[0..totalSize]);

        self.metadata = null;
        self.entries = null;
        self.size = 0;
        self.cap = 0;
        self.available = 0;
    }

    pub fn remove(self: *ValueMap, vm: *cy.VM, key: cy.Value) linksection(".core") bool {
        if (self.getIndex(vm, key)) |idx| {
            self.removeByIndex(idx);
            return true;
        }
        return false;
    }

    fn removeByIndex(self: *ValueMap, idx: usize) linksection(".core") void {
        self.metadata.?[idx].remove();
        self.size -= 1;
        self.available += 1;
    }
};

pub const ValueMapEntry = struct {
    key: cy.Value,
    value: cy.Value,
};

comptime {
    std.debug.assert(@sizeOf(ValueMapEntry) == 16);
    std.debug.assert(@alignOf(*ValueMapEntry) == 8);
    std.debug.assert(@sizeOf(Metadata) == 1);
    std.debug.assert(@sizeOf(ValueMap) == 32);
}

/// Metadata for a slot. It can be in three states: empty, used or
/// tombstone. Tombstones indicate that an entry was previously used,
/// they are a simple way to handle removal.
/// To this state, we add 7 bits from the slot's key hash. These are
/// used as a fast way to disambiguate between entries without
/// having to use the equality function. If two fingerprints are
/// different, we know that we don't have to compare the keys at all.
/// The 7 bits are the highest ones from a 64 bit hash. This way, not
/// only we use the `log2(capacity)` lowest bits from the hash to determine
/// a slot index, but we use 7 more bits to quickly resolve collisions
/// when multiple elements with different hashes end up wanting to be in the same slot.
/// Not using the equality function means we don't have to read into
/// the entries array, likely avoiding a cache miss and a potentially
/// costly function call.
const Metadata = packed struct {
    fingerprint: FingerPrint = Free,
    used: u1 = 0,

    const FingerPrint = u7;
    const Free: FingerPrint = 0;
    const Tombstone: FingerPrint = 1;
    const SlotFree = @bitCast(u8, Metadata{ .fingerprint = Free });
    const SlotTombstone = @bitCast(u8, Metadata{ .fingerprint = Tombstone });

    pub inline fn isUsed(self: Metadata) bool {
        return self.used == 1;
    }

    pub inline fn takeFingerprint(hash: u64) FingerPrint {
        const hashBits = comptime @typeInfo(u64).Int.bits;
        const fpBits = comptime @typeInfo(FingerPrint).Int.bits;
        return @truncate(FingerPrint, hash >> (hashBits - fpBits));
    }

    pub inline fn isFree(self: Metadata) bool {
        return @bitCast(u8, self) == SlotFree;
    }

    pub inline fn fill(self: *Metadata, fp: FingerPrint) void {
        self.used = 1;
        self.fingerprint = fp;
    }

    pub inline fn remove(self: *Metadata) void {
        self.used = 0;
        self.fingerprint = Tombstone;
    }

    pub inline fn isTombstone(self: Metadata) bool {
        return @bitCast(u8, self) == SlotTombstone;
    }
};

pub const GetOrPutResult = struct {
    keyPtr: *cy.Value,
    valuePtr: *cy.Value,
    foundExisting: bool,
};

// TODO: See how much faster ValueMap is compared to std.HashMapUnmanaged.

// const MapKeyType = enum {
//     constStr,
//     heapStr,
//     number,
// };

// const MapKey = struct {
//     keyT: MapKeyType,
//     inner: packed union {
//         constStr: packed struct {
//             start: u32,
//             end: u32,
//         },
//         heapStr: Value,
//         number: u64,
//     },
// };

// pub const MapContext = struct {
//     vm: *const VM,

//     pub fn hash(self: MapContext, key: MapKey) u64 {
//         @setRuntimeSafety(debug);
//         switch (key.keyT) {
//             .constStr => return std.hash.Wyhash.hash(0, self.vm.strBuf[key.inner.constStr.start..key.inner.constStr.end]),
//             .heapStr => stdx.panic("unsupported heapStr"),
//             .number => {
//                 return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.inner.number));
//             },
//         }
//     }

//     pub fn eql(self: MapContext, a: MapKey, b: MapKey) bool {
//         @setRuntimeSafety(debug);
//         switch (a.keyT) {
//             .constStr => {
//                 if (b.keyT == .constStr) {
//                     const aStr = self.vm.strBuf[a.inner.constStr.start..a.inner.constStr.end];
//                     const bStr = self.vm.strBuf[b.inner.constStr.start..b.inner.constStr.end];
//                     return std.mem.eql(u8, aStr, bStr);
//                 } else if (b.keyT == .heapStr) {
//                     stdx.panic("unsupported heapStr");
//                 } else {
//                     return false;
//                 }
//             },
//             .heapStr => {
//                 stdx.panic("unsupported heapStr");
//             },
//             .number => {
//                 if (b.keyT != .number) {
//                     return false;
//                 } else {
//                     return a.inner.number == b.inner.number;
//                 }
//             },
//         }
//     }
// };

pub const Iterator = struct {
    map: *const ValueMap,
    idx: u32 = 0,

    pub fn next(self: *Iterator) ?ValueMapEntry {
        std.debug.assert(self.idx <= self.map.cap);
        if (self.map.size == 0) {
            return null;
        }

        while (self.idx < self.map.cap) : (self.idx += 1) {
            const md = self.map.metadata.?[self.idx];
            if (md.isUsed()) {
                defer self.idx += 1;
                return self.map.entries.?[self.idx];
            }
        }
        return null;
    }
};