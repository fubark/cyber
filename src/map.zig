const std = @import("std");
const stdx = @import("stdx");
const t = stdx.testing;
const builtin = @import("builtin");
const build_options = @import("build_options");
const debug = builtin.mode == .Debug;

const cy = @import("cyber.zig");
const bt = cy.types.BuiltinTypes;
const log = cy.log.scoped(.map);

/// The implementation of ValueMap is based on Zig's std.HashMapUnmanaged.
/// Since keys and values are just cy.Values (8 bytes each), the memory layout can be made more compact.
/// This also allows inserting optimizations where needed.
pub const ValueMap = struct {
    metadata: ?[*] align(8) Metadata = null,
    entries: ?[*]ValueMapEntry = null,
    size: u32 = 0,
    cap: u32 = 0,
    available: u32 = 0,

    const MaxLoadPercentage = 80;

    pub fn iterator(self: *const ValueMap) Iterator {
        return Iterator{ .map = self };
    }

    pub fn put(self: *ValueMap, alloc: std.mem.Allocator, key: cy.Value, value: cy.Value) std.mem.Allocator.Error!void {
        const res = try self.getOrPut(alloc, key);
        res.valuePtr.* = value;
    }

    pub fn get(self: ValueMap, key: cy.Value) ?cy.Value {
        if (self.getIndex(key)) |idx| {
            return self.entries.?[idx].value;
        }
        return null;
    }

    pub fn getByString(self: ValueMap, key: []const u8) ?cy.Value {
        @setRuntimeSafety(debug);
        if (self.getIndexByString(key)) |idx| {
            return self.entries.?[idx].value;
        }
        return null;
    }

    // This counts the number of occupied slots (not counting tombstones), which is
    // what has to stay under the max_load_percentage of capacity.
    fn load(self: *const ValueMap) u32 {
        const maxLoad = (self.cap * MaxLoadPercentage) / 100;
        std.debug.assert(maxLoad >= self.available);
        return @truncate(maxLoad - self.available);
    }

    pub fn getOrPut(self: *ValueMap, alloc: std.mem.Allocator, key: cy.Value) std.mem.Allocator.Error!GetOrPutResult {
        const res = try self.getOrPutAdapted(alloc, key);
        if (!res.foundExisting) {
            res.keyPtr.* = key;
        }
        return res;
    }

    pub fn getOrPutAdapted(self: *ValueMap, alloc: std.mem.Allocator, key: cy.Value) std.mem.Allocator.Error!GetOrPutResult {
        if (self.available == 0) {
            try self.grow(alloc, capacityForSize(self.load() + 1));
        }
        return self.getOrPutAssumeCapacityAdapted(key);
    }

    pub fn getOrPutAssumeCapacityAdapted(self: *ValueMap, key: cy.Value) GetOrPutResult {
        const hash = computeHash(key);
        const mask = self.cap - 1;
        const fingerprint = Metadata.takeFingerprint(hash);
        var limit = self.cap;
        var idx: usize = @truncate(hash & mask);

        var first_tombstone_idx: usize = self.cap; // invalid index
        var md = self.metadata.?[idx];
        while (!md.isFree() and limit != 0) {
            if (md.isUsed() and md.fingerprint == fingerprint) {
                const testKey = self.entries.?[idx].key;
                if (keysEqual(key, testKey)) {
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

    pub fn putAssumeCapacityNoClobber(self: *ValueMap, key: cy.Value, value: cy.Value) void {
        std.debug.assert(!self.contains(key));

        const hash = computeHash(key);
        const mask = self.cap - 1;
        var idx: usize = @truncate(hash & mask);

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

    pub fn contains(self: ValueMap, key: cy.Value) bool {
        return self.getIndex(key) != null;
    }

    fn computeStringHash(str: []const u8) u64 {
        @setRuntimeSafety(debug);
        return std.hash.Wyhash.hash(0, str);
    }

    fn computeHash(key: cy.Value) u64 {
        if (key.isPointer()) {
            const obj = key.asHeapObject();
            switch (obj.getTypeId()) {
                bt.String => {
                    return std.hash.Wyhash.hash(0, obj.string.getSlice());
                },
                bt.Integer => {
                    return std.hash.Wyhash.hash(0, std.mem.asBytes(&obj.integer.val));
                },
                else => {},
            }
        }
        return std.hash.Wyhash.hash(0, std.mem.asBytes(&key.val));
    }

    fn stringKeyEqual(a: []const u8, b: cy.Value) bool {
        if (!b.isPointer()) {
            return false;
        }
        const bobj = b.asHeapObject();
        if (bobj.getTypeId() != bt.String) {
            return false;
        }
        return std.mem.eql(u8, a, bobj.string.getSlice());
    }

    fn keysEqual(a: cy.Value, b: cy.Value) bool {
        if (a.val == b.val) {
            return true;
        }
        if (!a.isPointer()) {
            return false;
        }
        const a_t = a.getTypeId();
        if (a_t != b.getTypeId()) {
            return false;
        }
        switch (a_t) {
            bt.String => {
                return std.mem.eql(u8, a.asString(), b.asString());
            },
            bt.Integer => {
                return a.asBoxInt() == b.asBoxInt();
            },
            else => {
                return false;
            }
        }
    }

    fn capacityForSize(size: u32) u32 {
        var newCap: u32 = @truncate((@as(u64, size) * 100) / MaxLoadPercentage + 1);
        newCap = std.math.ceilPowerOfTwo(u32, newCap) catch unreachable;
        return newCap;
    }

    inline fn getIndexByString(self: ValueMap, key: []const u8) ?usize {
        @setRuntimeSafety(debug);

        if (self.size == 0) {
            return null;
        }

        const hash = computeStringHash(key);
        const mask = self.cap - 1;
        const fingerprint = Metadata.takeFingerprint(hash);

        // Don't loop indefinitely when there are no empty slots.
        var limit = self.cap;
        var idx: usize = @truncate(hash & mask);

        var md = self.metadata.?[idx];
        while (!md.isFree() and limit != 0) {
            if (md.isUsed() and md.fingerprint == fingerprint) {
                const testKey = self.entries.?[idx].key;
                if (stringKeyEqual(key, testKey)) {
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
    inline fn getIndex(self: ValueMap, key: cy.Value) ?usize {
        if (self.size == 0) {
            return null;
        }

        const hash = computeHash(key);
        const mask = self.cap - 1;
        const fingerprint = Metadata.takeFingerprint(hash);

        // Don't loop indefinitely when there are no empty slots.
        var limit = self.cap;
        var idx: usize = @truncate(hash & mask);

        var md = self.metadata.?[idx];
        while (!md.isFree() and limit != 0) {
            if (md.isUsed() and md.fingerprint == fingerprint) {
                const testKey = self.entries.?[idx].key;
                if (keysEqual(key, testKey)) {
                    return idx;
                }
            }

            limit -= 1;
            idx = (idx + 1) & mask;
            md = self.metadata.?[idx];
        }
        return null;
    }

    fn grow(self: *ValueMap, alloc: std.mem.Allocator, newCap: u32) std.mem.Allocator.Error!void {
        const finalCap = @max(newCap, 8);
        std.debug.assert(finalCap > self.cap);
        std.debug.assert(std.math.isPowerOfTwo(newCap));

        const metaSize = finalCap;
        const entriesStart = std.mem.alignForward(usize, metaSize, 8);
        const totalSize = entriesStart + finalCap * @sizeOf(ValueMapEntry);

        const newBuf = try alloc.alignedAlloc(u8, 8, totalSize);
        var newMap = ValueMap{
            .metadata = @ptrCast(newBuf.ptr),
            .entries = @ptrFromInt(@intFromPtr(newBuf.ptr) + entriesStart),
            .size = 0,
            .cap = finalCap,
            .available = @truncate((finalCap * MaxLoadPercentage) / 100),
        };
        @memset(@as([*] align (8) u8, @ptrCast(newMap.metadata))[0..finalCap], 0);

        if (self.size != 0) {
            // Rehash into new map.
            var i: u32 = 0;
            while (i < self.cap) : (i += 1) {
                if (self.metadata.?[i].isUsed()) {
                    const entry = self.entries.?[i];
                    newMap.putAssumeCapacityNoClobber(entry.key, entry.value);
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
        const entriesStart = std.mem.alignForward(usize, metaSize, 8);
        const totalSize = entriesStart + self.cap * @sizeOf(ValueMapEntry);

        alloc.free(self.metadata.?[0..totalSize]);

        self.metadata = null;
        self.entries = null;
        self.size = 0;
        self.cap = 0;
        self.available = 0;
    }

    pub fn remove(self: *ValueMap, vm: *cy.VM, key: cy.Value) bool {
        if (self.getIndex(key)) |idx| {
            self.removeByIndex(idx);
            // Release key since it can be an object.
            const e = self.entries.?[idx];
            cy.arc.release(vm, e.key);
            cy.arc.release(vm, e.value);
            return true;
        }
        return false;
    }

    fn removeByIndex(self: *ValueMap, idx: usize) void {
        self.metadata.?[idx].remove();
        self.size -= 1;
        self.available += 1;
    }

    pub fn next(self: *ValueMap, idx: *u32) ?ValueMapEntry {
        std.debug.assert(idx.* <= self.cap);
        if (self.size == 0) {
            return null;
        }
        while (idx.* < self.cap) : (idx.* += 1) {
            const md = self.metadata.?[idx.*];
            if (md.isUsed()) {
                defer idx.* += 1;
                return self.entries.?[idx.*];
            }
        }
        return null;
    }
};

pub const ValueMapEntry = struct {
    key: cy.Value,
    value: cy.Value,
};

test "map internals." {
    if (cy.is32Bit) {
        try t.eq(@alignOf(*ValueMapEntry), 4);
        try t.eq(@sizeOf(ValueMap), 20);
    } else {
        try t.eq(@alignOf(*ValueMapEntry), 8);
        try t.eq(@sizeOf(ValueMap), 32);
    }
    try t.eq(@sizeOf(ValueMapEntry), 16);
    try t.eq(@sizeOf(Metadata), 1);
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
    const SlotFree: u8 = @bitCast(Metadata{ .fingerprint = Free });
    const SlotTombstone: u8 = @bitCast(Metadata{ .fingerprint = Tombstone });

    pub inline fn isUsed(self: Metadata) bool {
        return self.used == 1;
    }

    pub inline fn takeFingerprint(hash: u64) FingerPrint {
        const hashBits = comptime @typeInfo(u64).int.bits;
        const fpBits = comptime @typeInfo(FingerPrint).int.bits;
        return @truncate(hash >> (hashBits - fpBits));
    }

    pub inline fn isFree(self: Metadata) bool {
        return @as(u8, @bitCast(self)) == SlotFree;
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
        return @as(u8, @bitCast(self)) == SlotTombstone;
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
//             .heapStr => cy.panic("unsupported heapStr"),
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
//                     cy.panic("unsupported heapStr");
//                 } else {
//                     return false;
//                 }
//             },
//             .heapStr => {
//                 cy.panic("unsupported heapStr");
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
