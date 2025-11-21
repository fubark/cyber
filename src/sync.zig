const std = @import("std");

/// Contains synchronized state between a thread that owns and consumes the future's result
/// and the thread that produces the result.
/// When a future has a sync state, `FutureValue.completed` should not be used.
/// 0, pending: Initially marked as pending. At this point, the future's result can not be read.
/// 1, writing: Transition from `pending` by producer to prevent a race when the owner invalidates the future.
/// 2, completed: Transition from `writing` by producer once it's done writing the result.
/// 3, invalidated: Transition from `pending` by owner when the future is deinitialized.
pub const FutureSyncState = u8;

pub const SeqLock = packed struct {
    value: u8 = 0,

    pub fn read_start(self: *SeqLock) ?u8 {
        const state = @atomicLoad(SeqLock, self, .acquire);
        if (state.value & 1 == 1) {
            return null;
        }
        return state.value;
    }

    pub fn read_needs_retry(self: *SeqLock, start: u8) bool {
        return @atomicLoad(SeqLock, self, .acquire).value == start;
    }

    pub fn write_lock(self: *SeqLock) u8 {
        var state: SeqLock = undefined;
        while (true) {
            state = @atomicLoad(SeqLock, self, .monotonic);
            if (state.value & 1 == 0) {
                break;
            }
            std.atomic.spinLoopHint();
        }
        const new = SeqLock{.value=state.value + 1};
        while (@cmpxchgWeak(SeqLock, self, state, new, .acquire, .monotonic)) |_| {}
        return state.value;
    }

    pub fn write_unlock(self: *SeqLock, start: u8) void {
        @atomicStore(SeqLock, self, .{.value=start +% 2}, .release);
    }
};

pub const RWLock = packed struct {
    readers: u7 = 0,
    writing: u1 = 0,

    pub fn read_lock(self: *RWLock) void {
        var state: RWLock = undefined;
        while (true) {
            state = @atomicLoad(RWLock, self, .monotonic);
            if (state.writing == 0) {
                break;
            }
            std.atomic.spinLoopHint();
        }
        const new = RWLock{.readers=state.readers+1, .writing=0};
        while (@cmpxchgWeak(RWLock, self, state, new, .acquire, .monotonic)) |_| {}
    }

    pub fn read_unlock(self: *RWLock) void {
        _ = @atomicRmw(u8, @as(*u8, @ptrCast(self)), .Sub, 1, .release);
    }

    pub fn write_lock(self: *RWLock) void {
        var state: RWLock = undefined;
        while (true) {
            state = @atomicLoad(RWLock, self, .monotonic);
            if (state.readers == 0 and state.writing == 0) {
                break;
            }
            std.atomic.spinLoopHint();
        }
        const new = RWLock{.readers=0, .writing=1};
        while (@cmpxchgWeak(RWLock, self, state, new, .acquire, .monotonic)) |_| {}
    }

    pub fn write_unlock(self: *RWLock) void {
        @atomicStore(RWLock, self, .{.readers=0, .writing=0}, .release);
    }
};