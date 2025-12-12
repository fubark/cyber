const std = @import("std");
const cy = @import("cyber.zig");
const log = cy.log.scoped(.worker);
const build_config = @import("build_config");

const TaskKind = enum(u8) {
    thread,
};

pub const Task = struct {
    kind: TaskKind,
    node: std.SinglyLinkedList.Node,
    data: union {
        thread: struct {
            thread: *cy.Thread,
            func_ptr: cy.Value,
            fut_sync_state: *cy.sync.FutureSyncState,
            fut: *cy.heap.FutureValue,

            // Byte size, not reg size. void return if ret_size == 0.
            ret_size: u16,
        },
    },
};

pub const Pool = struct {
    vm: *cy.VM,
    alloc: std.mem.Allocator,

    mutex: std.Thread.Mutex = .{},
    cond: std.Thread.Condition = .{},

    // Protected by `mutex`.
    workers: []Worker,
    active_workers: usize,
    num_workers: usize,
    task_queue: std.SinglyLinkedList,
    last_task_node: ?*std.SinglyLinkedList.Node,

    pub fn init(vm: *cy.VM, max_workers: usize) !Pool {
        return .{
            .vm = vm,
            .alloc = vm.alloc,
            .workers = try vm.alloc.alloc(Worker, max_workers),
            .active_workers = 0,
            .last_task_node = null,
            .task_queue = .{},
            .num_workers = 0,
        };
    }

    pub fn deinit(self: *Pool, alloc: std.mem.Allocator) void {
        alloc.free(self.workers);
    }

    // Assumes in critical section.
    pub fn request_idle_worker(self: *Pool) !void {
        if (self.active_workers < self.num_workers) return;
        if (self.num_workers == self.workers.len) return;

        self.workers[self.num_workers] = .{
            .id = self.num_workers,
            .os_thread = try std.Thread.spawn(.{
                .stack_size = 16 * 1024 * 1024,
                .allocator = self.alloc,
            }, Worker.run, .{&self.workers[self.num_workers], self}),
        };
        self.num_workers += 1;
    }

    pub fn add_thread_task(self: *Pool, thread: *cy.Thread, func_ptr: cy.Value, fut: *cy.heap.FutureValue, ret_size: usize) !void {
        if (!build_config.threads) {
            @panic("unsupported");
        }
        {
            self.mutex.lock();
            defer self.mutex.unlock();

            try self.request_idle_worker();       

            const task = try self.alloc.create(Task);
            task.* = .{
                .kind = .thread,
                .node = .{},
                .data = .{ .thread = .{
                    .thread = thread,
                    .func_ptr = func_ptr,
                    .fut = fut,
                    .fut_sync_state = fut.shared_state.?,
                    .ret_size = @intCast(ret_size),
                }},
            };
            if (self.last_task_node) |last| {
                last.insertAfter(&task.node);
            } else {
                self.task_queue.first = &task.node;
            }
            self.last_task_node = &task.node;
        }

        self.cond.signal();
        log.tracev("signal new thread task", .{});
    }
};

pub const Worker = struct {
    id: usize,
    os_thread: std.Thread,

    pub fn run(self: *Worker, pool: *Pool) void {
        pool.mutex.lock();
        defer pool.mutex.unlock();
        while (true) {
            const node = pool.task_queue.popFirst() orelse {
                log.tracev("w{}: cond wait", .{self.id});
                pool.cond.wait(&pool.mutex);
                log.tracev("w{}: woke up", .{self.id});
                continue;
            };
            if (pool.task_queue.first == null) {
                // No more tasks in queue.
                pool.last_task_node = null;
            }
            pool.active_workers += 1;
            pool.mutex.unlock();
            defer {
                pool.mutex.lock();
                pool.active_workers -= 1;
            }

            // Run VM task.
            const task: *Task = @fieldParentPtr("node", node);
            switch (task.kind) {
                .thread => {
                    const thread = task.data.thread.thread;
                    const ret_byte_size = task.data.thread.ret_size;
                    const ret_reg_size = (task.data.thread.ret_size + 7) >> 3;

                    // Ensure `fp_end` is set.
                    thread.c.fp_end = thread.c.stack_ptr + 1;
                    const ret = thread.callFunc2(thread.c.stack_ptr, ret_reg_size, task.data.thread.func_ptr) catch |err| {
                        std.debug.panic("error: {}", .{err});
                    };

                    const state = task.data.thread.fut_sync_state;
                    while (@cmpxchgWeak(u8, state, 0, 1, .acquire, .monotonic)) |actual| {
                        if (actual == 3) {
                            @panic("TODO: future was invalidated");
                        }
                    }

                    @as(*u64, @ptrCast(&task.data.thread.fut.data)).* = 1;
                    if (ret_reg_size > 0) {
                        const dst = @as([*]u8, @ptrCast(&task.data.thread.fut.data)) + 8;
                        @memcpy(dst[0..ret_byte_size], @as([*]u8, @ptrCast(ret))[0..ret_byte_size]);
                    }

                    @atomicStore(u8, state, 2, .release);

                    if (cy.Trace) {
                        const grc = thread.heap.c.refCounts;
                        if (grc != 0) {
                            cy.C.thread_dump_live_objects(@ptrCast(thread));
                            cy.panicFmt("unreleased refcount: {}", .{grc});
                        }
                    }

                    thread.deinit(false);
                    pool.alloc.destroy(thread);
                },
            }

            pool.alloc.destroy(task);
        }
    }
};
