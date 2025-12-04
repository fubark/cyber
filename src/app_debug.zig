const std = @import("std");
const builtin = @import("builtin");
const C = @import("capi.zig");
const vmc = @import("vmc");

/// NOTE: Dupe Zig segfault handler to wrap a custom user handler.
var windows_segfault_handle: ?std.os.windows.HANDLE = null;

/// Attaches a global SIGSEGV handler which calls `@panic("segmentation fault");`
const SigactionFn = if (builtin.os.tag == .windows) *const fn (i32, *const void, ?*anyopaque) callconv(.c) noreturn else std.posix.Sigaction.sigaction_fn;
pub fn attachSegfaultHandler(handler: SigactionFn) void {
    if (!std.debug.have_segfault_handling_support) {
        @compileError("segfault handler not supported for this target");
    }
    if (builtin.os.tag == .windows) {
        windows_segfault_handle = std.os.windows.kernel32.AddVectoredExceptionHandler(0, handleSegfaultWindows);
        return;
    }
    const act = std.posix.Sigaction{
        .handler = .{ .sigaction = handler },
        .mask = std.posix.sigemptyset(),
        .flags = (std.posix.SA.SIGINFO | std.posix.SA.RESTART | std.posix.SA.RESETHAND),
    };
    std.debug.updateSegfaultHandler(&act);
}

fn resetSegfaultHandler() void {
    if (builtin.os.tag == .windows) {
        if (windows_segfault_handle) |handle| {
            std.debug.assert(std.os.windows.kernel32.RemoveVectoredExceptionHandler(handle) != 0);
            windows_segfault_handle = null;
        }
        return;
    }
    const act = std.posix.Sigaction{
        .handler = .{ .handler = std.posix.SIG.DFL },
        .mask = std.posix.sigemptyset(),
        .flags = 0,
    };
    std.debug.updateSegfaultHandler(&act);
}

/// Non-zero whenever the program triggered a panic.
/// The counter is incremented/decremented atomically.
var panicking = std.atomic.Value(u8).init(0);

/// Counts how many times the panic handler is invoked by this thread.
/// This is used to catch and handle panics triggered by the panic handler.
pub threadlocal var panic_stage: usize = 0;

/// Modified from `std.debug.defaultPanic` to wrap a custom `handler`.
/// Dumps a stack trace to standard error, then aborts.
pub fn defaultPanic(
    msg: []const u8,
    first_trace_addr: ?usize,
    handler: *const fn () anyerror!void,
) noreturn {
    @branchHint(.cold);

    // For backends that cannot handle the language features depended on by the
    // default panic handler, we have a simpler panic handler:
    switch (builtin.zig_backend) {
        .stage2_aarch64,
        .stage2_arm,
        .stage2_powerpc,
        .stage2_riscv64,
        .stage2_spirv,
        .stage2_wasm,
        .stage2_x86,
        => @trap(),
        .stage2_x86_64 => switch (builtin.target.ofmt) {
            .elf, .macho => {},
            else => @trap(),
        },
        else => {},
    }

    switch (builtin.os.tag) {
        .freestanding, .other => {
            @trap();
        },
        .uefi => {
            const uefi = std.os.uefi;

            var utf16_buffer: [1000]u16 = undefined;
            const len_minus_3 = std.unicode.utf8ToUtf16Le(&utf16_buffer, msg) catch 0;
            utf16_buffer[len_minus_3..][0..3].* = .{ '\r', '\n', 0 };
            const len = len_minus_3 + 3;
            const exit_msg = utf16_buffer[0 .. len - 1 :0];

            // Output to both std_err and con_out, as std_err is easier
            // to read in stuff like QEMU at times, but, unlike con_out,
            // isn't visible on actual hardware if directly booted into
            inline for ([_]?*uefi.protocol.SimpleTextOutput{ uefi.system_table.std_err, uefi.system_table.con_out }) |o| {
                if (o) |out| {
                    out.setAttribute(.{ .foreground = .red }) catch {};
                    _ = out.outputString(exit_msg) catch {};
                    out.setAttribute(.{ .foreground = .white }) catch {};
                }
            }

            if (uefi.system_table.boot_services) |bs| {
                // ExitData buffer must be allocated using boot_services.allocatePool (spec: page 220)
                const exit_data = uefi.raw_pool_allocator.dupeZ(u16, exit_msg) catch @trap();
                bs.exit(uefi.handle, .aborted, exit_data) catch {};
            }
            @trap();
        },
        .cuda, .amdhsa => std.posix.abort(),
        .plan9 => {
            var status: [std.os.plan9.ERRMAX]u8 = undefined;
            const len = @min(msg.len, status.len - 1);
            @memcpy(status[0..len], msg[0..len]);
            status[len] = 0;
            std.os.plan9.exits(status[0..len :0]);
        },
        else => {},
    }

    if (std.options.enable_segfault_handler) {
        // If a segfault happens while panicking, we want it to actually segfault, not trigger
        // the handler.
        resetSegfaultHandler();
    }

    // Note there is similar logic in handleSegfaultPosix and handleSegfaultWindowsExtra.
    nosuspend switch (panic_stage) {
        0 => {
            panic_stage = 1;

            _ = panicking.fetchAdd(1, .seq_cst);

            {
                const stderr = std.debug.lockStderrWriter(&.{});
                defer std.debug.unlockStderrWriter();

                if (builtin.single_threaded) {
                    stderr.print("panic: ", .{}) catch std.posix.abort();
                } else {
                    const current_thread_id = std.Thread.getCurrentId();
                    stderr.print("thread {} panic: ", .{current_thread_id}) catch std.posix.abort();
                }
                stderr.print("{s}\n", .{msg}) catch std.posix.abort();

                if (@errorReturnTrace()) |t| std.debug.dumpStackTrace(t.*);
                std.debug.dumpCurrentStackTraceToWriter(first_trace_addr orelse @returnAddress(), stderr) catch {};
            }

            handler() catch |err| {
                std.debug.print("error during panic: {}", .{err});
                std.posix.abort();
            };

            waitForOtherThreadToFinishPanicking();
        },
        1 => {
            panic_stage = 2;

            // A panic happened while trying to print a previous panic message.
            // We're still holding the mutex but that's fine as we're going to
            // call abort().
            std.fs.File.stderr().writeAll("aborting due to recursive panic\n") catch {};
        },
        else => {}, // Panicked while printing the recursive panic message.
    };

    std.posix.abort();
}

pub fn vm_segv_handler(_: *C.VM) !void {
    const t: *C.Thread = @ptrCast(vmc.cur_thread orelse {
        std.debug.print("segfault without thread context\n", .{});
        return;
    });
    C.thread_signal_host_segfault(t);
}

pub fn vm_panic_handler(_: *C.VM) !void {
    const t: *C.Thread = @ptrCast(vmc.cur_thread orelse {
        std.debug.print("panic without thread context\n", .{});
        return;
    });
    C.thread_signal_host_panic(t);
}

pub fn handleSegfaultPosix(sig: i32, info: *const std.posix.siginfo_t, ctx_ptr: ?*anyopaque) callconv(.c) noreturn {
    // Reset to the default handler so that if a segfault happens in this handler it will crash
    // the process. Also when this handler returns, the original instruction will be repeated
    // and the resulting segfault will crash the process rather than continually dump stack traces.
    resetSegfaultHandler();

    const addr = switch (builtin.os.tag) {
        .linux => @intFromPtr(info.fields.sigfault.addr),
        .freebsd, .macos => @intFromPtr(info.addr),
        .netbsd => @intFromPtr(info.info.reason.fault.addr),
        .openbsd => @intFromPtr(info.data.fault.addr),
        .solaris, .illumos => @intFromPtr(info.reason.fault.addr),
        else => unreachable,
    };

    const code = if (builtin.os.tag == .netbsd) info.info.code else info.code;
    nosuspend switch (panic_stage) {
        0 => {
            panic_stage = 1;
            _ = panicking.fetchAdd(1, .seq_cst);

            {
                std.debug.lockStdErr();
                defer std.debug.unlockStdErr();

                dumpSegfaultInfoPosix(sig, code, addr, ctx_ptr);
            }

            waitForOtherThreadToFinishPanicking();
        },
        else => {
            // panic mutex already locked
            dumpSegfaultInfoPosix(sig, code, addr, ctx_ptr);
        },
    };

    // We cannot allow the signal handler to return because when it runs the original instruction
    // again, the memory may be mapped and undefined behavior would occur rather than repeating
    // the segfault. So we simply abort here.
    std.posix.abort();
}

fn handleSegfaultWindows(info: *std.os.windows.EXCEPTION_POINTERS) callconv(.winapi) c_long {
    switch (info.ExceptionRecord.ExceptionCode) {
        std.os.windows.EXCEPTION_DATATYPE_MISALIGNMENT => handleSegfaultWindowsExtra(info, 0, "Unaligned Memory Access"),
        std.os.windows.EXCEPTION_ACCESS_VIOLATION => handleSegfaultWindowsExtra(info, 1, null),
        std.os.windows.EXCEPTION_ILLEGAL_INSTRUCTION => handleSegfaultWindowsExtra(info, 2, null),
        std.os.windows.EXCEPTION_STACK_OVERFLOW => handleSegfaultWindowsExtra(info, 0, "Stack Overflow"),
        else => return std.os.windows.EXCEPTION_CONTINUE_SEARCH,
    }
}

fn handleSegfaultWindowsExtra(info: *std.os.windows.EXCEPTION_POINTERS, msg: u8, label: ?[]const u8) noreturn {
    // For backends that cannot handle the language features used by this segfault handler, we have a simpler one,
    switch (builtin.zig_backend) {
        .stage2_x86_64 => if (builtin.target.ofmt == .coff) @trap(),
        else => {},
    }

    comptime std.debug.assert(std.os.windows.CONTEXT != void);
    nosuspend switch (panic_stage) {
        0 => {
            panic_stage = 1;
            _ = panicking.fetchAdd(1, .seq_cst);

            {
                const stderr = std.debug.lockStderrWriter(&.{});
                defer std.debug.unlockStderrWriter();

                dumpSegfaultInfoWindows(info, msg, label, stderr);
            }

            waitForOtherThreadToFinishPanicking();
        },
        1 => {
            panic_stage = 2;
            std.fs.File.stderr().writeAll("aborting due to recursive panic\n") catch {};
        },
        else => {},
    };
    std.posix.abort();
}

const native_arch = builtin.cpu.arch;
const native_os = builtin.os.tag;
fn dumpSegfaultInfoPosix(sig: i32, code: i32, addr: usize, ctx_ptr: ?*anyopaque) void {
    const stderr = std.debug.lockStderrWriter(&.{});
    defer std.debug.unlockStderrWriter();
    _ = switch (sig) {
        std.posix.SIG.SEGV => if (native_arch == .x86_64 and native_os == .linux and code == 128) // SI_KERNEL
            // x86_64 doesn't have a full 64-bit virtual address space.
            // Addresses outside of that address space are non-canonical
            // and the CPU won't provide the faulting address to us.
            // This happens when accessing memory addresses such as 0xaaaaaaaaaaaaaaaa
            // but can also happen when no addressable memory is involved;
            // for example when reading/writing model-specific registers
            // by executing `rdmsr` or `wrmsr` in user-space (unprivileged mode).
            stderr.writeAll("General protection exception (no address available)\n")
        else
            stderr.print("Segmentation fault at address 0x{x}\n", .{addr}),
        std.posix.SIG.ILL => stderr.print("Illegal instruction at address 0x{x}\n", .{addr}),
        std.posix.SIG.BUS => stderr.print("Bus error at address 0x{x}\n", .{addr}),
        std.posix.SIG.FPE => stderr.print("Arithmetic exception at address 0x{x}\n", .{addr}),
        else => unreachable,
    } catch std.posix.abort();

    switch (native_arch) {
        .x86,
        .x86_64,
        .arm,
        .armeb,
        .thumb,
        .thumbeb,
        .aarch64,
        .aarch64_be,
        => {
            // Some kernels don't align `ctx_ptr` properly. Handle this defensively.
            const ctx: *align(1) std.posix.ucontext_t = @ptrCast(ctx_ptr);
            var new_ctx: std.posix.ucontext_t = ctx.*;
            if (builtin.os.tag.isDarwin() and builtin.cpu.arch == .aarch64) {
                // The kernel incorrectly writes the contents of `__mcontext_data` right after `mcontext`,
                // rather than after the 8 bytes of padding that are supposed to sit between the two. Copy the
                // contents to the right place so that the `mcontext` pointer will be correct after the
                // `relocateContext` call below.
                new_ctx.__mcontext_data = @as(*align(1) extern struct {
                    onstack: c_int,
                    sigmask: std.c.sigset_t,
                    stack: std.c.stack_t,
                    link: ?*std.c.ucontext_t,
                    mcsize: u64,
                    mcontext: *std.c.mcontext_t,
                    __mcontext_data: std.c.mcontext_t align(@sizeOf(usize)), // Disable padding after `mcontext`.
                }, @ptrCast(ctx)).__mcontext_data;
            }
            std.debug.relocateContext(&new_ctx);
            std.debug.dumpStackTraceFromBase(&new_ctx, stderr);
        },
        else => {},
    }
}

fn dumpSegfaultInfoWindows(info: *std.os.windows.EXCEPTION_POINTERS, msg: u8, label: ?[]const u8, stderr: *std.io.Writer) void {
    _ = switch (msg) {
        0 => stderr.print("{s}\n", .{label.?}),
        1 => stderr.print("Segmentation fault at address 0x{x}\n", .{info.ExceptionRecord.ExceptionInformation[1]}),
        2 => stderr.print("Illegal instruction at address 0x{x}\n", .{info.ContextRecord.getRegs().ip}),
        else => unreachable,
    } catch std.posix.abort();

    std.debug.dumpStackTraceFromBase(info.ContextRecord, stderr);
}

/// Must be called only after adding 1 to `panicking`. There are three callsites.
fn waitForOtherThreadToFinishPanicking() void {
    if (panicking.fetchSub(1, .seq_cst) != 1) {
        // Another thread is panicking, wait for the last one to finish
        // and call abort()
        if (builtin.single_threaded) unreachable;

        // Sleep forever without hammering the CPU
        var futex = std.atomic.Value(u32).init(0);
        while (true) std.Thread.Futex.wait(&futex, 0);
        unreachable;
    }
}

// Protected by `std.debug.lockStderrWriter`.
pub var print_buf: [1024]u8 = undefined;
