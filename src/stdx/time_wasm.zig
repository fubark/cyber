// Returns milliseconds.
pub extern fn hostMilliTimestamp() f64;

pub const Timer = struct {
    const Self = @This();

    start_ns: u64,

    pub fn start() !Self {
        return Self{
            .start_ns = getNanoTime(),
        };
    }

    pub fn read(self: Self) u64 {
        return getNanoTime() - self.start_ns;
    }
};

pub fn getMilliTimestamp() i64 {
    return @intFromFloat(hostMilliTimestamp());
}

fn getNanoTime() u64 {
    // TODO: Separate extern for accurate timer. On web: performance.now()
    return @intFromFloat(hostMilliTimestamp() * 1e6);
}
