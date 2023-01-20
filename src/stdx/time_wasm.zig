// Returns milliseconds.
pub extern "stdx" fn jsPerformanceNow() f64;

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

pub fn getMillisTime() f64 {
    return jsPerformanceNow();
}

fn getNanoTime() u64 {
    return @floatToInt(u64, jsPerformanceNow() * 1e6);
}
