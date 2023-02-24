// Copyright (c) 2023 Cyber (See LICENSE)

pub fn load(comptime Size: usize, comptime T: type, buf: []const T) @Vector(Size, T) {
    var v = @splat(Size, @as(T, 0));
    for (buf, 0..) |it, i| {
        v[i] = it;
    }
    return v;
}

pub fn negIotaIntExt(comptime len: usize, comptime offset: u32, comptime factor: u32) @Vector(len, i32) {
    var out: [len]i32 = undefined;
    for (out, 0..) |*element, i| {
        element.* = ~@intCast(i32, offset + i * factor);
    }
    return @as(@Vector(len, i32), out);
}

pub fn iotaExt(comptime T: type, comptime len: usize, comptime offset: T, comptime factor: T) @Vector(len, T) {
    var out: [len]T = undefined;
    for (out, 0..) |*element, i| {
        element.* = switch (@typeInfo(T)) {
            .Int => offset + @intCast(T, i) * factor,
            .Float => offset + @intToFloat(T, i) * factor,
            else => @compileError("Can't use type " ++ @typeName(T) ++ " in iota."),
        };
    }
    return @as(@Vector(len, T), out);
}
