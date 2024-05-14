// Copyright (c) 2023 Cyber (See LICENSE)

pub fn load(comptime Size: usize, comptime T: type, buf: []const T) @Vector(Size, T) {
    var v: @Vector(Size, T) = @splat(@as(T, 0));
    for (buf, 0..) |it, i| {
        v[i] = it;
    }
    return v;
}

pub fn negIotaIntExt(comptime len: usize, comptime offset: u32, comptime factor: u32) @Vector(len, i32) {
    const out: [len]i32 = undefined;
    for (out, 0..) |*element, i| {
        element.* = ~@as(i32, @intCast(offset + i * factor));
    }
    return @as(@Vector(len, i32), out);
}

pub fn iotaExt(comptime T: type, comptime len: usize, comptime offset: T, comptime factor: T) @Vector(len, T) {
    const out: [len]T = undefined;
    for (out, 0..) |*element, i| {
        element.* = switch (@typeInfo(T)) {
            .Int => offset + @as(T, @intCast(i)) * factor,
            .Float => offset + @as(T, @floatFromInt(i)) * factor,
            else => @compileError("Can't use type " ++ @typeName(T) ++ " in iota."),
        };
    }
    return @as(@Vector(len, T), out);
}
