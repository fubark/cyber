const std = @import("std");
const stdx = @import("stdx");
const fatal = stdx.fatal;
const builtin = @import("builtin");
const IsWasm = builtin.target.isWasm();
const qjs = @import("qjs");

const log = stdx.log.scoped(.interpreter);

extern "app" fn jsEval(id: usize, ptr: [*]const u8, len: usize) usize;
extern "app" fn jsGetValueType(id: usize) usize;
extern "app" fn jsGetValueString(val_id: usize, ptr: [*]const u8, len: usize) usize;
extern "app" fn jsGetInt32(id: usize) i32;
extern "app" fn jsGetFloat64(id: usize) f64;
extern "app" fn jsDeleteValue(id: usize) void;
extern "app" fn jsGetProperty(id: usize, prop_ptr: [*]const u8, prop_len: usize) usize;
extern "app" fn jsGetGlobalValue(ptr: [*]const u8, len: usize) usize;
extern "app" fn jsCall(func: usize, this: usize, args_ptr: [*]const u32, args_len: usize) usize;
extern "app" fn jsCallGen(func: usize, this: usize, args_ptr: [*]const u32, args_len: usize) usize;

pub const JsEngine = struct {
    alloc: std.mem.Allocator,
    inner: if (IsWasm) struct {
        buf: std.ArrayListUnmanaged(u8),
        u32_buf: std.ArrayListUnmanaged(u32),
    } else struct {
        rt: *qjs.JSRuntime,
        ctx: *qjs.JSContext,
    },

    pub const Undefined: JsValue = if (IsWasm) 
        JsValue.initWebJS(1)
    else JsValue.initQJS(qjs.Undefined);

    pub const Null: JsValue = if (IsWasm) 
        JsValue.initWebJS(2)
    else JsValue.initQJS(qjs.Null);

    pub fn init(alloc: std.mem.Allocator) JsEngine {
        if (IsWasm) {
            return .{
                .alloc = alloc,
                .inner = .{
                    .buf = .{},
                    .u32_buf = .{},
                },
            };
        } else {
            const rt = qjs.JS_NewRuntime().?;
            return .{
                .alloc = alloc,
                .inner = .{
                    .rt = rt,
                    .ctx = qjs.JS_NewContext(rt).?,
                },
            };
        }
    }

    pub fn deinit(self: JsEngine) void {
        if (IsWasm) {
            self.buf.deinit(self.alloc);
            self.u32_buf.deinit(self.alloc);
        } else {
            qjs.JS_FreeContext(self.inner.ctx);
            qjs.JS_FreeRuntime(self.inner.rt);
        }
    }

    pub fn initInt32(self: JsEngine, val: i32) JsValue {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            return .{
                .inner = qjs.JS_NewInt32(self.inner.ctx, val),
            };
        }
    }

    pub fn deinitValue(self: JsEngine, val: JsValue) void {
        if (IsWasm) {
            if (val.inner.id > 4) {
                jsDeleteValue(val.inner.id);
            }
        } else {
            qjs.JS_FreeValue(self.inner.ctx, val.inner);
        }
    }

    pub fn eval(self: JsEngine, src: [:0]const u8) JsValue {
        if (IsWasm) {
            const val_id = jsEval(0, src.ptr, src.len);
            return JsValue.initWebJS(val_id);
        } else {
            // Even though JS_Eval takes src length, it still needs to be null terminated.
            const val = qjs.JS_Eval(self.inner.ctx, src.ptr, src.len, "eval", qjs.JS_EVAL_TYPE_GLOBAL);
            return .{
                .inner = val,
            };
        }
    }

    pub fn getValueTag(self: JsEngine, val: JsValue) JsValueType {
        if (IsWasm) {
            return @intToEnum(JsValueType, jsGetValueType(val.inner.id));
        } else {
            return QJS.getTag(self.inner.ctx, val.inner);
        }
    }

    pub fn valueToString(self: *JsEngine, val: JsValue) ![]const u8 {
        if (IsWasm) {
            self.inner.buf.clearRetainingCapacity();
            var len = jsGetValueString(val.inner.id, self.inner.buf.items.ptr, self.inner.buf.capacity);
            if (len > self.inner.buf.capacity) {
                try self.inner.buf.ensureTotalCapacity(self.alloc, len);
                len = jsGetValueString(val.inner.id, self.inner.buf.items.ptr, self.inner.buf.capacity);
            }
            self.inner.buf.items.len = len;
            return try self.alloc.dupe(u8, self.inner.buf.items[0..len]);
        } else {
            const str = qjs.JS_ToCString(self.inner.ctx, val.inner);
            defer qjs.JS_FreeCString(self.inner.ctx, str);
            return try self.alloc.dupe(u8, stdx.cstr.spanOrEmpty(str));
        }
    }

    pub fn valueToIntSlice(self: *JsEngine, val: JsValue) ![]const i32 {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            const len = self.getPropertyStr(val, "length").getInt32();
            const buf = try self.alloc.alloc(i32, @intCast(usize, len));
            var i: u32 = 0;
            while (i < len) : (i += 1) {
                buf[i] = self.getPropertyIndex(val, i).getInt32();
            }
            return buf;
        }
    }

    pub fn callMethod(self: *JsEngine, this: JsValue, prop: [:0]const u8, args: []const JsValue) JsValue {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            const atom = qjs.JS_NewAtom(self.inner.ctx, prop);
            defer qjs.JS_FreeAtom(self.inner.ctx, atom);
            return .{
                .inner = qjs.JS_Invoke(self.inner.ctx, this.inner, atom, @intCast(c_int, args.len), @intToPtr([*c]qjs.JSValue, @ptrToInt(args.ptr))),
            };
        }
    }

    pub fn callGen(self: *JsEngine, func: JsValue, this: JsValue, args: []const JsValue) JsValue {
        if (IsWasm) {
            self.inner.u32_buf.resize(self.alloc, args.len) catch fatal();
            if (args.len > 0) {
                var i: u32 = 0;
                while (i < args.len) : (i += 1) {
                    self.inner.u32_buf.items[i] = args[i].inner.id;
                }
            }
            const id = jsCallGen(func.inner.id, this.inner.id, self.inner.u32_buf.items.ptr, self.inner.u32_buf.items.len);
            return JsValue.initWebJS(id);
        } else {
            stdx.unsupported();
        }
    }

    pub fn call(self: *JsEngine, func: JsValue, this: JsValue, args: []const JsValue) JsValue {
        if (IsWasm) {
            self.inner.u32_buf.resize(self.alloc, args.len) catch fatal();
            if (args.len > 0) {
                var i: u32 = 0;
                while (i < args.len) : (i += 1) {
                    self.inner.u32_buf.items[i] = args[i].inner.id;
                }
            }
            const id = jsCall(func.inner.id, this.inner.id, self.inner.u32_buf.items.ptr, self.inner.u32_buf.items.len);
            return JsValue.initWebJS(id);
        } else {
            const val = qjs.JS_Call(self.inner.ctx, func.inner, this.inner, @intCast(c_int, args.len), @intToPtr([*c]qjs.JSValue, @ptrToInt(args.ptr)));
            return JsValue.initQJS(val);
        }
    }

    pub fn getPropertyIndex(self: *JsEngine, obj: JsValue, idx: u32) JsValue {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            const val = qjs.JS_GetPropertyUint32(self.inner.ctx, obj.inner, idx);
            return JsValue.initQJS(val);
        }
    }

    pub fn getPropertyStr(self: *JsEngine, obj: JsValue, prop: [:0]const u8) JsValue {
        if (IsWasm) {
            return JsValue{
                .inner = .{
                    .id = jsGetProperty(obj.inner.id, prop.ptr, prop.len),
                },
            };
        } else {
            const val = qjs.JS_GetPropertyStr(self.inner.ctx, obj.inner, prop);
            return JsValue.initQJS(val);
        }
    }

    pub fn setPropertyStr(self: *JsEngine, obj: JsValue, prop: [:0]const u8, val: JsValue) !void {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            const res = qjs.JS_SetPropertyStr(self.inner.ctx, obj.inner, prop, val.inner);
            if (res != 1) {
                return error.RuntimeException;
            }
        }
    }

    pub fn getGlobalVar(self: JsEngine, name: [:0]const u8) JsValue {
        _ = self;
        if (IsWasm) {
            return JsValue.initWebJS(jsGetGlobalValue(name.ptr, name.len));
        } else {
            stdx.unsupported();
        }
    }

    pub fn getGlobal(self: JsEngine) JsValue {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            return JsValue.initQJS(qjs.JS_GetGlobalObject(self.inner.ctx));
        }
    }
};

pub const JsValue = struct {
    inner: if (IsWasm) WebJsValue else qjs.JSValue,

    const NanBoxing = @sizeOf(?*anyopaque) == 4;

    pub fn initWebJS(id: u32) JsValue {
        return .{
            .inner = .{
                .id = id,
            },
        };
    }

    pub fn initQJS(val: qjs.JSValue) JsValue {
        return .{
            .inner = val,
        };
    }

    inline fn getValuePtr(self: JsValue) ?*anyopaque {
        if (NanBoxing) {
            return @intToPtr(?*anyopaque, self.inner);
        } else return @intToPtr(?*anyopaque, self.inner.u.ptr);
    }

    pub fn getTag(self: JsValue, engine: JsEngine) JsValueType {
        return engine.getValueTag(self);
    }

    pub fn getInt32(self: JsValue) i32 {
        if (IsWasm) {
            return jsGetInt32(self.inner.id);
        } else {
            return QJS.getInt32(self.inner);
        }
    }

    pub fn isInt32(self: JsValue, engine: JsEngine) bool {
        return self.getTag(engine) == .int32;
    }

    pub fn isNull(self: JsValue) bool {
        if (IsWasm) {
            stdx.unsupported();
        } else {
            return QJS.getQjsTag(self.inner) == qjs.JS_TAG_NULL;
        }
    }

    pub fn getBool(self: JsValue) bool {
        return QJS.getBool(self.inner);
    }

    pub fn getInt32Err(self: JsValue) !i32 {
        if (!QJS.isInt32(self.inner)) {
            log.debug("{} {}", .{QJS.getQjsTag(self.inner), qjs.JS_TAG_INT});
            return error.BadType;
        }
        return QJS.getInt32(self.inner);
    }

    pub fn getFloat64(self: JsValue) f64 {
        if (IsWasm) {
            return jsGetFloat64(self.inner.id);
        } else {
            return QJS.getFloat64(self.inner);
        }
    }
};

pub const JsContext = if (IsWasm) void else qjs.JSContext;

pub const QJS = struct {
    const NanBoxing = @sizeOf(?*anyopaque) == 4;

    pub fn getInt32(val: qjs.JSValue) i32 {
        return val.u.int32;
    }

    pub fn getFloat64(val: qjs.JSValue) f64 {
        return val.u.float64;
    }

    pub fn getBool(val: qjs.JSValue) bool {
        return val.u.int32 == 1;
    }

    pub fn getQjsTag(val: qjs.JSValue) i64 {
        return if (NanBoxing) b: {
            // Nan Boxing.
            break :b val >> 32;
        } else val.tag;
    }

    pub fn isInt32(val: qjs.JSValue) bool {
        return getQjsTag(val) == qjs.JS_TAG_INT;
    }

    pub fn getTag(ctx: *qjs.JSContext, val: qjs.JSValue) JsValueType {
        const tag = if (NanBoxing) b: {
            // Nan Boxing.
            break :b val >> 32;
        } else val.tag;
        return switch (tag) {
            qjs.JS_TAG_BOOL => .boolean,
            qjs.JS_TAG_STRING => .string,
            qjs.JS_TAG_EXCEPTION => .exception,
            qjs.JS_TAG_FLOAT64 => .float64,
            qjs.JS_TAG_SYMBOL => .symbol,
            qjs.JS_TAG_OBJECT => {
                if (qjs.JS_IsFunction(ctx, val) == 1) {
                    return .function;
                } else {
                    return .object;
                }
            },
            qjs.JS_TAG_NULL => .jsnull,
            qjs.JS_TAG_UNDEFINED => .undef,
            qjs.JS_TAG_INT => .int32,
            else => {
                if (@sizeOf(?*anyopaque) == 4) {
                    if (tag - qjs.JS_TAG_FIRST >= qjs.JS_TAG_FLOAT64 - qjs.JS_TAG_FIRST) {
                        return .float64;
                    }
                }
                stdx.panicFmt("unknown tag: {}", .{tag});
            }
        };
    }
};

pub const WebJsValue = struct {
    id: u32,
};

pub const JsValueType = enum(u4) {
    string = 0,
    exception = 1,
    float64 = 2,
    object = 3,
    undef = 4,
    int32 = 5,
    function = 6,
    boolean = 7,
    symbol = 8,
    jsnull = 9,
};