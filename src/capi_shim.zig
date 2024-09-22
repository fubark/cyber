const cy = @import("cyber.zig");
const C = @import("./capi.zig");

pub inline fn DECL_TYPE(out_type: **cy.Type) C.HostType {
    return C.DECL_TYPE(@ptrCast(out_type));
}

pub inline fn HOBJ_TYPE_PRE(out_type: ?**cy.Type, get_children: C.GetChildrenFn, finalizer: C.FinalizerFn) C.HostType {
    return C.HOBJ_TYPE_PRE(@ptrCast(out_type), get_children, finalizer);
}

pub inline fn HOBJ_TYPE(out_type: ?**cy.Type, get_children: C.GetChildrenFn, finalizer: C.FinalizerFn) C.HostType {
    return C.HOBJ_TYPE(@ptrCast(out_type), get_children, finalizer);
}

pub inline fn RESERVE_HOBJ_TYPE(id: cy.TypeId, out_type: ?**cy.Type) C.HostType {
    return C.RESERVE_HOBJ_TYPE(id, @ptrCast(out_type));
}

pub inline fn RESERVE_DECL_TYPE(id: cy.TypeId, out_type: ?**cy.Type) C.HostType {
    return C.RESERVE_DECL_TYPE(id, @ptrCast(out_type));
}

pub fn toFieldInit(name: []const u8, val: cy.Value) C.FieldInit {
    return .{ .name = C.toStr(name), .value = @bitCast(val) };
}