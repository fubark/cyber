const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const sema = cy.sema;
const types = cy.types;
const bt = types.BuiltinTypeSymIds;
const fmt = cy.fmt;
const v = fmt.v;
const vmc = cy.vmc;

pub const ModuleSymKey = cy.hash.KeyU64;

pub const ModuleId = u32;

pub const Module = struct {
    syms: std.HashMapUnmanaged(ModuleSymKey, ModuleSym, cy.hash.KeyU64Context, 80),

    id: ModuleId,

    /// Chunk that contains the module declaration. `NullId` if this module is a builtin.
    chunkId: cy.ChunkId,

    /// The root resolved symbol for this Module.
    /// This is duped from Chunk for user modules, but for builtins, it's only available here.
    resolvedRootSymId: sema.SymbolId,

    /// Owned absolute specifier path.
    /// If this is a submodule, it is a relative name.
    absSpec: []const u8,

    pub fn setNativeTypedFunc(self: *Module, c: *cy.VMcompiler, name: []const u8,
        sig: []const sema.SymbolId, retSymId: sema.SymbolId, func: cy.ZHostFuncFn) !void {
        return self.setNativeTypedFuncExt(c, name, false, sig, retSymId, func);
    } 

    pub fn setNativeTypedFuncExt(self: *Module, c: *cy.VMcompiler, name: []const u8, dupeName: bool,
        sig: []const sema.SymbolId, retSymId: sema.SymbolId, func: cy.ZHostFuncFn) !void {
        const nameId = try sema.ensureNameSymExt(c, name, dupeName);

        // AnyType for params and return.
        const funcSigId = try sema.ensureFuncSig(c, sig, retSymId);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = funcSigId,
            },
        };
        const res = try self.syms.getOrPut(c.alloc, key);
        res.value_ptr.* = .{
            .symT = .hostFunc,
            .inner = .{
                .hostFunc = .{
                    .func = @ptrCast(func),
                },
            },
        };
        if (!res.found_existing) {
            try declareFuncForNameSym(c, self, nameId, funcSigId);
        }
    }

    pub fn setNativeFunc(self: *Module, c: *cy.VMcompiler, name: []const u8, numParams: u32, func: cy.ZHostFuncFn) !void {
        return self.setNativeFuncExt(c, name, false, numParams, func);
    }

    pub fn setNativeFuncExt(self: *Module, c: *cy.VMcompiler, name: []const u8, dupeName: bool, numParams: u32, func: cy.ZHostFuncFn) !void {
        const nameId = try sema.ensureNameSymExt(c, name, dupeName);

        // AnyType for params and return.
        const funcSigId = try sema.ensureResolvedUntypedFuncSig(c, numParams);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = funcSigId,
            },
        };
        const res = try self.syms.getOrPut(c.alloc, key);
        res.value_ptr.* = .{
            .symT = .hostFunc,
            .inner = .{
                .hostFunc = .{
                    .func = @ptrCast(func),
                },
            },
        };
        if (!res.found_existing) {
            try declareFuncForNameSym(c, self, nameId, funcSigId);
        }
    }

    pub fn getVarVal(self: *const Module, c: *cy.VMcompiler, name: []const u8) !?cy.Value {
        const nameId = try sema.ensureNameSym(c, name);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        if (self.syms.get(key)) |sym| {
            return sym.inner.variable.val;
        } else return null;
    }

    pub fn setTypedVar(self: *Module, c: *cy.VMcompiler, name: []const u8, typeSymId: sema.SymbolId, val: cy.Value) !void {
        try self.setVarExt(c, name, false, typeSymId, val);
    }

    pub fn setVar(self: *Module, c: *cy.VMcompiler, name: []const u8, val: cy.Value) !void {
        try self.setVarExt(c, name, false, bt.Any, val);
    }

    pub fn setVarExt(self: *Module, c: *cy.VMcompiler, name: []const u8, dupeName: bool, typeSymId: sema.SymbolId, val: cy.Value) !void {
        const nameId = try sema.ensureNameSymExt(c, name, dupeName);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        try self.syms.put(c.alloc, key, .{
            .symT = .variable,
            .extra = .{
                .variable = .{
                    .rTypeSymId = typeSymId,
                },
            },
            .inner = .{
                .variable = .{
                    .val = val,
                },
            },
        });
    }

    pub fn setTypeAlias(self: *Module, c: *cy.VMcompiler, name: []const u8, declId: cy.NodeId) !void {
        const nameId = try sema.ensureNameSym(c, name);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        try self.syms.put(c.alloc, key, .{
            .symT = .typeAlias,
            .inner = .{
                .typeAlias = .{
                    .declId = declId,
                },
            },
        });
    }

    pub fn declareEnumMember(self: *Module, c: *cy.VMcompiler, name: []const u8, enumId: u32, memberId: u32) !void {
        const nameId = try sema.ensureNameSym(c, name);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        if (self.syms.contains(key)) {
            return error.DuplicateSymName;
        }
        try self.syms.put(c.alloc, key, .{
            .symT = .enumMember,
            .inner = .{
                .enumMember = .{
                    .rtEnumId = enumId,
                    .memberId = memberId,
                },
            },
        });
    }

    pub fn declareEnumType(self: *Module, c: *cy.VMcompiler, name: []const u8, rtEnumId: u32, modId: ModuleId) !void {
        const nameId = try sema.ensureNameSym(c, name);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        if (self.syms.contains(key)) {
            return error.DuplicateSymName;
        }
        try self.syms.put(c.alloc, key, .{
            .symT = .enumType,
            .inner = .{
                .enumType = .{
                    .rtEnumId = rtEnumId,
                    .modId = modId,
                },
            },
        });
    }

    pub fn setUserFunc(self: *Module, c: *cy.VMcompiler, name: []const u8, numParams: u32, declId: sema.FuncDeclId) !void {
        const nameId = try sema.ensureNameSym(c, name);

        const funcSigId = try sema.ensureResolvedUntypedFuncSig(c, numParams);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = funcSigId, 
            },
        };
        const res = try self.syms.getOrPut(c.alloc, key);
        res.value_ptr.* = .{
            .symT = .userFunc,
            .inner = .{
                .userFunc = .{
                    .declId = declId,
                },
            },
        };
        if (!res.found_existing) {
            try self.addFuncToSym(c, nameId, funcSigId);
        }
    }

    pub fn setUserVar(self: *Module, c: *cy.VMcompiler, name: []const u8, declId: cy.NodeId) !void {
        const nameId = try sema.ensureNameSym(c, name);
        const key = ModuleSymKey{
            .moduleSymKey = .{
                .nameId = nameId,
                .funcSigId = cy.NullId,
            },
        };
        try self.syms.put(c.alloc, key, .{
            .symT = .userVar,
            .inner = .{
                .userVar = .{
                    .declId = declId,
                },
            },
        });
    }

    pub fn deinit(self: *Module, alloc: std.mem.Allocator) void {
        var iter = self.syms.iterator();
        while (iter.next()) |e| {
            const sym = e.value_ptr.*;
            if (sym.symT == .symToManyFuncs) {
                var cur: ?*ModuleFuncNode = sym.inner.symToManyFuncs.head;
                while (cur != null) {
                    const next = cur.?.next;
                    alloc.destroy(cur.?);
                    cur = next;
                }
            }
        }
        self.syms.deinit(alloc);
        alloc.free(self.absSpec);
    }

    pub fn dump(self: *const Module, c: *cy.VMcompiler) void {
        std.debug.print("Module spec={s} ({} syms):\n", .{self.absSpec, self.syms.size});
        var iter = self.syms.iterator();
        while (iter.next()) |e| {
            const sym = e.value_ptr.*;
            const key = e.key_ptr.*.moduleSymKey;
            const name = sema.getName(c, key.nameId);
            std.debug.print("{s}: {}\n", .{name, sym.symT});
        }
    }
};

const ModuleSymType = enum {
    variable,
    hostFunc,

    /// Symbol that points to one function signature.
    symToOneFunc,

    /// Symbol that points to multiple overloaded functions.
    symToManyFuncs,

    userVar,
    userFunc,
    object,
    enumType,
    enumMember,
    userObject,
    typeAlias,
};

const ModuleSym = struct {
    symT: ModuleSymType,
    extra: union {
        variable: struct {
            rTypeSymId: sema.SymbolId,
        }
    } = undefined,
    inner: union {
        hostFunc: struct {
            func: vmc.HostFuncFn,
        },
        variable: struct {
            val: cy.Value,
        },
        symToOneFunc: struct {
            funcSigId: sema.FuncSigId,
        },
        symToManyFuncs: struct {
            head: *ModuleFuncNode,
        },
        userVar: struct {
            declId: cy.NodeId,
        },
        typeAlias: extern struct {
            /// Type aliases are lazily loaded.
            declId: cy.NodeId,
        },
        userFunc: struct {
            declId: sema.FuncDeclId,
            hasStaticInitializer: bool,
        },
        object: struct {
            modId: ModuleId,
            declId: cy.NodeId,
        },
        enumType: struct {
            rtEnumId: u32,
            modId: ModuleId,
        },
        enumMember: struct {
            rtEnumId: u32,
            memberId: u32,
        },
        userObject: struct {
            declId: cy.NodeId,
        },
    },
};

const ModuleFuncNode = struct {
    next: ?*ModuleFuncNode,
    funcSigId: sema.FuncSigId,
};

pub fn declareTypeObject(c: *cy.VMcompiler, modId: ModuleId, name: []const u8, chunkId: cy.ChunkId, declId: cy.NodeId) !ModuleId {
    const nameId = try sema.ensureNameSym(c, name);
    const key = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = cy.NullId,
        },
    };
    if (c.sema.getModule(modId).syms.contains(key)) {
        return error.DuplicateSymName;
    }

    const spec = try std.fmt.allocPrint(c.alloc, "{s}.{s}", .{c.sema.getModule(modId).absSpec, name});
    defer c.alloc.free(spec);

    const objModId = try appendModule(c, spec);
    c.sema.getModulePtr(objModId).chunkId = chunkId;

    try c.sema.getModulePtr(modId).syms.put(c.alloc, key, .{
        .symT = .object,
        .inner = .{
            .object = .{
                .modId = objModId,
                .declId = declId,
            },
        },
    });
    return objModId;
}

pub fn declareHostFunc(
    c: *cy.VMcompiler, modId: ModuleId, name: []const u8, funcSigId: sema.FuncSigId,
    declId: sema.FuncDeclId, funcPtr: vmc.HostFuncFn,
) !void {
    _ = declId;
    const nameId = try sema.ensureNameSym(c, name);
    const key = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = funcSigId, 
        },
    };
    const mod = c.sema.getModulePtr(modId);
    if (mod.syms.contains(key)) {
        return error.DuplicateFuncSig;
    }

    try mod.syms.put(c.alloc, key, .{
        .symT = .hostFunc,
        .inner = .{
            .hostFunc = .{
                .func = funcPtr,
            },
        },
    });

    try declareFuncForNameSym(c, mod, nameId, funcSigId);
}

pub fn declareUserFunc(
    c: *cy.VMcompiler, modId: ModuleId, name: []const u8, funcSigId: sema.FuncSigId,
    declId: sema.FuncDeclId, hasStaticInitializer: bool
) !void {
    const nameId = try sema.ensureNameSym(c, name);
    const key = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = funcSigId, 
        },
    };
    const mod = c.sema.getModulePtr(modId);
    if (mod.syms.contains(key)) {
        return error.DuplicateFuncSig;
    }

    try mod.syms.put(c.alloc, key, .{
        .symT = .userFunc,
        .inner = .{
            .userFunc = .{
                .declId = declId,
                .hasStaticInitializer = hasStaticInitializer,
            },
        },
    });

    try declareFuncForNameSym(c, mod, nameId, funcSigId);
}

fn declareFuncForNameSym(
    c: *cy.VMcompiler, mod: *Module, nameId: sema.NameSymId, funcSigId: sema.FuncSigId
) !void {
    const key = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = cy.NullId,
        },
    };
    const res = try mod.syms.getOrPut(c.alloc, key);
    if (res.found_existing) {
        if (res.value_ptr.*.symT == .symToOneFunc) {
            const first = try c.alloc.create(ModuleFuncNode);
            first.* = .{
                .funcSigId = res.value_ptr.*.inner.symToOneFunc.funcSigId,
                .next = null,
            };
            const new = try c.alloc.create(ModuleFuncNode);
            new.* = .{
                .funcSigId = funcSigId,
                .next = first,
            };
            res.value_ptr.* = .{
                .symT = .symToManyFuncs,
                .inner = .{
                    .symToManyFuncs = .{
                        .head = new,
                    },
                },
            };
        } else if (res.value_ptr.*.symT == .symToManyFuncs) {
            const new = try c.alloc.create(ModuleFuncNode);
            new.* = .{
                .funcSigId = funcSigId,
                .next = res.value_ptr.*.inner.symToManyFuncs.head,
            };
            res.value_ptr.*.inner.symToManyFuncs.head = new;
        } else {
            return error.DuplicateSymName;
        }
    } else {
        res.value_ptr.* = .{
            .symT = .symToOneFunc,
            .inner = .{
                .symToOneFunc = .{
                    .funcSigId = funcSigId,
                },
            },
        };
    }
}

pub fn findDistinctModuleSym(chunk: *cy.Chunk, modId: ModuleId, nameId: sema.NameSymId) !bool {
    const relKey = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = cy.NullId,
        },
    };

    const mod = chunk.compiler.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        switch (modSym.symT) {
            .userVar,
            .userFunc,
            .variable,
            .object,
            .enumType,
            .enumMember,
            .typeAlias,
            .symToOneFunc,
            .userObject,
            .hostFunc => {
                return true;
            },
            .symToManyFuncs => {
                // More than one func for sym.
                const name = sema.getName(chunk.compiler, nameId);
                return chunk.reportError("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)});
            },
        }
    }
    return false;
}

// fn pushModuleFuncCandidates(c: *cy.Chunk, modId: ModuleId, nameId: NameSymId, numParams: u32) !void {
//     const key = ModuleSymKey{
//         .moduleSymKey = .{
//             .nameId = nameId,
//             .funcSigId = cy.NullId,
//         },
//     };
//     const mod = c.compiler.sema.modules.items[modId];
//     if (mod.syms.get(key)) |modSym| {
//         switch (modSym.symT) {
//             .symToManyFuncs => {
//                 var optNode: ?*ModuleFuncNode = modSym.inner.symToManyFuncs.head;
//                 while (optNode) |node| {
//                     const funcSig = c.compiler.sema.getFuncSig(node.funcSigId);
//                     if (funcSig.paramLen == numParams) {
//                         try c.funcCandidateStack.append(c.alloc, .{
//                             .parentSymId = mod.resolvedRootSymId,
//                             .funcSigId = node.funcSigId,
//                         });
//                     }
//                     optNode = node.next;
//                 }
//             },
//             .symToOneFunc => {
//                 const funcSigId = modSym.inner.symToOneFunc.funcSigId;
//                 const funcSig = c.compiler.sema.getFuncSig(funcSigId);
//                 if (funcSig.paramLen == numParams) {
//                     try c.funcCandidateStack.append(c.alloc, .{
//                         .parentSymId = mod.resolvedRootSymId,
//                         .funcSigId = funcSigId,
//                     });
//                 }
//             },
//             else => {
//             },
//         }
//     }
// }

pub fn findModuleSymForDynamicFuncCall(
    chunk: *cy.Chunk, modId: ModuleId, nameId: sema.NameSymId,
) !?sema.FuncSigId {
    const relKey = ModuleSymKey{
        .moduleSymKey = .{
            .nameId = nameId,
            .funcSigId = cy.NullId,
        },
    };

    const mod = chunk.compiler.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        switch (modSym.symT) {
            .symToManyFuncs => {
                return chunk.reportError("Unsupported dynamic call to overloaded symbol.", &.{});
            },
            .symToOneFunc => {
                return modSym.inner.symToOneFunc.funcSigId;
            },
            else => {
            },
        }
    }
    return null;
}

const FindModuleFuncResult = struct {
    funcSigId: sema.FuncSigId,
    typeChecked: bool,
};

/// Finds the first function that matches the constrained signature.
pub fn findModuleSymForFuncCall(
    chunk: *cy.Chunk, modId: ModuleId, nameId: sema.NameSymId,
    args: []const types.TypeId, ret: types.TypeId, hasDynamicArg: bool,
) !?FindModuleFuncResult {
    const relKey = ModuleSymKey.initModuleSymKey(nameId, null);
    const mod = chunk.compiler.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        switch (modSym.symT) {
            .symToManyFuncs => {
                if (hasDynamicArg) {
                    return FindModuleFuncResult{
                        .funcSigId = modSym.inner.symToManyFuncs.head.funcSigId,
                        .typeChecked = true,
                    };
                } else {
                    var optNode: ?*ModuleFuncNode = modSym.inner.symToManyFuncs.head;
                    while (optNode) |node| {
                        if (cy.types.isTypeFuncSigCompat(chunk.compiler, args, ret, node.funcSigId)) {
                            return FindModuleFuncResult{
                                .funcSigId = node.funcSigId,
                                .typeChecked = true,
                            };
                        }
                        optNode = node.next;
                    }
                }
            },
            .symToOneFunc => {
                if (hasDynamicArg) {
                    return FindModuleFuncResult{
                        .funcSigId = modSym.inner.symToOneFunc.funcSigId,
                        .typeChecked = true,
                    };
                } else {
                    const funcSigId = modSym.inner.symToOneFunc.funcSigId;
                    if (cy.types.isTypeFuncSigCompat(chunk.compiler, args, ret, funcSigId)) {
                        return FindModuleFuncResult{
                            .funcSigId = funcSigId,
                            .typeChecked = true,
                        };
                    }
                }
            },
            else => {
            },
        }
    }
    return null;
}

pub fn appendModule(c: *cy.VMcompiler, name: []const u8) !ModuleId {
    const nameDupe = try c.alloc.dupe(u8, name);

    // Add empty module placeholder.
    const id: u32 = @intCast(c.sema.modules.items.len);
    try c.sema.modules.append(c.alloc, .{
        .id = id,
        .syms = .{},
        .chunkId = cy.NullId,
        // Updated afterwards.
        .resolvedRootSymId = cy.NullId,
        .absSpec = nameDupe,
    });
    if (builtin.mode == .Debug) {
        if (c.sema.moduleMap.contains(nameDupe)) {
            cy.panicFmt("Duplicate module: {s}", .{nameDupe});
        }
    }
    try c.sema.moduleMap.put(c.alloc, nameDupe, id);
    return id;
}

test "module internals" {
    if (cy.is32Bit) {
        try t.eq(@sizeOf(ModuleFuncNode), 8);
    } else {
        try t.eq(@sizeOf(ModuleFuncNode), 16);
    }
    if (builtin.mode == .Debug) {
        try t.eq(@sizeOf(ModuleSym), 24);
    } else {
        try t.eq(@sizeOf(ModuleSym), 16);
    }
}