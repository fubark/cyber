const std = @import("std");
const builtin = @import("builtin");
const stdx = @import("stdx");
const t = stdx.testing;
const cy = @import("cyber.zig");
const types = cy.types;
const Type = types.Type;
const bt = types.BuiltinTypeSymIds;
const Nullable = cy.Nullable;
const fmt = @import("fmt.zig");
const v = fmt.v;

const vm_ = @import("vm.zig");

const log = stdx.log.scoped(.sema);

const ValueAddrType = enum {
    frameOffset,
};

const ValueAddr = struct {
    addrT: ValueAddrType,
    inner: union {
        frameOffset: u32,
    },
};

const RegisterId = u8;

pub const LocalVarId = u32;

/// Represents a variable in a block.
/// If the variable was declared `static`, references to it will use a static symbol instead.
/// Other variables are given reserved registers on the stack frame.
/// Captured variables have box values at runtime.
pub const LocalVar = struct {
    /// The current type of the var as the ast is traversed.
    /// This is updated when there is a variable assignment or a child block returns.
    vtype: Type,

    /// Whether this var is a captured function param.
    isCaptured: bool = false,

    /// Whether this var references a static variable.
    isStaticAlias: bool = false,

    /// Whether the variable references a captured/static var from a modifier or implicity from a read reference.
    hasCaptureOrStaticModifier: bool = false,

    /// Currently a captured var always needs to be boxed.
    /// In the future, the concept of a const variable could change this.
    isBoxed: bool = false,

    /// Whether this is a function param. (Does not include captured vars.)
    isParam: bool = false,

    /// Indicates that at some point during the vars lifetime it was an rcCandidate.
    /// Since all exit paths jump to the same release inst, this flag is used to determine
    /// which vars need a release.
    lifetimeRcCandidate: bool,

    /// There are two cases where the compiler needs to implicitly generate var initializers.
    /// 1. Var is first assigned in a branched block. eg. Assigned inside if block.
    ///    Since the var needs to be released at the end of the root block,
    ///    it needs to have a defined value.
    /// 2. Var is first assigned in an iteration block.
    /// 3. Var's lifetimeRcCandidate becomes true and is assigned on the main branch to 
    ///    expression that can throw. If the expression throws, the var needs to be defined
    ///    for the deinitializers. Currently, every expression is assumed to throw. 
    /// At the beginning of codegen for this block, these vars will be inited to the `none` value.
    genInitializer: bool = false,

    /// Local register offset assigned to this var.
    /// Locals are relative to the stack frame's start position.
    local: RegisterId = undefined,

    /// Since the same sema var is used later by codegen,
    /// use a flag to indicate whether the var has been loosely defined in the block. (eg. assigned to lvalue)
    /// Note that assigning inside a branch counts as defined.
    /// Entering an iter block will auto mark those as defined since the var could have been assigned by a previous iteration.
    genIsDefined: bool = false,

    inner: extern union {
        staticAlias: extern struct {
            crSymId: CompactResolvedSymId,
        }
    } = undefined,

    name: if (builtin.mode == .Debug) []const u8 else void,
};

pub const CapVarDesc = packed union {
    /// The user of a captured var contains the SemaVarId back to the owner's var.
    user: LocalVarId,
};

const VarAndType = struct {
    id: LocalVarId,
    vtype: Type,
};

pub const SubBlockId = u32;

pub const SubBlock = struct {
    /// Save var start types for entering a codegen iter block.
    /// This can only be determined after the sema pass.
    /// This is used to initialize the var type when entering the codegen iter block so
    /// that the first `genSetVar` produces the correct `set` op.
    iterVarBeginTypes: std.ArrayListUnmanaged(VarAndType),

    /// Record any merged narrow types at the end of the subblock for codegen.
    endMergeTypes: std.ArrayListUnmanaged(VarAndType),

    /// Track which vars were assigned to in the current sub block.
    /// If the var was first assigned in a parent sub block, the type is saved in the map to
    /// be merged later with the ending var type.
    /// Can be freed after the end of block.
    prevVarTypes: std.AutoHashMapUnmanaged(LocalVarId, Type),

    /// Start of vars assigned in this block in `assignedVarStack`.
    /// When leaving this block, all assigned var types in this block are merged
    /// back to the parent scope.
    assignedVarStart: u32,

    /// Previous sema sub block.
    /// When this sub block ends, the previous sub block id is set as the current.
    prevSubBlockId: SubBlockId,

    pub fn init(prevSubBlockId: SubBlockId, assignedVarStart: usize) SubBlock {
        return .{
            .assignedVarStart = @intCast(u32, assignedVarStart),
            .iterVarBeginTypes = .{},
            .endMergeTypes = .{},
            .prevVarTypes = .{},
            .prevSubBlockId = prevSubBlockId,
        };
    }

    pub fn deinit(self: *SubBlock, alloc: std.mem.Allocator) void {
        self.iterVarBeginTypes.deinit(alloc);
        self.endMergeTypes.deinit(alloc);
    }
};

pub const BlockId = u32;

pub const Block = struct {
    /// Local vars defined in this block. Does not include function params.
    locals: std.ArrayListUnmanaged(LocalVarId),

    /// Param vars for function blocks. Includes captured vars for closures.
    /// Captured vars are always at the end since the function params are known from the start.
    /// Codegen will reserve these first for the calling convention layout.
    params: std.ArrayListUnmanaged(LocalVarId),

    /// Name to var.
    /// This can be deinited after ending the sema block.
    nameToVar: std.StringHashMapUnmanaged(LocalVarId),

    /// First sub block id is recorded so the rest can be obtained by advancing
    /// the id in the same order it was traversed in the sema pass.
    firstSubBlockId: SubBlockId,

    /// Current sub block depth.
    subBlockDepth: u32,

    /// Index into `CompileChunk.funcDecls`. Main block if `NullId`.
    funcDeclId: u32,

    /// Whether this block belongs to a static function.
    isStaticFuncBlock: bool,

    /// Whether temporaries (nameToVar) was deinit.
    deinitedTemps: bool,

    pub fn init(funcDeclId: cy.NodeId, firstSubBlockId: SubBlockId, isStaticFuncBlock: bool) Block {
        return .{
            .nameToVar = .{},
            .locals = .{},
            .params = .{},
            .subBlockDepth = 0,
            .funcDeclId = funcDeclId,
            .firstSubBlockId = firstSubBlockId,
            .isStaticFuncBlock = isStaticFuncBlock,
            .deinitedTemps = false,
        };
    }

    pub fn deinit(self: *Block, alloc: std.mem.Allocator) void {
        self.locals.deinit(alloc);
        self.params.deinit(alloc);

        // Deinit for CompileError during sema.
        self.deinitTemps(alloc);
    }

    fn deinitTemps(self: *Block, alloc: std.mem.Allocator) void {
        if (!self.deinitedTemps) {
            self.nameToVar.deinit(alloc);
            self.deinitedTemps = true;
        }
    }

    fn getReturnType(self: *const Block, c: *cy.CompileChunk) !Type {
        if (self.funcDeclId != cy.NullId) {
            return c.semaFuncDecls.items[self.funcDeclId].getReturnType(c);
        } else {
            return types.AnyType;
        }
    }
};

pub fn getBlockNodeId(c: *cy.CompileChunk, block: *Block) cy.NodeId {
    if (block.funcDeclId == cy.NullId) {
        return c.parserAstRootId;
    } else {
        const decl = c.semaFuncDecls.items[block.funcDeclId];
        return decl.nodeId;
    }
}

pub const NameSymId = u32;

pub const Name = struct {
    ptr: [*]const u8,
    len: u32,
    owned: bool,

    pub fn getName(self: Name) []const u8 {
        return self.ptr[0..self.len];
    }
};

pub fn getName(c: *const cy.VMcompiler, nameId: NameSymId) []const u8 {
    const name = c.sema.nameSyms.items[nameId];
    return name.ptr[0..name.len];
}

/// This is only called after symbol resolving.
pub fn symHasStaticInitializer(c: *const cy.CompileChunk, crSymId: CompactResolvedSymId) bool {
    if (crSymId.isFuncSymId) {
        return c.compiler.sema.getResolvedFuncSym(crSymId.id).hasStaticInitializer;
    } else {
        const rsym = c.compiler.sema.getResolvedSym(crSymId.id);
        if (rsym.symT == .variable) {
            return rsym.inner.variable.declId != cy.NullId;
        }
    }
    return false;
}

pub const ResolvedFuncSymId = u32;
pub const ResolvedFuncSym = struct {
    chunkId: CompileChunkId,
    /// DeclId can be the cy.NullId for native functions.
    declId: u32,

    /// Access to rSymId, rFuncSigId.
    key: AbsResolvedFuncSymKey,

    /// Return type.
    retType: Type,

    /// Whether this func has a static initializer.
    hasStaticInitializer: bool,
    
    /// Whether the symbol has been or is in the process of generating it's static initializer.
    genStaticInitVisited: bool = false,

    pub fn getResolvedSymId(self: ResolvedFuncSym) ResolvedSymId {
        return self.key.absResolvedFuncSymKey.rSymId;
    }

    pub fn getResolvedFuncSigId(self: ResolvedFuncSym) ResolvedFuncSigId {
        return self.key.absResolvedFuncSymKey.rFuncSigId;
    }
};

const ResolvedSymType = enum {
    func,
    variable,
    object,
    enumType,
    enumMember,
    module,
    builtinType,
};

pub const ResolvedSymId = u32;

const ResolvedSymData = extern union {
    func: extern struct {
        /// Refers to exactly one resolved func sym.
        /// rFuncSymId == cy.NullId indicates this sym is overloaded;
        /// more than one func shares the same symbol. To disambiguate,
        /// `resolvedFuncSymMap` must be queried with a absResolvedFuncSymKey.
        rFuncSymId: ResolvedFuncSymId,
    },
    variable: extern struct {
        chunkId: CompileChunkId,
        declId: cy.NodeId,
        rTypeSymId: ResolvedSymId,
    },
    object: extern struct {
        modId: ModuleId,
    },
    enumType: extern struct {
        modId: ModuleId,
    },
    enumMember: extern struct {
        enumId: u32,
        memberId: u32,
    },
    module: extern struct {
        id: ModuleId,
    },
    builtinType: extern struct {
        modId: ModuleId,
        // TypeTag.
        typeT: u8,
    },
};

/// Local symbols are resolved during and after the sema pass.
/// Not all module members from builtins are resolved, only ones that are used.
pub const ResolvedSym = struct {
    symT: ResolvedSymType,
    /// Used to backtrack and build the full sym name.
    key: AbsResolvedSymKey,
    inner: ResolvedSymData,
    /// Whether the symbol is exported.
    exported: bool,
    /// Whether the symbol has been or is in the process of generating it's static initializer.
    genStaticInitVisited: bool = false,

    pub fn getObjectTypeId(self: ResolvedSym, vm: *cy.VM) ?cy.TypeId {
        return vm.getObjectTypeId(self.key.absResolvedSymKey.rParentSymId, self.key.absResolvedSymKey.nameId);
    }

    pub fn getModuleId(self: ResolvedSym) ?ModuleId {
        switch (self.symT) {
            .module => {
                return self.inner.module.id;
            },
            .enumType => {
                return self.inner.enumType.modId;
            },
            .object => {
                return self.inner.object.modId;
            },
            .builtinType => {
                return self.inner.builtinType.modId;
            },
            .enumMember,
            .func,
            .variable => {
                return null;
            },
        }
    }
};

/// Additional info attached to a initializer symbol.
pub const InitializerSym = struct {
    /// This points to a list of sema sym ids in `bufU32` that it depends on for initialization.
    depsStart: u32,
    depsEnd: u32,
};

const RelModuleSymKey = vm_.KeyU64;
const CompileChunkId = u32;

pub const ModuleId = u32;
pub const Module = struct {
    syms: std.HashMapUnmanaged(RelModuleSymKey, ModuleSym, vm_.KeyU64Context, 80),

    id: ModuleId,

    /// Attached chunk id. `NullId` if this module is a builtin.
    chunkId: CompileChunkId,

    /// The root resolved symbol for this Module.
    /// This is duped from CompileChunk for user modules, but for builtins, it's only available here.
    resolvedRootSymId: ResolvedSymId,

    /// Owned absolute specifier path.
    /// If this is a submodule, it is a relative name.
    absSpec: []const u8,

    pub fn setNativeTypedFunc(self: *Module, c: *cy.VMcompiler, name: []const u8,
        sig: []const ResolvedSymId, retSymId: ResolvedSymId, func: cy.NativeFuncPtr) !void {
        return self.setNativeTypedFuncExt(c, name, false, sig, retSymId, func);
    } 

    pub fn setNativeTypedFuncExt(self: *Module, c: *cy.VMcompiler, name: []const u8, dupeName: bool,
        sig: []const ResolvedSymId, retSymId: ResolvedSymId, func: cy.NativeFuncPtr) !void {
        const nameId = try ensureNameSymExt(c, name, dupeName);

        // AnyType for params and return.
        const rFuncSigId = try ensureResolvedFuncSig(c, sig, retSymId);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = rFuncSigId,
            },
        };
        const res = try self.syms.getOrPut(c.alloc, key);
        res.value_ptr.* = .{
            .symT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = .{
                    .func = func,
                },
            },
        };
        if (!res.found_existing) {
            try self.addFuncToSym(c, nameId, rFuncSigId);
        }
    }

    pub fn setNativeFunc(self: *Module, c: *cy.VMcompiler, name: []const u8, numParams: u32, func: cy.NativeFuncPtr) !void {
        return self.setNativeFuncExt(c, name, false, numParams, func);
    }

    pub fn setNativeFuncExt(self: *Module, c: *cy.VMcompiler, name: []const u8, dupeName: bool, numParams: u32, func: cy.NativeFuncPtr) !void {
        const nameId = try ensureNameSymExt(c, name, dupeName);

        // AnyType for params and return.
        const rFuncSigId = try ensureResolvedUntypedFuncSig(c, numParams);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = rFuncSigId,
            },
        };
        const res = try self.syms.getOrPut(c.alloc, key);
        res.value_ptr.* = .{
            .symT = .nativeFunc1,
            .inner = .{
                .nativeFunc1 = .{
                    .func = func,
                },
            },
        };
        if (!res.found_existing) {
            try self.addFuncToSym(c, nameId, rFuncSigId);
        }
    }

    fn addFuncToSym(self: *Module, c: *cy.VMcompiler, nameId: NameSymId, rFuncSigId: ResolvedFuncSigId) !void {
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        const res = try self.syms.getOrPut(c.alloc, key);
        if (res.found_existing) {
            if (res.value_ptr.*.symT == .symToOneFunc) {
                const first = try c.alloc.create(ModuleFuncNode);
                first.* = .{
                    .rFuncSigId = res.value_ptr.*.inner.symToOneFunc.rFuncSigId,
                    .next = null,
                };
                const new = try c.alloc.create(ModuleFuncNode);
                new.* = .{
                    .rFuncSigId = rFuncSigId,
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
                    .rFuncSigId = rFuncSigId,
                    .next = res.value_ptr.*.inner.symToManyFuncs.head,
                };
                res.value_ptr.*.inner.symToManyFuncs.head = new;
            } else {
                stdx.panicFmt("Unexpected symT: {}", .{res.value_ptr.*.symT});
            }
        } else {
            res.value_ptr.* = .{
                .symT = .symToOneFunc,
                .inner = .{
                    .symToOneFunc = .{
                        .rFuncSigId = rFuncSigId,
                    },
                },
            };
        }
    }

    pub fn getVarVal(self: *const Module, c: *cy.VMcompiler, name: []const u8) !?cy.Value {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (self.syms.get(key)) |sym| {
            return sym.inner.variable.val;
        } else return null;
    }

    pub fn setTypedVar(self: *Module, c: *cy.VMcompiler, name: []const u8, typeSymId: ResolvedSymId, val: cy.Value) !void {
        try self.setVarExt(c, name, false, typeSymId, val);
    }

    pub fn setVar(self: *Module, c: *cy.VMcompiler, name: []const u8, val: cy.Value) !void {
        try self.setVarExt(c, name, false, bt.Any, val);
    }

    pub fn setVarExt(self: *Module, c: *cy.VMcompiler, name: []const u8, dupeName: bool, typeSymId: ResolvedSymId, val: cy.Value) !void {
        const nameId = try ensureNameSymExt(c, name, dupeName);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
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
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
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
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
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
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
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

    pub fn setTypeObject(self: *Module, c: *cy.VMcompiler, name: []const u8, typeId: cy.TypeId, modId: ModuleId) !void {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        try self.syms.put(c.alloc, key, .{
            .symT = .object,
            .inner = .{
                .object = .{
                    .typeId = typeId,
                    .modId = modId,
                },
            },
        });
    }

    pub fn setUserFunc(self: *Module, c: *cy.VMcompiler, name: []const u8, numParams: u32, declId: cy.NodeId) !void {
        const nameId = try ensureNameSym(c, name);

        const rFuncSigId = try ensureResolvedUntypedFuncSig(c, numParams);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = rFuncSigId, 
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
            try self.addFuncToSym(c, nameId, rFuncSigId);
        }
    }

    pub fn setUserTypedFunc(self: *Module, c: *cy.VMcompiler, name: []const u8, rFuncSigId: ResolvedFuncSigId, declId: cy.NodeId) !void {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = rFuncSigId, 
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
            try self.addFuncToSym(c, nameId, rFuncSigId);
        }
    }

    pub fn setUserVar(self: *Module, c: *cy.VMcompiler, name: []const u8, declId: cy.NodeId) !void {
        const nameId = try ensureNameSym(c, name);
        const key = RelModuleSymKey{
            .relModuleSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
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
            const key = e.key_ptr.*.relModuleSymKey;
            const name = getName(c, key.nameId);
            std.debug.print("{s}: {}\n", .{name, sym.symT});
        }
    }
};

pub const LocalSym = struct {
    /// Can be NullId since some syms are not resolved until they are used. eg. ImportAll syms
    rSymId: Nullable(ResolvedSymId),
    rFuncSymId: Nullable(ResolvedFuncSymId),

    /// Which module to find the sym in.
    rParentSymId: ResolvedSymId,
};

const ModuleSymType = enum {
    variable,
    nativeFunc1,

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
            rTypeSymId: ResolvedSymId,
        }
    } = undefined,
    inner: union {
        nativeFunc1: struct {
            func: *const fn (*cy.UserVM, [*]const cy.Value, u8) cy.Value,
        },
        variable: struct {
            val: cy.Value,
        },
        symToOneFunc: struct {
            rFuncSigId: ResolvedFuncSigId,
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
            declId: cy.NodeId,
        },
        object: struct {
            typeId: cy.TypeId,
            modId: ModuleId,
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
    rFuncSigId: ResolvedFuncSigId,
};

/// Relative symbol signature key. RelFuncSigKey is repurposed for variable syms when `numParams` == cy.NullId.
const RelSymSigKey = cy.RelFuncSigKey;
const RelSymSigKeyContext = cy.RelFuncSigKeyContext;
pub const RelLocalSymKey = vm_.KeyU64;

/// Absolute symbol signature key. AbsFuncSigKey is repurposed for variable syms when `numParams` == cy.NullId.
pub const AbsSymSigKey = vm_.KeyU64;
pub const AbsResolvedSymKey = vm_.KeyU64;
pub const AbsResolvedFuncSymKey = vm_.KeyU64;
pub const AbsSymSigKeyContext = cy.AbsFuncSigKeyContext;

pub fn semaStmts(self: *cy.CompileChunk, head: cy.NodeId, comptime attachEnd: bool) anyerror!void {
    var cur_id = head;
    while (cur_id != cy.NullId) {
        const node = self.nodes[cur_id];
        if (attachEnd) {
            if (node.next == cy.NullId) {
                try semaStmt(self, cur_id, false);
            } else {
                try semaStmt(self, cur_id, true);
            }
        } else {
            try semaStmt(self, cur_id, true);
        }
        cur_id = node.next;
    }
}

pub fn semaStmt(c: *cy.CompileChunk, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !void {
    // log.debug("sema stmt {}", .{node.node_t});
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    switch (node.node_t) {
        .pass_stmt => {
            return;
        },
        .expr_stmt => {
            _ = try semaExpr(c, node.head.child_head, discardTopExprReg);
        },
        .breakStmt => {
            return;
        },
        .continueStmt => {
            return;
        },
        .opAssignStmt => {
            const left = c.nodes[node.head.opAssignStmt.left];
            if (left.node_t == .ident) {
                const rtype = try semaExpr(c, node.head.opAssignStmt.right, false);
                _ = try assignVar(c, node.head.opAssignStmt.left, rtype, .assign);
            } else if (left.node_t == .accessExpr) {
                _ = try accessExpr(c, node.head.opAssignStmt.left, false);
                _ = try semaExpr(c, node.head.opAssignStmt.right, false);
            } else {
                return c.reportErrorAt("Assignment to the left {} is not allowed.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .assign_stmt => {
            const left = c.nodes[node.head.left_right.left];
            if (left.node_t == .ident) {
                const right = c.nodes[node.head.left_right.right];
                if (right.node_t == .matchBlock) {
                    const rtype = try matchBlock(c, node.head.left_right.right, true);
                    _ = try assignVar(c, node.head.left_right.left, rtype, .assign);
                } else {
                    const rtype = try semaExpr(c, node.head.left_right.right, false);
                    _ = try assignVar(c, node.head.left_right.left, rtype, .assign);
                }
            } else if (left.node_t == .arr_access_expr) {
                _ = try semaExpr(c, left.head.left_right.left, false);
                _ = try semaExpr(c, left.head.left_right.right, false);
                _ = try semaExpr(c, node.head.left_right.right, false);
            } else if (left.node_t == .accessExpr) {
                _ = try accessExpr(c, node.head.left_right.left, false);
                _ = try semaExpr(c, node.head.left_right.right, false);
            } else {
                return c.reportErrorAt("Assignment to the left {} is not allowed.", &.{fmt.v(left.node_t)}, nodeId);
            }
        },
        .varDecl => {
            try varDecl(c, nodeId, true);
        },
        .captureDecl => {
            const left = c.nodes[node.head.left_right.left];
            std.debug.assert(left.node_t == .ident);
            if (node.head.left_right.right != cy.NullId) {
                const rtype = try semaExpr(c, node.head.left_right.right, false);
                _ = try assignVar(c, node.head.left_right.left, rtype, .captureAssign);
            } else {
                _ = try assignVar(c, node.head.left_right.left, types.UndefinedType, .captureAssign);
            }
        },
        .staticDecl => {
            const left = c.nodes[node.head.left_right.left];
            std.debug.assert(left.node_t == .ident);
            if (node.head.left_right.right != cy.NullId) {
                const rtype = try semaExpr(c, node.head.left_right.right, false);
                _ = try assignVar(c, node.head.left_right.left, rtype, .staticAssign);
            } else {
                _ = try assignVar(c, node.head.left_right.left, types.UndefinedType, .staticAssign);
            }
        },
        .typeAliasDecl => {
            const nameN = c.nodes[node.head.typeAliasDecl.name];
            const name = c.getNodeTokenString(nameN);
            const nameId = try ensureNameSym(c.compiler, name);

            const rSymId = try getOrResolveTypeSymFromSpecNode(c, node.head.typeAliasDecl.typeSpecHead);
            const rSym = c.compiler.sema.getResolvedSym(rSymId);
            try setLocalSym(c, nameId, .{
                .rSymId = rSymId,
                .rFuncSymId = cy.NullId,
                .rParentSymId = rSym.key.absResolvedSymKey.rParentSymId,
            });
        },
        .enumDecl => {
            return;
        },
        .objectDecl => {
            try objectDecl(c, nodeId, true);
        },
        .funcDeclInit => {
            try funcDeclInit(c, nodeId, true);
        },
        .funcDecl => {
            try funcDecl(c, nodeId, false);
        },
        .whileCondStmt => {
            try pushIterSubBlock(c);

            _ = try semaExpr(c, node.head.whileCondStmt.cond, false);
            try semaStmts(c, node.head.whileCondStmt.bodyHead, false);

            try endIterSubBlock(c);
        },
        .whileOptStmt => {
            try pushIterSubBlock(c);

            const optt = try semaExpr(c, node.head.whileOptStmt.opt, false);
            if (node.head.whileOptStmt.some != cy.NullId) {
                _ = try ensureLocalBodyVar(c, node.head.whileOptStmt.some, types.AnyType);
                _ = try assignVar(c, node.head.whileOptStmt.some, optt, .assign);
            }

            try semaStmts(c, node.head.whileOptStmt.bodyHead, false);

            try endIterSubBlock(c);
        },
        .whileInfStmt => {
            try pushIterSubBlock(c);
            try semaStmts(c, node.head.child_head, false);
            try endIterSubBlock(c);
        },
        .for_iter_stmt => {
            try pushIterSubBlock(c);

            _ = try semaExpr(c, node.head.for_iter_stmt.iterable, false);

            if (node.head.for_iter_stmt.eachClause != cy.NullId) {
                const eachClause = c.nodes[node.head.for_iter_stmt.eachClause];
                if (eachClause.head.eachClause.key != cy.NullId) {
                    const keyv = try ensureLocalBodyVar(c, eachClause.head.eachClause.key, types.AnyType);
                    c.vars.items[keyv].genInitializer = true;
                }
                const valv = try ensureLocalBodyVar(c, eachClause.head.eachClause.value, types.AnyType);
                c.vars.items[valv].genInitializer = true;
            }

            try semaStmts(c, node.head.for_iter_stmt.body_head, false);
            try endIterSubBlock(c);
        },
        .for_range_stmt => {
            try pushIterSubBlock(c);

            if (node.head.for_range_stmt.eachClause != cy.NullId) {
                const eachClause = c.nodes[node.head.for_range_stmt.eachClause];
                _ = try ensureLocalBodyVar(c, eachClause.head.eachClause.value, types.NumberType);
            }

            const range_clause = c.nodes[node.head.for_range_stmt.range_clause];
            _ = try semaExpr(c, range_clause.head.left_right.left, false);
            _ = try semaExpr(c, range_clause.head.left_right.right, false);

            try semaStmts(c, node.head.for_range_stmt.body_head, false);
            try endIterSubBlock(c);
        },
        .matchBlock => {
            _ = try matchBlock(c, nodeId, false);
        },
        .if_stmt => {
            _ = try semaExpr(c, node.head.left_right.left, false);

            try pushSubBlock(c);
            try semaStmts(c, node.head.left_right.right, false);
            try endSubBlock(c);

            var elseClauseId = node.head.left_right.extra;
            while (elseClauseId != cy.NullId) {
                const elseClause = c.nodes[elseClauseId];
                if (elseClause.head.else_clause.cond == cy.NullId) {
                    try pushSubBlock(c);
                    try semaStmts(c, elseClause.head.else_clause.body_head, false);
                    try endSubBlock(c);
                    break;
                } else {
                    _ = try semaExpr(c, elseClause.head.else_clause.cond, false);

                    try pushSubBlock(c);
                    try semaStmts(c, elseClause.head.else_clause.body_head, false);
                    try endSubBlock(c);
                    elseClauseId = elseClause.head.else_clause.else_clause;
                }
            }
        },
        .tryStmt => {
            try pushSubBlock(c);
            try semaStmts(c, node.head.tryStmt.tryFirstStmt, false);
            try endSubBlock(c);

            try pushSubBlock(c);
            if (node.head.tryStmt.errorVar != cy.NullId) {
                _ = try ensureLocalBodyVar(c, node.head.tryStmt.errorVar, types.ErrorType);
            }
            try semaStmts(c, node.head.tryStmt.catchFirstStmt, false);
            try endSubBlock(c);
        },
        .importStmt => {
            return;
        },
        .return_stmt => {
            return;
        },
        .return_expr_stmt => {
            const block = curBlock(c);
            const retType = try block.getReturnType(c);
            _ = try semaExpr2(c, node.head.child_head, retType, false);
        },
        .atStmt => {
            return;
        },
        else => return c.reportErrorAt("Unsupported node: {}", &.{v(node.node_t)}, nodeId),
    }
}

pub fn declareTypeAlias(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.typeAliasDecl.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);

    // Check for local sym.
    const key = RelLocalSymKey{
        .relLocalSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };
    if (c.localSyms.contains(key)) {
        return c.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(c.compiler, nameId))}, node.head.typeAliasDecl.name);
    }

    const mod = c.getModule();
    try mod.setTypeAlias(c.compiler, name, nodeId);

    try setLocalSym(c, nameId, .{
        .rSymId = cy.NullId,
        .rFuncSymId = cy.NullId,
        .rParentSymId = c.semaResolvedRootSymId,
    });
}

pub fn declareImport(chunk: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const node = chunk.nodes[nodeId];
    const ident = chunk.nodes[node.head.left_right.left];
    const name = chunk.getNodeTokenString(ident);
    const nameId = try ensureNameSym(chunk.compiler, name);

    const spec = chunk.nodes[node.head.left_right.right];
    const specPath = chunk.getNodeTokenString(spec);

    const modId = try getOrLoadModule(chunk, specPath, nodeId);
    const mod = chunk.compiler.sema.getModule(modId);
    try setLocalSym(chunk, nameId, .{
        .rSymId = mod.resolvedRootSymId,
        .rFuncSymId = cy.NullId,
        .rParentSymId = cy.NullId,
    });
}

fn setLocalSym(chunk: *cy.CompileChunk, nameId: NameSymId, sym: LocalSym) !void {
    const key = RelLocalSymKey{
        .relLocalSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };
    try chunk.localSyms.put(chunk.alloc, key, sym);
}

fn matchBlock(c: *cy.CompileChunk, nodeId: cy.NodeId, canBreak: bool) !Type {
    const node = c.nodes[nodeId];
    _ = try semaExpr(c, node.head.matchBlock.expr, false);

    var curCase = node.head.matchBlock.firstCase;
    while (curCase != cy.NullId) {
        const case = c.nodes[curCase];
        var curCond = case.head.caseBlock.firstCond;
        while (curCond != cy.NullId) {
            const cond = c.nodes[curCond];
            if (cond.node_t != .elseCase) {
                _ = try semaExpr(c, curCond, false);
            }
            curCond = cond.next;
        }
        curCase = case.next;
    }

    curCase = node.head.matchBlock.firstCase;
    while (curCase != cy.NullId) {
        const case = c.nodes[curCase];
        try pushSubBlock(c);
        try semaStmts(c, case.head.caseBlock.firstChild, false);
        try endSubBlock(c);
        curCase = case.next;
    }

    if (canBreak) {
        return types.AnyType;
    } else {
        return types.UndefinedType;
    }
}

pub fn declareObjectMembers(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const rObjSymId = c.nodes[node.head.objectDecl.name].head.ident.sema_crSymId.id;

    const rObjSym = c.compiler.sema.getResolvedSym(rObjSymId);
    const objMod = c.compiler.sema.getModulePtr(rObjSym.inner.object.modId);

    var funcId = node.head.objectDecl.funcsHead;
    while (funcId != cy.NullId) {
        const declId = try appendFuncDecl(c, funcId, true);
        c.nodes[funcId].head.func.semaDeclId = declId;

        const func = &c.semaFuncDecls.items[declId];
        const funcN = c.nodes[funcId];

        if (func.numParams > 0) {
            const param = c.nodes[func.paramHead];
            const paramName = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);
            if (std.mem.eql(u8, paramName, "self")) {
                // Skip methods for now.

                funcId = funcN.next;
                continue;
            }
        }

        // Object function.
        const funcName = func.getName(c);
        const funcNameId = try ensureNameSym(c.compiler, funcName);

        // Export all static funcs in the object's namespace since `export` may be removed later on.
        const res = try resolveLocalFuncSym(c, rObjSymId, funcNameId, declId, func.rFuncSigId, true);
        func.rFuncSymId = res.rFuncSymId;
        func.rSymId = res.rSymId;

        try objMod.setUserTypedFunc(c.compiler, funcName, func.rFuncSigId, funcId);

        funcId = funcN.next;
    }
}

pub fn declareEnum(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.enumDecl.name];
    const name = c.getNodeTokenString(nameN);
    // const nameId = try ensureNameSym(c.compiler, name);

    const modId = try appendSubModule(c.compiler, name);
    const mod = c.compiler.sema.getModulePtr(modId);

    const eid = try c.compiler.vm.ensureEnum(name);

    var i: u32 = 0;
    var memberId = node.head.enumDecl.memberHead;
    var buf: std.ArrayListUnmanaged(NameSymId) = .{};
    defer buf.deinit(c.alloc);

    while (memberId != cy.NullId) : (i += 1) {
        const member = c.nodes[memberId];
        const mName = c.getNodeTokenString(member);
        const mNameId = try ensureNameSym(c.compiler, mName);
        try buf.append(c.alloc, mNameId);
        // const mSymId = try c.compiler.vm.ensureSymbol(mName);
        mod.declareEnumMember(c.compiler, mName, eid, i) catch |err| {
            if (err == error.DuplicateSymName) {
                return c.reportErrorAt("The enum symbol `{}` was already declared.", &.{v(mName)}, memberId);
            } else {
                return err;
            }
        };
        memberId = member.next;
    }
    c.compiler.vm.enums.buf[eid].members = try buf.toOwnedSlice(c.alloc);

    const chunkMod = c.compiler.sema.getModulePtr(c.modId);
    chunkMod.declareEnumType(c.compiler, name, eid, modId) catch |err| {
        if (err == error.DuplicateSymName) {
            return c.reportErrorAt("The symbol `{}` was already declared.", &.{v(name)}, nodeId);
        } else {
            return err;
        }
    };
}

pub fn declareObject(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.objectDecl.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);

    if (c.compiler.vm.getObjectTypeId(c.semaResolvedRootSymId, nameId) != null) {
        return c.reportErrorAt("Object type `{}` already exists", &.{v(name)}, nodeId);
    }

    const objModId = try appendSubModule(c.compiler, name);

    const rObjSymId = try resolveLocalObjectSym(c, c.semaResolvedRootSymId, name, objModId, nodeId, true);
    const objMod = c.compiler.sema.getModulePtr(objModId);
    objMod.resolvedRootSymId = rObjSymId;
    // Persist local sym for codegen.
    c.nodes[node.head.objectDecl.name].head.ident.sema_crSymId = CompactResolvedSymId.initSymId(rObjSymId);

    const sid = try c.compiler.vm.ensureObjectType(c.semaResolvedRootSymId, nameId, rObjSymId);

    const mod = c.compiler.sema.getModulePtr(c.modId);
    try mod.setTypeObject(c.compiler, name, sid, objModId);
}

fn objectDecl(c: *cy.CompileChunk, nodeId: cy.NodeId, exported: bool) !void {
    _ = exported;
    const node = c.nodes[nodeId];
    const nameN = c.nodes[node.head.objectDecl.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);

    const rSymId = nameN.head.ident.sema_crSymId.id;
    const sid = try c.compiler.vm.ensureObjectType(c.semaResolvedRootSymId, nameId, rSymId);

    var i: u32 = 0;
    var fieldId = node.head.objectDecl.fieldsHead;
    while (fieldId != cy.NullId) : (i += 1) {
        const field = c.nodes[fieldId];
        const fieldName = c.getNodeTokenString(field);
        const fieldSymId = try c.compiler.vm.ensureFieldSym(fieldName);
        try c.compiler.vm.addFieldSym(sid, fieldSymId, @intCast(u16, i));
        fieldId = field.next;
    }
    const numFields = i;
    c.compiler.vm.structs.buf[sid].numFields = numFields;

    var funcId = node.head.objectDecl.funcsHead;
    while (funcId != cy.NullId) {
        const declId = c.nodes[funcId].head.func.semaDeclId;

        const func = &c.semaFuncDecls.items[declId];
        const funcN = c.nodes[funcId];

        if (func.numParams > 0) {
            const param = c.nodes[func.paramHead];
            const paramName = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);
            if (std.mem.eql(u8, paramName, "self")) {
                // Struct method.
                const blockId = try pushBlock(c, funcN.head.func.semaDeclId);
                func.semaBlockId = blockId;
                errdefer endBlock(c) catch stdx.fatal();
                try pushMethodParamVars(c, func);
                try semaStmts(c, funcN.head.func.bodyHead, false);
                try endBlock(c);

                funcId = funcN.next;
                continue;
            }
        }

        // Object function.
        const blockId = try pushBlock(c, funcN.head.func.semaDeclId);
        func.semaBlockId = blockId;
        errdefer endBlock(c) catch stdx.fatal();
        try appendFuncParamVars(c, func);
        try semaStmts(c, funcN.head.func.bodyHead, false);
        try endFuncSymBlock(c, func.numParams);

        funcId = funcN.next;
    }
}

pub fn declareFuncInit(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const declId = try appendFuncDecl(c, nodeId, true);
    c.nodes[nodeId].head.func.semaDeclId = declId;

    const func = &c.semaFuncDecls.items[declId];
    const name = func.getName(c);
    const nameId = try ensureNameSym(c.compiler, name);

    const res = try resolveLocalFuncSym(c, c.semaResolvedRootSymId, nameId,
        @intCast(u32, declId), func.rFuncSigId, true);
    c.compiler.sema.resolvedFuncSyms.items[res.rFuncSymId].hasStaticInitializer = true;
    func.rFuncSymId = res.rFuncSymId;
    func.rSymId = res.rSymId;
    try c.compiler.sema.modules.items[c.modId].setUserTypedFunc(c.compiler, name, func.rFuncSigId, nodeId);
}

fn funcDeclInit(c: *cy.CompileChunk, nodeId: cy.NodeId, exported: bool) !void {
    _ = exported;
    const declId = c.nodes[nodeId].head.func.semaDeclId;

    const node = c.nodes[nodeId];
    const func = &c.semaFuncDecls.items[declId];

    const name = func.getName(c);

    c.curSemaInitingSym = CompactResolvedSymId.initFuncSymId(func.rFuncSymId);
    c.semaVarDeclDeps.clearRetainingCapacity();
    defer c.curSemaInitingSym = CompactResolvedSymId.initNull();

    _ = semaExpr(c, node.head.func.bodyHead, false) catch |err| {
        if (err == error.CanNotUseLocal) {
            const local = c.nodes[c.compiler.errorPayload];
            const localName = c.getNodeTokenString(local);
            return c.reportErrorAt("The declaration initializer of static function `{}` can not reference the local variable `{}`.", &.{v(name), v(localName)}, nodeId);
        } else {
            return err;
        }
    };
}

pub fn declareFunc(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const declId = try appendFuncDecl(c, nodeId, true);
    const func = &c.semaFuncDecls.items[declId];
    const name = func.getName(c);
    const nameId = try ensureNameSym(c.compiler, name);
    c.nodes[nodeId].head.func.semaDeclId = declId;

    const res = try resolveLocalFuncSym(c, c.semaResolvedRootSymId, nameId,
        @intCast(u32, declId), func.rFuncSigId, true);
    func.rFuncSymId = res.rFuncSymId;
    func.rSymId = res.rSymId;
    try c.compiler.sema.modules.items[c.modId].setUserTypedFunc(c.compiler, name, func.rFuncSigId, nodeId);
}

fn funcDecl(c: *cy.CompileChunk, nodeId: cy.NodeId, exported: bool) !void {
    _ = exported;
    const declId = c.nodes[nodeId].head.func.semaDeclId;

    const node = c.nodes[nodeId];
    const func = &c.semaFuncDecls.items[declId];

    const blockId = try pushBlock(c, declId);
    try appendFuncParamVars(c, func);
    try semaStmts(c, node.head.func.bodyHead, false);

    try endFuncSymBlock(c, func.numParams);

    func.semaBlockId = blockId;
}

pub fn declareVar(c: *cy.CompileChunk, nodeId: cy.NodeId) !void {
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.varDecl.varSpec];
    const nameN = c.nodes[varSpec.head.varSpec.name];
    const name = c.getNodeTokenString(nameN);
    const nameId = try ensureNameSym(c.compiler, name);
    try c.compiler.sema.modules.items[c.modId].setUserVar(c.compiler, name, nodeId);

    // var type.
    var typeSymId: ResolvedSymId = undefined;
    if (varSpec.head.varSpec.typeSpecHead != cy.NullId) {
        typeSymId = try getOrResolveTypeSymFromSpecNode(c, varSpec.head.varSpec.typeSpecHead);
    } else {
        typeSymId = bt.Any;
    }

    const rSymId = try resolveLocalVarSym(c, c.semaResolvedRootSymId, nameId, typeSymId, nodeId, true);
    c.nodes[nodeId].head.varDecl.sema_rSymId = rSymId;
}

fn varDecl(c: *cy.CompileChunk, nodeId: cy.NodeId, exported: bool) !void {
    _ = exported;
    const node = c.nodes[nodeId];
    const varSpec = c.nodes[node.head.varDecl.varSpec];
    const nameN = c.nodes[varSpec.head.varSpec.name];
    if (nameN.node_t == .ident) {
        const name = c.getNodeTokenString(nameN);

        const rSymId = node.head.varDecl.sema_rSymId;
        const crSymId = CompactResolvedSymId.initSymId(rSymId);

        c.curSemaInitingSym = crSymId;
        c.semaVarDeclDeps.clearRetainingCapacity();
        defer c.curSemaInitingSym = CompactResolvedSymId.initNull();

        const right = c.nodes[node.head.varDecl.right];
        if (right.node_t == .matchBlock) {
            _ = try matchBlock(c, node.head.varDecl.right, true);
        } else {
            _ = semaExpr(c, node.head.varDecl.right, false) catch |err| {
                if (err == error.CanNotUseLocal) {
                    const local = c.nodes[c.compiler.errorPayload];
                    const localName = c.getNodeTokenString(local);
                    return c.reportErrorAt("The declaration of static variable `{}` can not reference the local variable `{}`.", &.{v(name), v(localName)}, nodeId);
                } else {
                    return err;
                } 
            };
        }
    } else {
        return c.reportErrorAt("Static variable declarations can only have an identifier as the name. Parsed {} instead.", &.{fmt.v(nameN.node_t)}, nodeId);
    }
}

fn semaExpr(c: *cy.CompileChunk, nodeId: cy.NodeId, comptime discardTopExprReg: bool) anyerror!Type {
    return semaExpr2(c, nodeId, types.AnyType, discardTopExprReg);
}

fn semaExpr2(c: *cy.CompileChunk, nodeId: cy.NodeId, reqType: Type, comptime discardTopExprReg: bool) anyerror!Type {
    c.curNodeId = nodeId;
    const node = c.nodes[nodeId];
    // log.debug("sema expr {}", .{node.node_t});
    switch (node.node_t) {
        .true_literal => {
            return types.BoolType;
        },
        .false_literal => {
            return types.BoolType;
        },
        .none => {
            return types.NoneType;
        },
        .arr_literal => {
            var expr_id = node.head.child_head;
            var i: u32 = 0;
            while (expr_id != cy.NullId) : (i += 1) {
                var expr = c.nodes[expr_id];
                _ = try semaExpr(c, expr_id, discardTopExprReg);
                expr_id = expr.next;
            }

            return types.ListType;
        },
        .symbolLit => {
            return types.SymbolType;
        },
        .errorSymLit => {
            return types.ErrorType;
        },
        .objectInit => {
            _ = try semaExpr(c, node.head.objectInit.name, discardTopExprReg);
            const nameN = c.nodes[node.head.objectInit.name];

            var crSymId = CompactResolvedSymId.initNull();
            if (nameN.node_t == .ident) {
                crSymId = nameN.head.ident.sema_crSymId;
            } else if (nameN.node_t == .accessExpr) {
                crSymId = nameN.head.accessExpr.sema_crSymId;
            }

            if (crSymId.isPresent()) {
                if (!crSymId.isFuncSymId) {
                    c.nodes[nodeId].head.objectInit.sema_rSymId = crSymId.id;
                    const initializer = c.nodes[node.head.objectInit.initializer];
                    var i: u32 = 0;
                    var entry_id = initializer.head.child_head;
                    while (entry_id != cy.NullId) : (i += 1) {
                        var entry = c.nodes[entry_id];
                        _ = try semaExpr(c, entry.head.mapEntry.right, discardTopExprReg);
                        entry_id = entry.next;
                    }
                    return types.initResolvedSymType(crSymId.id);
                }
            }

            const name = c.getNodeTokenString(nameN);
            return c.reportError("Object type `{}` does not exist.", &.{v(name)});
        },
        .map_literal => {
            var i: u32 = 0;
            var entry_id = node.head.child_head;
            while (entry_id != cy.NullId) : (i += 1) {
                var entry = c.nodes[entry_id];

                _ = try semaExpr(c, entry.head.mapEntry.right, discardTopExprReg);
                entry_id = entry.next;
            }
            return types.MapType;
        },
        .nonDecInt => {
            const literal = c.getNodeTokenString(node);
            var val: u64 = undefined;
            if (literal[1] == 'x') {
                val = try std.fmt.parseInt(u64, literal[2..], 16);
            } else if (literal[1] == 'o') {
                val = try std.fmt.parseInt(u64, literal[2..], 8);
            } else if (literal[1] == 'b') {
                val = try std.fmt.parseInt(u64, literal[2..], 2);
            } else if (literal[1] == 'u') {
                var start: usize = 3;
                if (literal[3] == '\\') {
                    start = 4;
                }
                const len = std.unicode.utf8ByteSequenceLength(literal[start]) catch {
                    return c.reportError("Invalid UTF-8 Rune.", &.{});
                };
                if (start == 3) {
                    if (literal.len != @as(usize, 4) + len) {
                        return c.reportError("Invalid UTF-8 Rune.", &.{});
                    }
                } else {
                    if (literal.len != @as(usize, 5) + len) {
                        return c.reportError("Invalid UTF-8 Rune.", &.{});
                    }
                }
                const cp = std.unicode.utf8Decode(literal[start..start+len]) catch {
                    return c.reportError("Invalid UTF-8 Rune.", &.{});
                };
                val = cp;
            } else {
                const char: []const u8 = &[_]u8{literal[1]};
                return c.reportError("Unsupported integer notation: {}", &.{v(char)});
            }
            c.nodes[nodeId].head = .{
                .nonDecInt = .{
                    .semaNumberVal = @intToFloat(f64, val),
                },
            };
            if (std.math.cast(i32, val) != null) {
                return types.NumberOrRequestIntegerType;
            }
            return types.NumberType;
        },
        .number => {
            const literal = c.getNodeTokenString(node);
            const val = try std.fmt.parseFloat(f64, literal);
            if (cy.Value.floatCanBeInteger(val)) {
                const int = @floatToInt(i64, val);
                if (std.math.cast(i32, int) != null) {
                    return types.NumberOrRequestIntegerType;
                }
            }
            return types.NumberType;
        },
        .string => {
            return types.StaticStringType;
        },
        .stringTemplate => {
            var expStringPart = true;
            var curId = node.head.stringTemplate.partsHead;
            while (curId != cy.NullId) {
                const cur = c.nodes[curId];
                if (!expStringPart) {
                    _ = try semaExpr(c, curId, discardTopExprReg);
                }
                curId = cur.next;
                expStringPart = !expStringPart;
            }
            return types.StringType;
        },
        .ident => {
            return identifier(c, nodeId);
        },
        .if_expr => {
            _ = try semaExpr(c, node.head.if_expr.cond, false);

            _ = try semaExpr(c, node.head.if_expr.body_expr, discardTopExprReg);

            if (node.head.if_expr.else_clause != cy.NullId) {
                const else_clause = c.nodes[node.head.if_expr.else_clause];
                _ = try semaExpr(c, else_clause.head.child_head, discardTopExprReg);
            }
            return types.AnyType;
        },
        .arr_range_expr => {
            _ = try semaExpr(c, node.head.arr_range_expr.arr, discardTopExprReg);

            if (node.head.arr_range_expr.left == cy.NullId) {
                // nop
            } else {
                _ = try semaExpr(c, node.head.arr_range_expr.left, discardTopExprReg);
            }
            if (node.head.arr_range_expr.right == cy.NullId) {
                // nop
            } else {
                _ = try semaExpr(c, node.head.arr_range_expr.right, discardTopExprReg);
            }

            return types.ListType;
        },
        .accessExpr => {
            return accessExpr(c, nodeId, discardTopExprReg);
        },
        .arr_access_expr => {
            _ = try semaExpr(c, node.head.left_right.left, discardTopExprReg);

            const index = c.nodes[node.head.left_right.right];
            if (index.node_t == .unary_expr and index.head.unary.op == .minus) {
                _ = try semaExpr(c, index.head.unary.child, discardTopExprReg);
            } else {
                _ = try semaExpr(c, node.head.left_right.right, discardTopExprReg);
            }
            return types.AnyType;
        },
        .tryExpr => {
            _ = try semaExpr(c, node.head.tryExpr.expr, discardTopExprReg);
            if (node.head.tryExpr.elseExpr != cy.NullId) {
                _ = try semaExpr(c, node.head.tryExpr.elseExpr, discardTopExprReg);
            }
            return types.AnyType;
        },
        .throwExpr => {
            _ = try semaExpr(c, node.head.child_head, discardTopExprReg);
            return types.AnyType;
        },
        .unary_expr => {
            const op = node.head.unary.op;
            switch (op) {
                .minus => {
                    _ = try semaExpr(c, node.head.unary.child, discardTopExprReg);
                    return types.NumberType;
                },
                .not => {
                    _ = try semaExpr(c, node.head.unary.child, discardTopExprReg);
                    return types.BoolType;
                },
                .bitwiseNot => {
                    _ = try semaExpr(c, node.head.unary.child, discardTopExprReg);
                    return types.NumberType;
                },
                // else => return self.reportErrorAt("Unsupported unary op: {}", .{op}, node),
            }
        },
        .group => {
            return semaExpr(c, node.head.child_head, discardTopExprReg);
        },
        .castExpr => {
            _ = try semaExpr(c, node.head.castExpr.expr, discardTopExprReg);
            const rTypeSymId = try getOrResolveTypeSymFromSpecNode(c, node.head.castExpr.typeSpecHead);
            c.nodes[nodeId].head.castExpr.semaTypeSymId = rTypeSymId;
            
            return types.initResolvedSymType(rTypeSymId);
        },
        .binExpr => {
            const left = node.head.binExpr.left;
            const right = node.head.binExpr.right;

            const op = node.head.binExpr.op;
            switch (op) {
                .star,
                .slash,
                .percent => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .caret => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .plus,
                .minus => {
                    const leftT = try semaExpr2(c, left, reqType, discardTopExprReg);
                    const rightT = try semaExpr2(c, right, reqType, discardTopExprReg);

                    if (reqType.typeT == .any) {
                        if (leftT.typeT == .int or rightT.typeT == .int) {
                            if (leftT.canBeInt() and rightT.canBeInt()) {
                                return types.IntegerType;
                            } else {
                                return c.reportError("Can not perform arithmetic between {} and {}.", &.{v(leftT.typeT), v(rightT.typeT)});
                            }
                        }
                        return types.NumberType;
                    } else {
                        if (reqType.typeT == .int or reqType.typeT == .number) {
                            return reqType;
                        } else {
                            return c.reportError("Expression can not evaluate to required type: {}", &.{v(reqType.typeT)});
                        }
                    }
                },
                .bitwiseAnd => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .bitwiseOr => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .bitwiseXor => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .bitwiseLeftShift => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .bitwiseRightShift => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.NumberType;
                },
                .and_op => {
                    const ltype = try semaExpr(c, left, discardTopExprReg);
                    const rtype = try semaExpr(c, right, discardTopExprReg);
                    if (ltype.typeT == rtype.typeT) {
                        return ltype;
                    } else return types.AnyType;
                },
                .or_op => {
                    const ltype = try semaExpr(c, left, discardTopExprReg);
                    const rtype = try semaExpr(c, right, discardTopExprReg);
                    if (ltype.typeT == rtype.typeT) {
                        return ltype;
                    } else return types.AnyType;
                },
                .bang_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.BoolType;
                },
                .equal_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.BoolType;
                },
                .less => {
                    const leftT = try semaExpr(c, left, discardTopExprReg);
                    const rightT = try semaExpr(c, right, discardTopExprReg);
            
                    const canRequestLeftInt = leftT.typeT == .int or (leftT.typeT == .number and leftT.inner.number.canRequestInteger);
                    const canRequestRightInt = rightT.typeT == .int or (rightT.typeT == .number and rightT.inner.number.canRequestInteger);
                    if (canRequestLeftInt and canRequestRightInt) {
                        c.nodes[nodeId].head.binExpr.semaCanRequestIntegerOperands = true;
                    }
                    
                    return types.BoolType;
                },
                .less_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.BoolType;
                },
                .greater => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.BoolType;
                },
                .greater_equal => {
                    _ = try semaExpr(c, left, discardTopExprReg);
                    _ = try semaExpr(c, right, discardTopExprReg);
                    return types.BoolType;
                },
                else => return c.reportErrorAt("Unsupported binary op: {}", &.{fmt.v(op)}, nodeId),
            }
        },
        .coyield => {
            return types.AnyType;
        },
        .coresume => {
            _ = try semaExpr(c, node.head.child_head, false);
            return types.AnyType;
        },
        .coinit => {
            _ = try semaExpr(c, node.head.child_head, false);
            return types.FiberType;
        },
        .callExpr => {
            const callee = c.nodes[node.head.callExpr.callee];
            if (!node.head.callExpr.has_named_arg) {
                if (callee.node_t == .accessExpr) {
                    _ = try semaExpr(c, callee.head.accessExpr.left, false);

                    const callArgStart = c.compiler.typeStack.items.len;
                    defer c.compiler.typeStack.items.len = callArgStart;

                    // Push Any in case it's a method sym. pushCallArgs will ignore the self param.
                    try c.compiler.typeStack.append(c.alloc, types.AnyType);

                    const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
                    const reqRet = try types.typeFromResolvedSym(c, bt.Any);

                    const left = c.nodes[callee.head.accessExpr.left];
                    var crLeftSym = CompactResolvedSymId.initNull();
                    if (left.node_t == .ident) {
                        crLeftSym = left.head.ident.sema_crSymId;
                    } else if (left.node_t == .accessExpr) {
                        crLeftSym = left.head.accessExpr.sema_crSymId;
                    }
                    if (crLeftSym.isPresent()) {
                        // Calling a symbol.
                        if (!crLeftSym.isFuncSymId) {
                            const right = c.nodes[callee.head.accessExpr.right];
                            const name = c.getNodeTokenString(right);
                            const nameId = try ensureNameSym(c.compiler, name);

                            if (try getOrResolveSymForFuncCall(c, crLeftSym.id, nameId, callArgs, reqRet)) |callRes| {
                                try referenceSym(c, callRes.crSymId, true);
                                c.nodes[node.head.callExpr.callee].head.accessExpr.sema_crSymId = callRes.crSymId;
                                return callRes.retType;
                            }
                        }
                    }

                    // Dynamic method call.
                    const params = c.compiler.typeStack.items[callArgStart..];
                    const rFuncSigId = try ensureResolvedFuncSigTypes(c.compiler, params, reqRet);
                    c.nodes[callee.head.accessExpr.right].head.ident.semaMethodSigId = rFuncSigId;

                    return types.AnyType;
                } else if (callee.node_t == .ident) {
                    const name = c.getNodeTokenString(callee);
                    const res = try getOrLookupVar(c, name, .readSkipStaticVar);
                    if (res.isLocal) {
                        c.nodes[node.head.callExpr.callee].head.ident.semaVarId = res.id;

                        var numArgs: u32 = 1;
                        var arg_id = node.head.callExpr.arg_head;
                        while (arg_id != cy.NullId) : (numArgs += 1) {
                            const arg = c.nodes[arg_id];
                            _ = try semaExpr(c, arg_id, false);
                            arg_id = arg.next;
                        }
                        return types.AnyType;
                    } else {
                        const callArgStart = c.compiler.typeStack.items.len;
                        defer c.compiler.typeStack.items.len = callArgStart;
                        const callArgs = try pushCallArgs(c, node.head.callExpr.arg_head);
                        const reqRet = try types.typeFromResolvedSym(c, bt.Any);

                        const nameId = try ensureNameSym(c.compiler, name);
                        c.curNodeId = node.head.callExpr.callee;
                        if (try getOrResolveSymForFuncCall(c, c.semaResolvedRootSymId, nameId, callArgs, reqRet)) |callRes| {
                            try referenceSym(c, callRes.crSymId, true);
                            c.nodes[node.head.callExpr.callee].head.ident.sema_crSymId = callRes.crSymId;
                            return callRes.retType;
                        }

                        return types.AnyType;
                    }
                } else {
                    // All other callees are treated as function value calls.
                    var numArgs: u32 = 0;
                    var arg_id = node.head.callExpr.arg_head;
                    while (arg_id != cy.NullId) : (numArgs += 1) {
                        const arg = c.nodes[arg_id];
                        _ = try semaExpr(c, arg_id, false);
                        arg_id = arg.next;
                    }

                    _ = try semaExpr(c, node.head.callExpr.callee, false);
                    return types.AnyType;
                }
            } else return c.reportErrorAt("Unsupported named args", &.{}, nodeId);
        },
        .lambda_multi => {
            if (!discardTopExprReg) {
                const declId = try appendFuncDecl(c, nodeId, false);
                c.nodes[nodeId].head.func.semaDeclId = declId;

                const blockId = try pushBlock(c, declId);

                // Generate function body.
                const func = &c.semaFuncDecls.items[declId];
                func.semaBlockId = blockId;
                try appendFuncParamVars(c, func);
                try semaStmts(c, node.head.func.bodyHead, false);

                try endFuncBlock(c, func.numParams);

                const rFuncSigId = try ensureResolvedUntypedFuncSig(c.compiler, func.numParams);
                func.inner.lambda.rFuncSigId = rFuncSigId;
            }
            return types.AnyType;
        },
        .lambda_expr => {
            if (!discardTopExprReg) {
                const declId = try appendFuncDecl(c, nodeId, false);
                c.nodes[nodeId].head.func.semaDeclId = declId;

                const blockId = try pushBlock(c, declId);

                // Generate function body.
                const func = &c.semaFuncDecls.items[declId];
                func.semaBlockId = blockId;
                try appendFuncParamVars(c, func);
                _ = try semaExpr(c, node.head.func.bodyHead, false);

                try endFuncBlock(c, func.numParams);

                const rFuncSigId = try ensureResolvedUntypedFuncSig(c.compiler, func.numParams);
                func.inner.lambda.rFuncSigId = rFuncSigId;
            }
            return types.AnyType;
        },
        else => return c.reportErrorAt("Unsupported node", &.{}, nodeId),
    }
}

fn identifier(c: *cy.CompileChunk, nodeId: cy.NodeId) !Type {
    const node = c.nodes[nodeId];
    const name = c.getNodeTokenString(node);
    const res = try getOrLookupVar(c, name, .read);
    if (res.isLocal) {
        c.nodes[nodeId].head.ident.semaVarId = res.id;
        return c.vars.items[res.id].vtype;
    } else {
        const nameId = try ensureNameSym(c.compiler, name);

        const symRes = try mustGetOrResolveDistinctSym(c, c.semaResolvedRootSymId, nameId);
        const crSymId = symRes.toCompactId();
        try referenceSym(c, crSymId, true);
        c.nodes[nodeId].head.ident.sema_crSymId = crSymId;

        const rSym = c.compiler.sema.getResolvedSym(symRes.rSymId);
        switch (rSym.symT) {
            .builtinType => {
                return types.MetaTypeType;
            },
            else => {
                return types.AnyType;
            },
        }
    }
}

/// Recursively walks a type spec head node and returns the final resolved type sym.
/// Assumes head is non null.
fn getOrResolveTypeSymFromSpecNode(chunk: *cy.CompileChunk, head: cy.NodeId) !ResolvedSymId {
    var nodeId = head;
    var rParentSymId = chunk.semaResolvedRootSymId;
    // log.debug("getOrResolveTypeSymFromSpecNode from {} ", .{rParentSymId});
    while (true) {
        const node = chunk.nodes[nodeId];
        const name = chunk.getNodeTokenString(node);
        const nameId = try ensureNameSym(chunk.compiler, name);
        // log.debug("looking for {s}", .{name});

        if (node.next == cy.NullId) {
            chunk.curNodeId = nodeId;
            return getOrResolveTypeSym(chunk, rParentSymId, nameId);
        } else {
            const res = try mustGetOrResolveDistinctSym(chunk, rParentSymId, nameId);
            rParentSymId = res.rSymId;
        }
        nodeId = node.next;
    }
}

fn appendFuncDecl(chunk: *cy.CompileChunk, nodeId: cy.NodeId, isStatic: bool) !FuncDeclId {
    const func = chunk.nodes[nodeId];
    const header = chunk.nodes[func.head.func.header];

    var decl = FuncDecl{
        .nodeId = nodeId,
        .paramHead = header.head.funcHeader.paramHead,
        .rRetTypeSymId = cy.NullId,
        .inner = undefined,
        .isStatic = isStatic,
        .numParams = undefined,
        .rFuncSigId = undefined,
        .rSymId = undefined,
    };
    if (isStatic) {
        decl.inner = .{
            .lambda = .{},
        };
    } else {
        decl.inner = .{
            .staticFunc = .{},
        };
    }

    // Get params, build func signature.
    chunk.compiler.tempSyms.clearRetainingCapacity();
    var curParamId = header.head.funcHeader.paramHead;
    var numParams: u8 = 0;
    while (curParamId != cy.NullId) {
        const param = chunk.nodes[curParamId];
        if (param.head.funcParam.typeSpecHead == cy.NullId) {
            try chunk.compiler.tempSyms.append(chunk.alloc, bt.Any);
        } else {
            const rTypeSymId = try getOrResolveTypeSymFromSpecNode(chunk, param.head.funcParam.typeSpecHead);
            try chunk.compiler.tempSyms.append(chunk.alloc, rTypeSymId);
        }
        numParams += 1;
        curParamId = param.next;
    }
    decl.numParams = numParams;

    // Get return type.
    var retSymId: ResolvedSymId = undefined;
    if (header.head.funcHeader.ret != cy.NullId) {
        retSymId = try getOrResolveTypeSymFromSpecNode(chunk, header.head.funcHeader.ret);
    } else {
        retSymId = bt.Any;
    }

    // Resolve func signature.
    decl.rFuncSigId = try ensureResolvedFuncSig(chunk.compiler, chunk.compiler.tempSyms.items, retSymId);
    decl.rRetTypeSymId = retSymId;

    const declId = @intCast(u32, chunk.semaFuncDecls.items.len);
    try chunk.semaFuncDecls.append(chunk.alloc, decl);
    return declId;
}

pub fn pushBlock(self: *cy.CompileChunk, funcDeclId: u32) !BlockId {
    self.curSemaBlockId = @intCast(u32, self.semaBlocks.items.len);
    const nextSubBlockId = @intCast(u32, self.semaSubBlocks.items.len);
    var isStaticFuncBlock = false;
    if (funcDeclId != cy.NullId) {
        isStaticFuncBlock = self.semaFuncDecls.items[funcDeclId].isStatic;
    }
    try self.semaBlocks.append(self.alloc, Block.init(funcDeclId, nextSubBlockId, isStaticFuncBlock));
    try self.semaBlockStack.append(self.alloc, self.curSemaBlockId);
    try pushSubBlock(self);
    return self.curSemaBlockId;
}

fn pushSubBlock(self: *cy.CompileChunk) !void {
    curBlock(self).subBlockDepth += 1;
    const prev = self.curSemaSubBlockId;
    self.curSemaSubBlockId = @intCast(u32, self.semaSubBlocks.items.len);
    try self.semaSubBlocks.append(self.alloc, SubBlock.init(prev, self.assignedVarStack.items.len));
}

fn pushMethodParamVars(c: *cy.CompileChunk, func: *const FuncDecl) !void {
    const curNodeId = c.curNodeId;
    defer c.curNodeId = curNodeId;

    const sblock = curBlock(c);

    if (func.numParams > 1) {
        const rFuncSig = c.compiler.sema.resolvedFuncSigs.items[func.rFuncSigId];
        const params = rFuncSig.params()[1..];

        // Skip the first param node.
        var curNode = func.paramHead;
        curNode = c.nodes[curNode].next;

        for (params) |rParamSymId| {
            const param = c.nodes[curNode];
            const name = c.getNodeTokenString(c.nodes[param.head.funcParam.name]);

            const paramT = try types.typeFromResolvedSym(c, rParamSymId);

            c.curNodeId = curNode;
            const id = try pushLocalVar(c, name, paramT);
            try sblock.params.append(c.alloc, id);

            curNode = param.next;
        }
    }

    // Add self receiver param.
    var id = try pushLocalVar(c, "self", types.AnyType);
    try sblock.params.append(c.alloc, id);
}

fn appendFuncParamVars(chunk: *cy.CompileChunk, func: *const FuncDecl) !void {
    const sblock = curBlock(chunk);

    if (func.numParams > 0) {
        const rFuncSig = chunk.compiler.sema.resolvedFuncSigs.items[func.rFuncSigId];
        const params = rFuncSig.params();
        var curNode = func.paramHead;
        for (params) |rParamSymId| {
            const param = chunk.nodes[curNode];
            const name = chunk.getNodeTokenString(chunk.nodes[param.head.funcParam.name]);

            const paramT = try types.typeFromResolvedSym(chunk, rParamSymId);
            const id = try pushLocalVar(chunk, name, paramT);
            try sblock.params.append(chunk.alloc, id);

            curNode = param.next;
        }
    }
}

fn pushLocalVar(c: *cy.CompileChunk, name: []const u8, vtype: Type) !LocalVarId {
    const sblock = curBlock(c);
    const id = @intCast(u32, c.vars.items.len);
    const res = try sblock.nameToVar.getOrPut(c.alloc, name);
    if (res.found_existing) {
        return c.reportError("Var `{}` already exists", &.{v(name)});
    } else {
        res.value_ptr.* = id;
        try c.vars.append(c.alloc, .{
            .name = if (builtin.mode == .Debug) name else {},
            .vtype = toLocalType(vtype),
            .lifetimeRcCandidate = vtype.rcCandidate,
        });
        return id;
    }
}

fn getVarPtr(self: *cy.CompileChunk, name: []const u8) ?*LocalVar {
    if (curBlock(self).nameToVar.get(name)) |varId| {
        return &self.vars.items[varId];
    } else return null;
}

fn pushStaticVarAlias(c: *cy.CompileChunk, name: []const u8, crSymId: CompactResolvedSymId) !LocalVarId {
    const id = try pushLocalVar(c, name, types.AnyType);
    c.vars.items[id].isStaticAlias = true;
    c.vars.items[id].inner.staticAlias = .{
        .crSymId = crSymId,
    };
    return id;
}

fn pushCapturedVar(self: *cy.CompileChunk, name: []const u8, parentVarId: LocalVarId, vtype: Type) !LocalVarId {
    const id = try pushLocalVar(self, name, vtype);
    self.vars.items[id].isCaptured = true;
    self.vars.items[id].isBoxed = true;
    try self.capVarDescs.put(self.alloc, id, .{
        .user = parentVarId,
    });
    try curBlock(self).params.append(self.alloc, id);
    return id;
}

fn pushLocalBodyVar(self: *cy.CompileChunk, name: []const u8, vtype: Type) !LocalVarId {
    const id = try pushLocalVar(self, name, vtype);
    try curBlock(self).locals.append(self.alloc, id);
    return id;
}

fn ensureLocalBodyVar(self: *cy.CompileChunk, ident: cy.NodeId, vtype: Type) !LocalVarId {
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);
    if (curBlock(self).nameToVar.get(name)) |varId| {
        self.nodes[ident].head.ident.semaVarId = varId;
        return varId;
    } else {
        const id = try pushLocalBodyVar(self, name, vtype);
        self.nodes[ident].head.ident.semaVarId = id;
        return id;
    }
}

fn referenceSym(c: *cy.CompileChunk, rSymId: CompactResolvedSymId, trackDep: bool) !void {
    if (trackDep) {
        if (c.isInStaticInitializer()) {
            // Record this symbol as a dependency.
            const res = try c.semaInitializerSyms.getOrPut(c.alloc, c.curSemaInitingSym);
            if (res.found_existing) {
                const depRes = try c.semaVarDeclDeps.getOrPut(c.alloc, rSymId);
                if (!depRes.found_existing) {
                    try c.bufU32.append(c.alloc, @bitCast(u32, rSymId));
                    res.value_ptr.*.depsEnd = @intCast(u32, c.bufU32.items.len);
                    depRes.value_ptr.* = {};
                }
            } else {
                const start = @intCast(u32, c.bufU32.items.len);
                try c.bufU32.append(c.alloc, @bitCast(u32, rSymId));
                res.value_ptr.* = .{
                    .depsStart = start,
                    .depsEnd = @intCast(u32, c.bufU32.items.len),
                };
            }
        }
    }
}

const VarLookupStrategy = enum {
    /// Look upwards for a parent local. If no such local exists, assume a static var.
    read,
    /// Same as read but does not try to find a static var symbol.
    readSkipStaticVar,
    /// Assume a static var.
    staticAssign,
    /// Look upwards for a parent local. If no such local exists, a compile error is returned.
    captureAssign,
    /// If missing in the current block, a new local is created.
    assign,
};

const VarLookupResult = struct {
    /// If `isLocal` is true, id is a LocalVarId.
    id: u32,
    /// If `isLocal` is false, id is a CompactResolvedSymId.
    isLocal: bool,
    /// Whether the local var was created.
    created: bool,
};

fn getOrLookupVar(self: *cy.CompileChunk, name: []const u8, strat: VarLookupStrategy) !VarLookupResult {
    const sblock = curBlock(self);
    if (sblock.nameToVar.get(name)) |varId| {
        const svar = self.vars.items[varId];
        switch (strat) {
            .readSkipStaticVar,
            .read => {
                if (!svar.isStaticAlias) {
                    // Can not reference local var in a static var decl unless it's in a nested block.
                    // eg. a = 0
                    //     var b = a
                    if (self.isInStaticInitializer() and self.semaBlockDepth() == 1) {
                        self.compiler.errorPayload = self.curNodeId;
                        return error.CanNotUseLocal;
                    }
                    return VarLookupResult{
                        .id = varId,
                        .isLocal = true,
                        .created = false,
                    };
                } else {
                    return VarLookupResult{
                        .id = @bitCast(u32, svar.inner.staticAlias.crSymId),
                        .isLocal = false,
                        .created = false,
                    };
                }
            },
            .assign => {
                if (svar.isStaticAlias) {
                    // Assumes static variables can only exist in the main block.
                    if (svar.hasCaptureOrStaticModifier or self.semaBlockDepth() == 1) {
                        return VarLookupResult{
                            .id = @bitCast(u32, svar.inner.staticAlias.crSymId),
                            .isLocal = false,
                            .created = false,
                        };
                    } else {
                        return self.reportError("`{}` already references a static variable. The variable must be declared with `static` before assigning to it.", &.{v(name)});
                    }
                } else if (svar.isCaptured) {
                    if (svar.hasCaptureOrStaticModifier) {
                        return VarLookupResult{
                            .id = varId,
                            .isLocal = true,
                            .created = false,
                        };
                    } else {
                        return self.reportError("`{}` already references a captured variable. The variable must be declared with `capture` before assigning to it.", &.{v(name)});
                    }
                } else {
                    return VarLookupResult{
                        .id = varId,
                        .isLocal = true,
                        .created = false,
                    };
                }
            },
            .captureAssign => {
                if (!svar.isCaptured) {
                    // Previously not captured, update to captured.
                    return self.reportError("TODO: update to captured variable", &.{});
                } else {
                    return VarLookupResult{
                        .id = varId,
                        .isLocal = true,
                        .created = false,
                    };
                }
            },
            .staticAssign => {
                if (!svar.isStaticAlias) {
                    // Previously not static alias, update to static alias.
                    return self.reportError("TODO: update to static alias", &.{});
                } else {
                    return VarLookupResult{
                        .id = cy.NullId,
                        .isLocal = false,
                        .created = false,
                    };
                }
            },
            // When typed declaration is implemented, that can create a new local if the variable was previously implicity captured.
            // // Create a new local var and update mapping so any references after will refer to the local var.
            // const sblock = curBlock(self);
            // _ = sblock.nameToVar.remove(name);
            // const id = try pushLocalBodyVar(self, name, vtype);
            // if (sblock.subBlockDepth > 1) {
            //     self.vars.items[id].genInitializer = true;
            // }
        }
    }

    // Perform lookup based on the strategy. See `VarLookupStrategy`.
    switch (strat) {
        .readSkipStaticVar,
        .read => {
            if (lookupParentLocal(self, name)) |res| {
                if (self.isInStaticInitializer()) {
                    // Can not capture local before this block.
                    if (res.blockDepth == 1) {
                        self.compiler.errorPayload = self.curNodeId;
                        return error.CanNotUseLocal;
                    }
                } else if (sblock.isStaticFuncBlock) {
                    // Can not capture local before static function block.
                    const func = self.semaFuncDecls.items[sblock.funcDeclId];
                    const funcName = func.getName(self);
                    return self.reportErrorAt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (function values) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
                }

                // Create a local captured variable.
                const parentVar = self.vars.items[res.varId];
                const id = try pushCapturedVar(self, name, res.varId, parentVar.vtype);
                return VarLookupResult{
                    .id = id,
                    .isLocal = true,
                    .created = true,
                };
            } else {
                if (strat == .read) {
                    const nameId = try ensureNameSym(self.compiler, name);
                    const res = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
                    _ = try pushStaticVarAlias(self, name, res.toCompactId());
                    return VarLookupResult{
                        .id = @bitCast(u32, res.toCompactId()),
                        .isLocal = false,
                        .created = false,
                    };
                } else {
                    return VarLookupResult{
                        .id = cy.NullId,
                        .isLocal = false,
                        .created = false,
                    };
                }
            }
        },
        .staticAssign => {
            const nameId = try ensureNameSym(self.compiler, name);
            const res = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
            const id = try pushStaticVarAlias(self, name, res.toCompactId());
            self.vars.items[id].hasCaptureOrStaticModifier = true;
            return VarLookupResult{
                .id = @bitCast(u32, res.toCompactId()),
                .isLocal = false,
                .created = true,
            };
        },
        .captureAssign => {
            if (lookupParentLocal(self, name)) |res| {
                if (self.isInStaticInitializer()) {
                    if (res.blockDepth == 1) {
                        return self.reportError("Can not use local in static variable initializer.", &.{});
                    }
                } else if (sblock.isStaticFuncBlock) {
                    // Can not capture local before static function block.
                    const func = self.semaFuncDecls.items[sblock.funcDeclId];
                    const funcName = func.getName(self);
                    return self.reportErrorAt("Can not capture the local variable `{}` from static function `{}`.\nOnly lambdas (function values) can capture local variables.", &.{v(name), v(funcName)}, self.curNodeId);
                }
                // Create a local captured variable.
                const parentVar = self.vars.items[res.varId];
                const id = try pushCapturedVar(self, name, res.varId, parentVar.vtype);
                self.vars.items[id].hasCaptureOrStaticModifier = true;
                return VarLookupResult{
                    .id = id,
                    .isLocal = true,
                    .created = true,
                };
            } else {
                return self.reportError("Could not find a parent local named `{}`.", &.{v(name)});
            }
        },
        .assign => {
            // Prefer static variable in the same block.
            // For now, only do this for main block.
            if (self.semaBlockDepth() == 1) {
                const nameId = try ensureNameSym(self.compiler, name);
                if (hasResolvedSym(self, self.semaResolvedRootSymId, nameId)) {
                    return VarLookupResult{
                        .id = cy.NullId,
                        .isLocal = false,
                        .created = false,
                    };
                }
            }
            const id = try pushLocalBodyVar(self, name, types.UndefinedType);
            if (sblock.subBlockDepth > 1) {
                self.vars.items[id].genInitializer = true;
            }
            return VarLookupResult{
                .id = id,
                .isLocal = true,
                .created = true,
            };
        },
    }
}

const LookupParentLocalResult = struct {
    varId: LocalVarId,

    // Main block starts at 1.
    blockDepth: u32,
};

fn lookupParentLocal(c: *cy.CompileChunk, name: []const u8) ?LookupParentLocalResult {
    // Only check one block above.
    if (c.semaBlockDepth() > 1) {
        const prevId = c.semaBlockStack.items[c.semaBlockDepth() - 1];
        const prev = c.semaBlocks.items[prevId];
        if (prev.nameToVar.get(name)) |varId| {
            if (!c.vars.items[varId].isStaticAlias) {
                return .{
                    .varId = varId,
                    .blockDepth = c.semaBlockDepth(),
                };
            }
        }
    }
    return null;
}

fn getTypeForResolvedValueSym(chunk: *cy.CompileChunk, crSymId: CompactResolvedSymId) !Type {
    if (crSymId.isFuncSymId) {
        return types.AnyType;
    } else {
        const rSym = chunk.compiler.sema.getResolvedSym(crSymId.id);
        switch (rSym.symT) {
            .variable => {
                return types.typeFromResolvedSym(chunk, rSym.inner.variable.rTypeSymId);
            },
            .enumMember => {
                const enumId = rSym.inner.enumMember.enumId;
                return types.initEnumType(enumId);
            },
            else => {
                return types.AnyType;
            },
        }
    }
}

pub fn addResolvedBuiltinSym(c: *cy.VMcompiler, typeT: types.TypeTag, name: []const u8) !ResolvedSymId {
    const nameId = try ensureNameSym(c, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = cy.NullId,
            .nameId = nameId,
        },
    };

    const modId = try appendSubModule(c, name);
    const mod = c.sema.getModulePtr(modId);

    const id = @intCast(u32, c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .key = key,
        .symT = .builtinType,
        .inner = .{
            .builtinType = .{
                .modId = modId,
                .typeT = @enumToInt(typeT),
            },
        },
        .exported = true,
    });
    try c.sema.resolvedSymMap.put(c.alloc, key, id);
    mod.resolvedRootSymId = id;
    return id;
}

fn addResolvedUntypedFuncSig(c: *cy.VMcompiler, numParams: u32) !ResolvedFuncSigId {
    // AnyType for params and return.
    try c.tempTypes.resize(c.alloc, numParams);
    for (c.tempTypes.items) |*stype| {
        stype.* = types.AnyType;
    }
    return ensureResolvedFuncSigTypes(c, c.tempTypes.items, types.AnyType);
}

pub fn ensureResolvedUntypedFuncSig(c: *cy.VMcompiler, numParams: u32) !ResolvedFuncSigId {
    if (numParams < c.sema.resolvedUntypedFuncSigs.items.len) {
        var rFuncSigId = c.sema.resolvedUntypedFuncSigs.items[numParams];
        if (rFuncSigId == cy.NullId) {
            rFuncSigId = try addResolvedUntypedFuncSig(c, numParams);
            c.sema.resolvedUntypedFuncSigs.items[numParams] = rFuncSigId;
        }
        return rFuncSigId;
    }
    const end = c.sema.resolvedUntypedFuncSigs.items.len;
    try c.sema.resolvedUntypedFuncSigs.resize(c.alloc, numParams + 1);
    for (end..c.sema.resolvedUntypedFuncSigs.items.len) |i| {
        c.sema.resolvedUntypedFuncSigs.items[i] = cy.NullId;
    }
    const rFuncSigId = try addResolvedUntypedFuncSig(c, numParams);
    c.sema.resolvedUntypedFuncSigs.items[numParams] = rFuncSigId;
    return rFuncSigId;
}

fn ensureResolvedFuncSigTypes(c: *cy.VMcompiler, params: []const Type, ret: Type) !ResolvedFuncSigId {
    try c.tempSyms.resize(c.alloc, params.len);
    for (params, 0..) |param, i| {
        const rSymId = types.typeToResolvedSym(param);
        c.tempSyms.items[i] = rSymId;
    }
    const retSymId = types.typeToResolvedSym(ret);
    return ensureResolvedFuncSig(c, c.tempSyms.items, retSymId);
}

pub fn ensureResolvedFuncSig(c: *cy.VMcompiler, params: []const ResolvedSymId, ret: ResolvedSymId) !ResolvedFuncSigId {
    const res = try c.sema.resolvedFuncSigMap.getOrPut(c.alloc, .{
        .paramPtr = params.ptr,
        .paramLen = @intCast(u32, params.len),
        .retSymId = ret,
    });
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id = @intCast(u32, c.sema.resolvedFuncSigs.items.len);
        const new = try c.alloc.dupe(ResolvedSymId, params);
        var isTyped = false;
        for (params) |rSymId| {
            const rSym = c.sema.getResolvedSym(rSymId);
            if (rSym.symT != .builtinType or rSym.inner.builtinType.typeT != @enumToInt(types.TypeTag.any)) {
                isTyped = true;
                break;
            }
        }
        const rRetSym = c.sema.getResolvedSym(ret);
        if (rRetSym.symT != .builtinType or rRetSym.inner.builtinType.typeT != @enumToInt(types.TypeTag.any)) {
            isTyped = true;
        }
        try c.sema.resolvedFuncSigs.append(c.alloc, .{
            .paramPtr = new.ptr,
            .paramLen = @intCast(u16, new.len),
            .retSymId = ret,
            .isTyped = isTyped,
        });
        res.value_ptr.* = id;
        res.key_ptr.* = .{
            .paramPtr = new.ptr,
            .paramLen = @intCast(u32, new.len),
            .retSymId = ret,
        };
        return id;
    }
}

/// Format: (Type, ...) RetType
pub fn getResolvedFuncSigTempStr(c: *cy.VMcompiler, rFuncSigId: ResolvedFuncSigId) ![]const u8 {
    c.vm.u8Buf.clearRetainingCapacity();
    const w = c.vm.u8Buf.writer(c.alloc);
    try writeResolvedFuncSigStr(c, w, rFuncSigId);
    return c.vm.u8Buf.items();
}
pub fn allocResolvedFuncSigStr(c: *cy.VMcompiler, rFuncSigId: ResolvedFuncSigId) ![]const u8 {
    var buf: std.ArrayListUnmanaged(u8) = .{};
    defer buf.deinit(c.alloc);

    const w = buf.writer(c.alloc);
    try writeResolvedFuncSigStr(c, w, rFuncSigId);
    return buf.toOwnedSlice(c.alloc);
}

pub fn writeResolvedFuncSigStr(c: *cy.VMcompiler, w: anytype, rFuncSigId: ResolvedFuncSigId) !void {
    const rFuncSig = c.sema.resolvedFuncSigs.items[rFuncSigId];
    try w.writeAll("(");

    if (rFuncSig.numParams() > 0) {
        var rParamSym = c.sema.getResolvedSym(rFuncSig.paramPtr[0]);
        var name = getName(c, rParamSym.key.absResolvedSymKey.nameId);
        try w.writeAll(name);

        if (rFuncSig.numParams() > 1) {
            for (rFuncSig.params()[1..]) |rParamSymId| {
                try w.writeAll(", ");
                rParamSym = c.sema.resolvedSyms.items[rParamSymId];
                name = getName(c, rParamSym.key.absResolvedSymKey.nameId);
                try w.writeAll(name);
            }
        }
    }
    try w.writeAll(") ");

    var rRetSym = c.sema.resolvedSyms.items[rFuncSig.retSymId];
    var name = getName(c, rRetSym.key.absResolvedSymKey.nameId);
    try w.writeAll(name);
}

pub const CompactResolvedSymId = packed struct {
    id: u31,
    isFuncSymId: bool,

    pub fn initNull() CompactResolvedSymId {
        return @bitCast(CompactResolvedSymId, @as(u32, cy.NullId));
    }

    pub fn initSymId(id: ResolvedSymId) CompactResolvedSymId {
        return .{
            .id = @intCast(u31, id),
            .isFuncSymId = false,
        };
    }

    pub fn initFuncSymId(id: ResolvedFuncSymId) CompactResolvedSymId {
        return .{
            .id = @intCast(u31, id),
            .isFuncSymId = true,
        };
    }

    fn isNull(self: CompactResolvedSymId) bool {
        return @bitCast(u32, self) == cy.NullId;
    }

    pub fn isPresent(self: CompactResolvedSymId) bool {
        return @bitCast(u32, self) != cy.NullId;
    }
};

fn findDistinctModuleSym(chunk: *cy.CompileChunk, modId: ModuleId, nameId: NameSymId) !bool {
    const relKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
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
            .nativeFunc1 => {
                return true;
            },
            .symToManyFuncs => {
                // More than one func for sym.
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)});
            },
        }
    }
    return false;
}

/// Finds the first function that matches the constrained signature.
fn findModuleSymForFuncCall(chunk: *cy.CompileChunk, modId: ModuleId, nameId: NameSymId, args: []const Type, ret: Type) !?ResolvedFuncSigId {
    const relKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };

    const mod = chunk.compiler.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        switch (modSym.symT) {
            .symToManyFuncs => {
                var optNode: ?*ModuleFuncNode = modSym.inner.symToManyFuncs.head;
                while (optNode) |node| {
                    if (types.isTypeFuncSigCompat(chunk.compiler, args, ret, node.rFuncSigId)) {
                        return node.rFuncSigId;
                    }
                    optNode = node.next;
                }
            },
            .symToOneFunc => {
                const rFuncSigId = modSym.inner.symToOneFunc.rFuncSigId;
                if (types.isTypeFuncSigCompat(chunk.compiler, args, ret, rFuncSigId)) {
                    return rFuncSigId;
                }
            },
            else => {
            },
        }
    }
    return null;
}

const ResolvedSymResult = struct {
    rSymId: ResolvedSymId,
    rFuncSymId: Nullable(ResolvedFuncSymId),

    fn toCompactId(self: ResolvedSymResult) CompactResolvedSymId {
        if (self.rFuncSymId != cy.NullId) {
            return CompactResolvedSymId.initFuncSymId(self.rFuncSymId);
        } else {
            return CompactResolvedSymId.initSymId(self.rSymId);
        }
    }
};

fn checkTypeSym(c: *cy.CompileChunk, rSymId: ResolvedSymId, nameId: NameSymId) !void {
    const rSym = c.compiler.sema.getResolvedSym(rSymId);
    if (rSym.symT == .object) {
        return;
    } else if (rSym.symT == .builtinType) {
        return;
    } else {
        const name = getName(c.compiler, nameId);
        return c.reportError("`{}` is not a type symbol.", &.{v(name)});
    }
}

fn getOrResolveTypeSym(chunk: *cy.CompileChunk, rParentSymId: ResolvedSymId, nameId: NameSymId) !ResolvedSymId {
    // Check builtin types.
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        if (nameId < types.BuiltinTypeTags.len) {
            return @intCast(ResolvedSymId, nameId);
        }
    }

    var key: vm_.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        // Faster check against local syms.
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            if (sym.rSymId != cy.NullId) {
                try checkTypeSym(chunk, sym.rSymId, nameId);
                return sym.rSymId;
            } else {
                // Unresolved.
                const res = try resolveDistinctLocalSym(chunk, key);
                try checkTypeSym(chunk, res.rSymId, nameId);
                return res.rSymId;
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };

    var rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (rSymId != cy.NullId) {
        try checkTypeSym(chunk, rSymId, nameId);
        return rSymId;
    }

    const parentSym = chunk.compiler.sema.resolvedSyms.items[rParentSymId];
    if (parentSym.symT == .module) {
        const modId = parentSym.inner.module.id;
        if (try resolveTypeSymFromModule(chunk, modId, nameId)) |resSymId| {
            return resSymId;
        }
    }
    const name = getName(chunk.compiler, nameId);
    return chunk.reportError("Could not find type symbol `{}`.", &.{v(name)});
}

fn mustGetOrResolveDistinctSym(chunk: *cy.CompileChunk, rParentSymId: ResolvedSymId, nameId: NameSymId) !ResolvedSymResult {
    return try getOrResolveDistinctSym(chunk, rParentSymId, nameId) orelse {
        const name = getName(chunk.compiler, nameId);
        return chunk.reportError("Can not find the symbol `{}`.", &.{v(name)});
    };
}

fn resolveDistinctLocalSym(chunk: *cy.CompileChunk, lkey: RelLocalSymKey) !ResolvedSymResult {
    const sym = chunk.localSyms.getPtr(lkey).?;

    // First check resolved syms.
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = sym.rParentSymId,
            .nameId = lkey.relLocalSymKey.nameId,
        },
    };
    if (chunk.compiler.sema.resolvedSymMap.get(key)) |rSymId| {
        const rSym = chunk.compiler.sema.getResolvedSym(rSymId);
        if (rSym.symT == .func) {
            if (rSym.inner.func.rFuncSymId == cy.NullId) {
                const name = getName(chunk.compiler, lkey.relLocalSymKey.nameId);
                return chunk.reportError("Can not disambiguate the symbol `{}`.", &.{v(name)});
            } else {
                sym.rSymId = rSymId;
                sym.rFuncSymId = rSym.inner.func.rFuncSymId;
                return ResolvedSymResult{
                    .rSymId = sym.rSymId,
                    .rFuncSymId = sym.rFuncSymId,
                };
            }
        } else {
            sym.rSymId = rSymId;
            sym.rFuncSymId = cy.NullId;
        }
    } else {
        const rParentSym = chunk.compiler.sema.getResolvedSym(sym.rParentSymId);
        if (rParentSym.symT == .module) {
            const crSymId = (try resolveSymFromModule(chunk, rParentSym.inner.module.id, lkey.relLocalSymKey.nameId, cy.NullId)).?;
            if (crSymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                sym.rSymId = rFuncSym.key.absResolvedFuncSymKey.rSymId;
                sym.rFuncSymId = crSymId.id;
            } else {
                sym.rSymId = crSymId.id;
                sym.rFuncSymId = cy.NullId;
            }
        } else {
            stdx.fatal();
        }
    }
    return ResolvedSymResult{
        .rSymId = sym.rSymId,
        .rFuncSymId = sym.rFuncSymId,
    };
}

/// TODO: This should perform type checking.
fn getOrResolveDistinctSym(chunk: *cy.CompileChunk, rParentSymId: ResolvedSymId, nameId: NameSymId) !?ResolvedSymResult {
    log.debug("getDistinctSym {}.{s}", .{rParentSymId, getName(chunk.compiler, nameId)} );
    var key: vm_.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        // Faster check against local syms.
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            if (sym.rSymId != cy.NullId) {
                return ResolvedSymResult{
                    .rSymId = sym.rSymId,
                    .rFuncSymId = sym.rFuncSymId,
                };
            } else {
                // Unresolved.
                const res = try resolveDistinctLocalSym(chunk, key);
                return ResolvedSymResult{
                    .rSymId = res.rSymId,
                    .rFuncSymId = res.rFuncSymId,
                };
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };

    var rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (rSymId != cy.NullId) {
        const rSym = chunk.compiler.sema.resolvedSyms.items[rSymId];
        if (rSym.symT == .func) {
            if (rSym.inner.func.rFuncSymId == cy.NullId) {
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Can not disambiguate the symbol `{}`.", &.{v(name)});
            } else {
                return ResolvedSymResult{
                    .rSymId = rSymId,
                    .rFuncSymId = rSym.inner.func.rFuncSymId,
                };
            }
        } else {
            return ResolvedSymResult{
                .rSymId = rSymId,
                .rFuncSymId = cy.NullId,
            };
        }
    }
            
    const parentSym = chunk.compiler.sema.getResolvedSym(rParentSymId);
    if (parentSym.getModuleId()) |modId| {
        if (try findDistinctModuleSym(chunk, modId, nameId)) {
            const crSymId = (try resolveSymFromModule(chunk, modId, nameId, cy.NullId)).?;
            if (crSymId.isFuncSymId) {
                const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                return ResolvedSymResult{
                    .rSymId = rFuncSym.key.absResolvedFuncSymKey.rSymId,
                    .rFuncSymId = crSymId.id,
                };
            } else {
                return ResolvedSymResult{
                    .rSymId = crSymId.id,
                    .rFuncSymId = cy.NullId,
                };
            }
        } else {
            // Check builtin types.
            if (rParentSymId == chunk.semaResolvedRootSymId) {
                if (nameId < types.BuiltinTypeTags.len) {
                    return ResolvedSymResult{
                        .rSymId = @intCast(ResolvedSymId, nameId),
                        .rFuncSymId = cy.NullId,
                    };
                }
            }

            // Report missing symbol when looking in a module.
            const name = getName(chunk.compiler, nameId);
            return chunk.reportError("Missing symbol: `{}`", &.{v(name)});
        }
    }

    return null;
}

const FuncCallSymResult = struct {
    crSymId: CompactResolvedSymId,
    retType: Type,
};

/// Assumes rParentSymId is not null.
/// Returns CompileError if the symbol exists but can't be used for a function call.
/// Returns null if the symbol is missing but can still be used as an accessor.
fn getOrResolveSymForFuncCall(chunk: *cy.CompileChunk, rParentSymId: ResolvedSymId, nameId: NameSymId, args: []const Type, ret: Type) !?FuncCallSymResult {
    const rFuncSigId = try ensureResolvedFuncSigTypes(chunk.compiler, args, ret);
    log.debug("getFuncCallSym {}.{s}, sig: {s}", .{rParentSymId, getName(chunk.compiler, nameId), try getResolvedFuncSigTempStr(chunk.compiler, rFuncSigId)} );

    // TODO: Cache lookup by rSymId and rFuncSigId.

    var rParentSymIdFinal = rParentSymId;

    var key: vm_.KeyU64 = undefined;
    if (rParentSymId == chunk.semaResolvedRootSymId) {
        key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (chunk.localSyms.get(key)) |sym| {
            rParentSymIdFinal = sym.rParentSymId;
        } else {
            if (nameId < types.BuiltinTypeTags.len) {
                rParentSymIdFinal = cy.NullId;
            }
        }
    }

    key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymIdFinal,
            .nameId = nameId,
        },
    };

    var rSymId = chunk.compiler.sema.resolvedSymMap.get(key) orelse cy.NullId;
    if (rSymId != cy.NullId) {
        const sym = chunk.compiler.sema.getResolvedSym(rSymId);
        switch (sym.symT) {
            .variable => {
                // TODO: Check var type.
                return FuncCallSymResult{
                    .crSymId = CompactResolvedSymId.initSymId(rSymId),
                    .retType = types.AnyType,
                };
            },
            .builtinType,
            .func => {
                // Match against exact signature.
                key = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = rSymId,
                        .rFuncSigId = rFuncSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                    return FuncCallSymResult{
                        .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                        .retType = rFuncSym.retType,
                    };
                }

                // Fallthrough. Still need to check the module that contains the func 
                // in case it's an overloaded function not yet resolved.
            },
            else => {
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("`{}` is not a callable symbol.", &.{v(name)});
            }
        }
    }

    if (rParentSymIdFinal != cy.NullId) {
        const parentSym = chunk.compiler.sema.getResolvedSym(rParentSymIdFinal);
        const modId = parentSym.getModuleId() orelse return null;

        if (try findModuleSymForFuncCall(chunk, modId, nameId, args, ret)) |mod_rFuncSigId| {
            if (rSymId != cy.NullId) {
                key = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = rSymId,
                        .rFuncSigId = mod_rFuncSigId,
                    },
                };
                if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                    return FuncCallSymResult{
                        .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                        .retType = rFuncSym.retType,
                    };
                }
            }

            if (try resolveSymFromModule(chunk, modId, nameId, mod_rFuncSigId)) |crSymId| {
                if (crSymId.isFuncSymId) {
                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                    return FuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = rFuncSym.retType,
                    };
                } else {
                    return FuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = types.AnyType,
                    };
                }
            } else {
                stdx.panic("unexpected");
            }
        } else {
            try reportIncompatibleFuncSig(chunk, nameId, rFuncSigId, modId);
        }
    }

    // Look for <call> magic function.
    if (rSymId != cy.NullId) {
        const sym = chunk.compiler.sema.getResolvedSym(rSymId);
        if (sym.getModuleId()) |symModId| {
            const callNameId = try ensureNameSym(chunk.compiler, "<call>");
            if (try findModuleSymForFuncCall(chunk, symModId, callNameId, args, ret)) |funcSigId| {
                // Check if already resolved.
                key = AbsResolvedSymKey{
                    .absResolvedSymKey = .{
                        .rParentSymId = rSymId,
                        .nameId = callNameId,
                    },
                };
                if (chunk.compiler.sema.resolvedSymMap.get(key)) |callSymId| {
                    key = AbsResolvedFuncSymKey{
                        .absResolvedFuncSymKey = .{
                            .rSymId = callSymId,
                            .rFuncSigId = funcSigId,
                        },
                    };
                    if (chunk.compiler.sema.resolvedFuncSymMap.get(key)) |rFuncSymId| {
                        const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(rFuncSymId);
                        return FuncCallSymResult{
                            .crSymId = CompactResolvedSymId.initFuncSymId(rFuncSymId),
                            .retType = rFuncSym.retType,
                        };
                    }
                }

                if (try resolveSymFromModule(chunk, symModId, callNameId, funcSigId)) |crSymId| {
                    std.debug.assert(crSymId.isFuncSymId);

                    const rFuncSym = chunk.compiler.sema.getResolvedFuncSym(crSymId.id);
                    return FuncCallSymResult{
                        .crSymId = crSymId,
                        .retType = rFuncSym.retType,
                    };
                } else {
                    stdx.panic("unexpected");
                }
            }
        }
    }
    return null;
}

fn reportIncompatibleFuncSig(c: *cy.CompileChunk, nameId: NameSymId, rFuncSigId: ResolvedFuncSigId, searchModId: ModuleId) !void {
    const name = getName(c.compiler, nameId);
    const sigStr = try getResolvedFuncSigTempStr(c.compiler, rFuncSigId);

    const modKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };
    const mod = c.compiler.sema.getModule(searchModId);
    if (mod.syms.get(modKey)) |modSym| {
        if (modSym.symT == .symToOneFunc) {
            const existingSigStr = try allocResolvedFuncSigStr(c.compiler, modSym.inner.symToOneFunc.rFuncSigId);
            defer c.alloc.free(existingSigStr);
            return c.reportError(
                \\Can not find compatible function signature for `{}{}`.
                \\Only `func {}{}` exists for the symbol `{}`.
            , &.{v(name), v(sigStr), v(name), v(existingSigStr), v(name)});
        } else if (modSym.symT == .symToManyFuncs) {
            return c.reportError(
                \\Can not find compatible function signature for `{}{}`.
                \\There are multiple overloaded functions named `{}`.
            , &.{v(name), v(sigStr), v(name)});
        }
    }
    return c.reportError(
        \\Can not find compatible function signature for `{}{}`.
        \\`{}` does not exist.
    , &.{v(name), v(sigStr), v(name)});
}

fn isResolvedSymVisibleFromMod(c: *cy.VMcompiler, id: ResolvedSymId, modId: ModuleId) bool {
    const rsym = c.sema.resolvedSyms.items[id];
    if (rsym.exported) {
        return true;
    }
    return modId == getResolvedSymRootMod(c, id);
}

fn getResolvedSymRootMod(c: *cy.VMcompiler, id: ResolvedSymId) ModuleId {
    const rsym = c.sema.resolvedSyms.items[id];
    if (rsym.key.absResolvedSymKey.rParentSymId == cy.NullId) {
        return rsym.inner.module.id;
    } else {
        return getResolvedSymRootMod(c, rsym.key.absResolvedSymKey.rParentSymId);
    }
}

fn getVisibleResolvedTypeSymFromModule(c: *cy.CompileChunk, modId: ModuleId, nameId: NameSymId, firstNodeId: cy.NodeId) !?ResolvedSymId {
    const mod = c.compiler.modules.items[modId];
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .nameId = nameId,
            .rParentSymId = mod.resolvedRootSymId,
        },
    };
    if (try getAndCheckResolvedTypeSym(c, key, firstNodeId)) |rsymId| {
        if (isResolvedSymVisibleFromMod(c.compiler, rsymId, c.modId)) {
            return rsymId;
        } else {
            const name = getName(c.compiler, nameId);
            return c.reportErrorAt("Symbol is not exported: `{}`", &.{v(name)}, firstNodeId);
        }
    }
    return null;
}

fn getVisibleResolvedSymFromModule(c: *cy.CompileChunk, modId: ModuleId, nameId: NameSymId, rFuncSigId: ResolvedFuncSigId, firstNodeId: cy.NodeId) !?ResolvedSymId {
    const mod = c.compiler.modules.items[modId];
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .nameId = nameId,
            .rParentSymId = mod.resolvedRootSymId,
        },
    };
    if (try getAndCheckResolvedSymBySig(c, key, rFuncSigId, firstNodeId)) |rsymId| {
        if (isResolvedSymVisibleFromMod(c.compiler, rsymId, c.modId)) {
            return rsymId;
        } else {
            const name = getName(c.compiler, nameId);
            return c.reportErrorAt("Symbol is not exported: `{}`", &.{v(name)}, firstNodeId);
        }
    }
    return null;
}

fn getAndCheckResolvedTypeSym(c: *cy.CompileChunk, key: AbsResolvedSymKey, nodeId: cy.NodeId) !?ResolvedSymId {
    if (c.compiler.semaResolvedSymMap.get(key)) |id| {
        const rsym = c.compiler.sema.resolvedSyms.items[id];
        if (rsym.symT == .object) {
            return id;
        } else {
            return c.reportErrorAt("`{}` does not refer to a type.", &.{v(getName(c.compiler, key.absResolvedSymKey.nameId))}, nodeId);
        }
    } else return null;
}

/// Get the resolved sym that matches a signature.
fn getAndCheckResolvedSymBySig(c: *cy.CompileChunk, key: AbsResolvedSymKey, rFuncSigId: ResolvedFuncSigId, nodeId: cy.NodeId) !?ResolvedSymId {
    if (c.compiler.semaResolvedSymMap.get(key)) |id| {
        const rsym = c.compiler.sema.resolvedSyms.items[id];
        if (rFuncSigId == cy.NullId) {
            // Searching for a non-func reference.
            if (rsym.symT == .func) {
                if (rsym.inner.func.rFuncSymId != cy.NullId) {
                    // When the signature is for a non-func reference,
                    // a non overloaded function symbol can be used.
                    return id;
                } else {
                    return c.reportErrorAt("Can not disambiguate the symbol `{}`.", &.{v(getName(c.compiler, key.absResolvedSymKey.nameId))}, nodeId);
                }
            } else {
                return id;
            }
        } else {
            // Searching for function reference.
            if (rsym.symT == .variable) {
                // When the signature is for a func reference,
                // a variable symbol can be used.
                return id;
            } else if (rsym.symT == .func) {
                // Function signature must match exactly.
                const funcKey = AbsResolvedFuncSymKey{
                    .absResolvedFuncSymKey = .{
                        .rSymId = id,
                        .rFuncSigId = rFuncSigId,
                    },
                };
                if (c.compiler.semaResolvedFuncSymMap.contains(funcKey)) {
                    return id;
                } else {
                    return null;
                }
            } else {
                return c.reportErrorAt("Can not use `{}` as a function reference.", &.{v(getName(c.compiler, key.absResolvedSymKey.nameId))}, nodeId);
            }
        }
    } else return null;
}

fn resolveTypeSymFromModule(chunk: *cy.CompileChunk, modId: ModuleId, nameId: NameSymId) !?ResolvedSymId {
    const self = chunk.compiler;
    const relKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = cy.NullId,
        },
    };

    const mod = self.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        const key = AbsResolvedSymKey{
            .absResolvedSymKey = .{
                .rParentSymId = mod.resolvedRootSymId,
                .nameId = nameId,
            },
        };

        switch (modSym.symT) {
            .object => {
                const id = @intCast(u32, self.sema.resolvedSyms.items.len);
                try self.sema.resolvedSyms.append(self.alloc, .{
                    .symT = .object,
                    .key = key,
                    .inner = .{
                        .object = .{
                            .modId = cy.NullId,
                            // .declId = cy.NullId,
                        },
                    },
                    .exported = true,
                });
                try self.sema.resolvedSymMap.put(self.alloc, key, id);

                return id;
            },
            .userObject => {
                return chunk.reportError("Unsupported module sym: userObject", &.{});
            },

            else => {},
        }
    }
    return null;
}

/// If the name symbol points to only one function, the function sym is returned.
fn resolveSymFromModule(chunk: *cy.CompileChunk, modId: ModuleId, nameId: NameSymId, rFuncSigId: ResolvedFuncSigId) anyerror!?CompactResolvedSymId {
    const self = chunk.compiler;
    const relKey = RelModuleSymKey{
        .relModuleSymKey = .{
            .nameId = nameId,
            .rFuncSigId = rFuncSigId,
        },
    };

    const mod = self.sema.modules.items[modId];
    if (mod.syms.get(relKey)) |modSym| {
        const key = AbsResolvedSymKey{
            .absResolvedSymKey = .{
                .rParentSymId = mod.resolvedRootSymId,
                .nameId = nameId,
            },
        };

        switch (modSym.symT) {
            .nativeFunc1 => {
                const rtSymId = try self.vm.ensureFuncSym(mod.resolvedRootSymId, nameId, rFuncSigId);

                const rFuncSig = chunk.compiler.sema.getResolvedFuncSig(rFuncSigId);
                const rtSym = cy.FuncSymbolEntry.initNativeFunc1(modSym.inner.nativeFunc1.func, rFuncSig.isTyped, rFuncSig.numParams(), rFuncSigId);
                self.vm.setFuncSym(rtSymId, rtSym);

                const res = try setResolvedFunc(chunk, key, rFuncSigId, cy.NullId, true);
                return CompactResolvedSymId.initFuncSymId(res.rFuncSymId);
            },
            .variable => {
                const rtSymId = try self.vm.ensureVarSym(mod.resolvedRootSymId, nameId);
                const rtSym = cy.VarSym.init(modSym.inner.variable.val);
                cy.arc.retain(self.vm, rtSym.value);
                self.vm.setVarSym(rtSymId, rtSym);
                const id = try self.sema.addResolvedSym(key, .variable, .{
                    .variable = .{
                        .chunkId = cy.NullId,
                        .declId = cy.NullId,
                        .rTypeSymId = modSym.extra.variable.rTypeSymId,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .userVar => {
                _ = try self.vm.ensureVarSym(mod.resolvedRootSymId, nameId);

                const id = try self.sema.addResolvedSym(key, .variable, .{
                    .variable = .{
                        .chunkId = self.sema.modules.items[modId].chunkId,
                        .declId = modSym.inner.userVar.declId,
                        .rTypeSymId = bt.Any,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .userFunc => {
                _ = try self.vm.ensureFuncSym(mod.resolvedRootSymId, nameId, rFuncSigId);
                // Func sym entry will be updated when the func is generated later.

                const res = try setResolvedFunc(chunk, key, rFuncSigId, modSym.inner.userFunc.declId, true);
                return CompactResolvedSymId.initFuncSymId(res.rFuncSymId);
            },
            .object => {
                const id = try self.sema.addResolvedSym(key, .object, .{
                    .object = .{
                        .modId = modSym.inner.object.modId,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .enumType => {
                const id = try self.sema.addResolvedSym(key, .enumType, .{
                    .enumType = .{
                        .modId = modSym.inner.enumType.modId,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .enumMember => {
                const id = try self.sema.addResolvedSym(key, .enumMember, .{
                    .enumMember = .{
                        .enumId = modSym.inner.enumMember.rtEnumId,
                        .memberId = modSym.inner.enumMember.memberId,
                    },
                });
                return CompactResolvedSymId.initSymId(id);
            },
            .typeAlias => {
                const node = chunk.nodes[modSym.inner.typeAlias.declId];
                const rSymId = try getOrResolveTypeSymFromSpecNode(chunk, node.head.typeAliasDecl.typeSpecHead);
                return CompactResolvedSymId.initSymId(rSymId);
            },
            .symToManyFuncs => {
                // More than one func for sym.
                const name = getName(chunk.compiler, nameId);
                return chunk.reportError("Symbol `{}` is ambiguous. There are multiple functions with the same name.", &.{v(name)});
            },
            .symToOneFunc => {
                const sigId = modSym.inner.symToOneFunc.rFuncSigId;
                return resolveSymFromModule(chunk, modId, nameId, sigId);
            },
            .userObject => {
                return chunk.reportError("Unsupported module sym: userObject", &.{});
            },
        }
    }
    return null;
}

pub fn ensureNameSym(c: *cy.VMcompiler, name: []const u8) !NameSymId {
    return ensureNameSymExt(c, name, false);
}

pub fn ensureNameSymExt(c: *cy.VMcompiler, name: []const u8, dupe: bool) !NameSymId {
    const res = try @call(.never_inline, c.sema.nameSymMap.getOrPut, .{c.alloc, name});
    if (res.found_existing) {
        return res.value_ptr.*;
    } else {
        const id = @intCast(u32, c.sema.nameSyms.items.len);
        if (dupe) {
            const new = try c.alloc.dupe(u8, name);
            try c.sema.nameSyms.append(c.alloc, .{
                .ptr = new.ptr,
                .len = @intCast(u32, new.len),
                .owned = true,
            });
            res.key_ptr.* = new;
        } else {
            try c.sema.nameSyms.append(c.alloc, .{
                .ptr = name.ptr,
                .len = @intCast(u32, name.len),
                .owned = false,
            });
        }
        res.value_ptr.* = id;
        return id;
    }
}

/// TODO: This should also return true for local function symbols.
fn hasResolvedSym(self: *const cy.CompileChunk, rParentSymId: ResolvedSymId, nameId: NameSymId) bool {
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };
    return self.compiler.sema.resolvedSymMap.contains(key);
}

pub fn getVarName(c: *cy.CompileChunk, varId: LocalVarId) []const u8 {
    if (builtin.mode == .Debug) {
        return c.vars.items[varId].name;
    } else {
        return "";
    }
}

pub fn curSubBlock(self: *cy.CompileChunk) *SubBlock {
    return &self.semaSubBlocks.items[self.curSemaSubBlockId];
}

pub fn curBlock(self: *cy.CompileChunk) *Block {
    return &self.semaBlocks.items[self.curSemaBlockId];
}

pub fn endBlock(self: *cy.CompileChunk) !void {
    try endSubBlock(self);
    const sblock = curBlock(self);
    sblock.deinitTemps(self.alloc);
    self.semaBlockStack.items.len -= 1;
    self.curSemaBlockId = self.semaBlockStack.items[self.semaBlockStack.items.len-1];
}

fn accessExpr(self: *cy.CompileChunk, nodeId: cy.NodeId, comptime discardTopExprReg: bool) !Type {
    const node = self.nodes[nodeId];
    const right = self.nodes[node.head.accessExpr.right];
    if (right.node_t == .ident) {
        var left = self.nodes[node.head.accessExpr.left];
        if (left.node_t == .ident) {
            const name = self.getNodeTokenString(left);
            const nameId = try ensureNameSym(self.compiler, name);
            const res = try getOrLookupVar(self, name, .read);
            if (!res.isLocal) {
                const leftSym = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
                const crLeftSym = leftSym.toCompactId();
                try referenceSym(self, crLeftSym, true);
                self.nodes[node.head.accessExpr.left].head.ident.sema_crSymId = crLeftSym;

                const rightName = self.getNodeTokenString(right);
                const rightNameId = try ensureNameSym(self.compiler, rightName);

                self.curNodeId = node.head.accessExpr.right;
                if (try getOrResolveDistinctSym(self, leftSym.rSymId, rightNameId)) |symRes| {
                    const crRightSym = symRes.toCompactId();
                    try referenceSym(self, crRightSym, true);
                    self.nodes[nodeId].head.accessExpr.sema_crSymId = crRightSym;
                    return try getTypeForResolvedValueSym(self, crRightSym);
                }
            } else {
                self.nodes[node.head.accessExpr.left].head.ident.semaVarId = res.id;
            }
        } else if (left.node_t == .accessExpr) {
            _ = try accessExpr(self, node.head.accessExpr.left, discardTopExprReg);

            left = self.nodes[node.head.accessExpr.left];
            if (left.head.accessExpr.sema_crSymId.isPresent()) {
                const crLeftSym = left.head.accessExpr.sema_crSymId;
                if (!crLeftSym.isFuncSymId) {
                    const rightName = self.getNodeTokenString(right);
                    const rightNameId = try ensureNameSym(self.compiler, rightName);
                    if (try getOrResolveDistinctSym(self, crLeftSym.id, rightNameId)) |rightSym| {
                        const crRightSym = rightSym.toCompactId();
                        try referenceSym(self, crRightSym, true);
                        self.nodes[nodeId].head.accessExpr.sema_crSymId = crRightSym;
                    }
                }
            }
        } else {
            _ = try semaExpr(self, node.head.accessExpr.left, discardTopExprReg);
        }
    }
    return types.AnyType;
}

const VarResult = struct {
    id: LocalVarId,
    fromParentBlock: bool,
};

/// To a local type before assigning to a local variable.
fn toLocalType(vtype: Type) Type {
    if (vtype.typeT == .number and vtype.inner.number.canRequestInteger) {
        return types.NumberType;
    } else {
        return vtype;
    }
}

fn assignVar(self: *cy.CompileChunk, ident: cy.NodeId, vtype: Type, strat: VarLookupStrategy) !void {
    // log.debug("set var {s}", .{name});
    const node = self.nodes[ident];
    const name = self.getNodeTokenString(node);

    const res = try getOrLookupVar(self, name, strat);
    if (res.isLocal) {
        const svar = &self.vars.items[res.id];
        if (svar.isCaptured) {
            if (!svar.isBoxed) {
                // Becomes boxed so codegen knows ahead of time.
                svar.isBoxed = true;
            }
        }

        if (!res.created) {
            const ssblock = curSubBlock(self);
            if (!ssblock.prevVarTypes.contains(res.id)) {
                // Same variable but branched to sub block.
                try ssblock.prevVarTypes.put(self.alloc, res.id, svar.vtype);
            }
        }

        // Update current type after checking for branched assignment.
        if (svar.vtype.typeT != vtype.typeT) {
            svar.vtype = toLocalType(vtype);
            if (!svar.lifetimeRcCandidate and vtype.rcCandidate) {
                svar.lifetimeRcCandidate = true;

                // Rule #3 for setting genInitializer. Assumes right expr throws for now.
                svar.genInitializer = true;
            }
        }

        try self.assignedVarStack.append(self.alloc, res.id);
        self.nodes[ident].head.ident.semaVarId = res.id;
    } else {
        const nameId = try ensureNameSym(self.compiler, name);
        const symRes = try mustGetOrResolveDistinctSym(self, self.semaResolvedRootSymId, nameId);
        const crSymId = symRes.toCompactId();
        try referenceSym(self, crSymId, true);
        self.nodes[ident].head.ident.sema_crSymId = crSymId;
    }
}

fn endSubBlock(self: *cy.CompileChunk) !void {
    const sblock = curBlock(self);
    const ssblock = curSubBlock(self);

    const curAssignedVars = self.assignedVarStack.items[ssblock.assignedVarStart..];
    self.assignedVarStack.items.len = ssblock.assignedVarStart;

    if (sblock.subBlockDepth > 1) {
        const pssblock = self.semaSubBlocks.items[ssblock.prevSubBlockId];

        // Merge types to parent sub block.
        for (curAssignedVars) |varId| {
            const svar = &self.vars.items[varId];
            // log.debug("merging {s}", .{self.getVarName(varId)});
            if (ssblock.prevVarTypes.get(varId)) |prevt| {
                // Update current var type by merging.
                if (svar.vtype.typeT != prevt.typeT) {
                    svar.vtype = types.AnyType;

                    // Previous sub block hasn't recorded the var assignment.
                    if (!pssblock.prevVarTypes.contains(varId)) {
                        try self.assignedVarStack.append(self.alloc, varId);
                    }

                    // Record merged type for codegen.
                    try ssblock.endMergeTypes.append(self.alloc, .{
                        .id = varId,
                        .vtype = svar.vtype,
                    });
                }
            } else {
                // New variable assignment, propagate to parent block.
                try self.assignedVarStack.append(self.alloc, varId);
            }
        }
    }
    ssblock.prevVarTypes.deinit(self.alloc);

    self.curSemaSubBlockId = ssblock.prevSubBlockId;
    sblock.subBlockDepth -= 1;
}

fn pushIterSubBlock(self: *cy.CompileChunk) !void {
    try pushSubBlock(self);
}

fn endIterSubBlock(self: *cy.CompileChunk) !void {
    const ssblock = curSubBlock(self);
    for (self.assignedVarStack.items[ssblock.assignedVarStart..]) |varId| {
        const svar = self.vars.items[varId];
        if (ssblock.prevVarTypes.get(varId)) |prevt| {
            if (svar.vtype.typeT != prevt.typeT) {
                // Type differs from prev scope type. Record change for iter block codegen.
                try ssblock.iterVarBeginTypes.append(self.alloc, .{
                    .id = varId,
                    .vtype = types.AnyType,
                });
            }
        } else {
            // First assigned in iter block. Record change for iter block codegen.
            try ssblock.iterVarBeginTypes.append(self.alloc, .{
                .id = varId,
                .vtype = svar.vtype,
            });
        }
    }
    try endSubBlock(self);
}

pub fn importAllFromModule(self: *cy.CompileChunk, modId: ModuleId) !void {
    const mod = self.compiler.sema.modules.items[modId];
    var iter = mod.syms.iterator();
    while (iter.next()) |entry| {
        const key = entry.key_ptr.*.relModuleSymKey;
        const modSym = entry.value_ptr.*;
        switch (modSym.symT) {
            .variable,
            .symToManyFuncs,
            .symToOneFunc => {
                try setLocalSym(self, key.nameId, .{
                    .rSymId = cy.NullId,
                    .rFuncSymId = cy.NullId,
                    .rParentSymId = mod.resolvedRootSymId,
                });
            },
            .nativeFunc1 => {
                // Skip exact func syms.
            },
            else => {
                stdx.panicFmt("Unsupported {}", .{modSym.symT});
            },
        }
    }
}

/// Writes resolved spec to temp buf.
fn resolveSpecTemp(self: *cy.CompileChunk, spec: []const u8, outBuiltin: *bool) ![]const u8 {
    if (self.compiler.moduleLoaders.contains(spec)) {
        outBuiltin.* = true;
        return spec;
    }

    if (cy.isWasm) {
        return error.NotSupported;
    }

    if (std.mem.startsWith(u8, spec, "http://") or std.mem.startsWith(u8, spec, "https://")) {
        outBuiltin.* = false;
        const uri = try std.Uri.parse(spec);
        if (std.mem.endsWith(u8, uri.host.?, "github.com")) {
            if (std.mem.count(u8, uri.path, "/") == 2 and uri.path[uri.path.len-1] != '/') {
                self.tempBufU8.clearRetainingCapacity();
                try self.tempBufU8.appendSlice(self.alloc, uri.scheme);
                try self.tempBufU8.appendSlice(self.alloc, "://raw.githubusercontent.com");
                try self.tempBufU8.appendSlice(self.alloc, uri.path);
                try self.tempBufU8.appendSlice(self.alloc, "/master/mod.cys");
                std.debug.print("{s}\n", .{self.tempBufU8.items});
                return self.tempBufU8.items;
            }
        }
        return spec;
    }

    self.tempBufU8.clearRetainingCapacity();

    // Create path from the current script.
    // There should always be a parent directory since `srcUri` should be absolute when dealing with file modules.
    const dir = std.fs.path.dirname(self.srcUri) orelse return error.NoParentDir;
    try self.tempBufU8.ensureTotalCapacity(self.alloc, dir.len + 1 + spec.len + std.fs.MAX_PATH_BYTES);
    try self.tempBufU8.appendSlice(self.alloc, dir);
    try self.tempBufU8.append(self.alloc, '/');
    try self.tempBufU8.appendSlice(self.alloc, spec);
    const path = self.tempBufU8.items;

    // Get canonical path.
    self.tempBufU8.items.len += std.fs.MAX_PATH_BYTES;
    outBuiltin.* = false;
    return std.fs.cwd().realpath(path, self.tempBufU8.items[path.len..]) catch |err| {
        if (err == error.FileNotFound) {
            return self.reportError("Import path does not exist: `{}`", &.{v(path)});
        } else {
            return err;
        }
    };
}

pub fn appendSubModule(c: *cy.VMcompiler, name: []const u8) !ModuleId {
    const nameDupe = try c.alloc.dupe(u8, name);

    // Add empty module placeholder.
    const id = @intCast(u32, c.sema.modules.items.len);
    try c.sema.modules.append(c.alloc, .{
        .id = id,
        .syms = .{},
        .chunkId = cy.NullId,
        // Updated afterwards.
        .resolvedRootSymId = cy.NullId,
        .absSpec = nameDupe,
    });
    try c.sema.moduleMap.put(c.alloc, nameDupe, id);
    return id;
}

pub fn appendRootModule(c: *cy.VMcompiler, absSpec: []const u8) !ModuleId {
    const absSpecDupe = try c.alloc.dupe(u8, absSpec);

    // Add empty module placeholder.
    const id = @intCast(u32, c.sema.modules.items.len);
    const rModSymId = try resolveRootModuleSym(c, absSpecDupe, id);
    try c.sema.modules.append(c.alloc, .{
        .id = id,
        .syms = .{},
        .chunkId = cy.NullId,
        .resolvedRootSymId = rModSymId,
        .absSpec = absSpecDupe,
    });
    try c.sema.moduleMap.put(c.alloc, absSpecDupe, id);
    return id;
}

pub fn getOrLoadModule(self: *cy.CompileChunk, spec: []const u8, nodeId: cy.NodeId) !ModuleId {
    var isBuiltin: bool = undefined;
    const absSpec = try resolveSpecTemp(self, spec, &isBuiltin);

    if (self.compiler.sema.moduleMap.get(absSpec)) |modId| {
        return modId;
    } else {
        const modId = try appendRootModule(self.compiler, absSpec);
        const dupedAbsSpec = self.compiler.sema.getModule(modId).absSpec;

        // Queue import task.
        try self.compiler.importTasks.append(self.alloc, .{
            .chunkId = self.id,
            .nodeId = nodeId,
            .absSpec = dupedAbsSpec,
            .modId = modId,
            .builtin = isBuiltin,
        });
        return modId;
    }
}

pub fn resolveEnumSym(c: *cy.VMcompiler, rParentSymId: ResolvedSymId, name: []const u8, modId: ModuleId) !ResolvedSymId {
    const nameId = try ensureNameSym(c, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };
    if (c.sema.resolvedSymMap.contains(key)) {
        return error.DuplicateSymName;
    }

    // Resolve the symbol.
    const rSymId = @intCast(u32, c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .symT = .enumT,
        .key = key,
        .inner = .{
            .enumT = .{
                .modId = modId,
            },
        },
        .exported = true,
    });
    try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, rSymId});

    return rSymId;
}

pub fn resolveObjectSym(c: *cy.VMcompiler, rParentSymId: ResolvedSymId, name: []const u8, modId: ModuleId) !ResolvedSymId {
    const nameId = try ensureNameSym(c, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };
    if (c.sema.resolvedSymMap.contains(key)) {
        return error.DuplicateSymName;
    }

    // Resolve the symbol.
    const rSymId = @intCast(u32, c.sema.resolvedSyms.items.len);
    try c.sema.resolvedSyms.append(c.alloc, .{
        .symT = .object,
        .key = key,
        .inner = .{
            .object = .{
                .modId = modId,
            },
        },
        .exported = true,
    });
    try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, rSymId});

    return rSymId;
}

/// Given the local sym path, add a resolved object sym entry.
/// Assumes parent is resolved.
fn resolveLocalObjectSym(chunk: *cy.CompileChunk, rParentSymId: ResolvedSymId, name: []const u8, modId: ModuleId, declId: cy.NodeId, exported: bool) !ResolvedSymId {
    _ = exported;
    const c = chunk.compiler;
    return resolveObjectSym(c, rParentSymId, name, modId) catch |err| {
        if (err == error.DuplicateSymName) {
            return chunk.reportErrorAt("The symbol `{}` was already declared.", &.{v(name)}, declId);
        } else {
            return err;
        }
    };
}

/// A root module symbol is used as the parent for it's members.
pub fn resolveRootModuleSym(self: *cy.VMcompiler, name: []const u8, modId: ModuleId) !ResolvedSymId {
    const nameId = try ensureNameSym(self, name);
    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = cy.NullId,
            .nameId = nameId,
        },
    };
    if (self.sema.resolvedSymMap.contains(key)) {
        // Assume no existing symbol, since each module has a unique srcUri.
        log.debug("Root symbol {s} already exists.", .{name});
        stdx.fatal();
    }

    // Resolve the symbol.
    const resolvedId = @intCast(u32, self.sema.resolvedSyms.items.len);
    try self.sema.resolvedSyms.append(self.alloc, .{
        .symT = .module,
        .key = key,
        .inner = .{
            .module = .{
                .id = modId,
            },
        },
        .exported = true,
    });
    try @call(.never_inline, self.sema.resolvedSymMap.put, .{self.alloc, key, resolvedId});

    return resolvedId;
}

/// Given the local sym path, add a resolved var sym entry.
/// Fail if there is already a symbol in this path with the same name.
fn resolveLocalVarSym(self: *cy.CompileChunk, rParentSymId: ResolvedSymId, nameId: NameSymId, typeSymId: ResolvedSymId, declId: cy.NodeId, exported: bool) !ResolvedSymId {
    if (rParentSymId == self.semaResolvedRootSymId) {
        // Check for local sym.
        const key = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (self.localSyms.contains(key)) {
            const node = self.nodes[declId];
            const varSpec = self.nodes[node.head.varDecl.varSpec];
            return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, nameId))}, varSpec.head.varSpec.name);
        }
    }

    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId,
            .nameId = nameId,
        },
    };

    if (self.compiler.sema.resolvedSymMap.contains(key)) {
        return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, nameId))}, declId);
    }

    // Resolve the symbol.
    const resolvedId = @intCast(u32, self.compiler.sema.resolvedSyms.items.len);
    try self.compiler.sema.resolvedSyms.append(self.alloc, .{
        .symT = .variable,
        .key = key,
        .inner = .{
            .variable = .{
                .chunkId = self.id,
                .declId = declId,
                .rTypeSymId = typeSymId,
            },
        },
        .exported = exported,
    });

    try @call(.never_inline, self.compiler.sema.resolvedSymMap.put, .{self.alloc, key, resolvedId});

    return resolvedId;
}

/// Dump the full path of a resolved sym.
fn allocAbsResolvedSymName(self: *cy.VMcompiler, id: ResolvedSymId) ![]const u8 {
    const sym = self.sema.resolvedSyms.items[id];
    if (sym.key.absResolvedSymKey.rParentSymId != cy.NullId) {
        var buf: std.ArrayListUnmanaged(u8) = .{};
        defer buf.deinit(self.alloc);
        try allocAbsResolvedSymNameR(self, &buf, sym.key.absResolvedSymKey.rParentSymId);
        try buf.append(self.alloc, '.');
        try buf.appendSlice(self.alloc, getName(self, sym.key.absResolvedSymKey.nameId));
        return buf.toOwnedSlice(self.alloc);
    } else {
        const name = getName(self, sym.key.absResolvedSymKey.nameId);
        return self.alloc.dupe(u8, name);
    }
}

fn allocAbsResolvedSymNameR(self: *cy.VMcompiler, buf: *std.ArrayListUnmanaged(u8), id: ResolvedSymId) !void {
    const sym = self.sema.resolvedSyms.items[id];
    if (sym.key.absResolvedSymKey.rParentSymId == cy.NullId) {
        try buf.appendSlice(self.alloc, getName(self, sym.key.absResolvedSymKey.nameId));
    } else {
        try allocAbsResolvedSymNameR(self, buf, sym.key.absResolvedSymKey.rParentSymId);
        try buf.append(self.alloc, '.');
        try buf.appendSlice(self.alloc, getName(self, sym.key.absResolvedSymKey.nameId));
    }
}

const ResolveFuncSymResult = struct {
    rSymId: ResolvedSymId,
    rFuncSymId: ResolvedFuncSymId,
};

fn setResolvedFunc(self: *cy.CompileChunk, key: AbsResolvedSymKey, rFuncSigId: ResolvedFuncSigId, declId: u32, exported: bool) !ResolvedSymResult {
    const c = self.compiler;
    var rsymId: ResolvedSymId = undefined;
    var createdSym = false;
    if (c.sema.resolvedSymMap.get(key)) |id| {
        const rsym = c.sema.resolvedSyms.items[id];
        if (rsym.symT != .func) {
            // Only fail if the symbol already exists and isn't a function.
            const name = getName(c, key.absResolvedSymKey.nameId);
            return self.reportError("The symbol `{}` was already declared.", &.{v(name)});
        }
        rsymId = id;
    } else {
        rsymId = @intCast(u32, c.sema.resolvedSyms.items.len);
        try c.sema.resolvedSyms.append(c.alloc, .{
            .symT = .func,
            .key = key,
            .inner = .{
                .func = .{
                    .rFuncSymId = undefined,
                },
            },
            .exported = exported,
        });
        try @call(.never_inline, c.sema.resolvedSymMap.put, .{c.alloc, key, rsymId});
        createdSym = true;
    }

    // Now check resolved function syms.
    const funcKey = AbsResolvedFuncSymKey{
        .absResolvedFuncSymKey = .{
            .rSymId = rsymId,
            .rFuncSigId = rFuncSigId,
        },
    };
    if (c.sema.resolvedFuncSymMap.contains(funcKey)) {
        const name = getName(c, key.absResolvedSymKey.nameId);
        return self.reportError("The function symbol `{}` with the same signature was already declared.", &.{v(name)});
    }

    const rFuncSig = c.sema.getResolvedFuncSig(rFuncSigId);
    const retSymId = rFuncSig.getRetTypeSymId();

    const rfsymId = @intCast(u32, c.sema.resolvedFuncSyms.items.len);
    try c.sema.resolvedFuncSyms.append(c.alloc, .{
        .chunkId = self.id,
        .declId = declId,
        .key = funcKey,
        .retType = try types.typeFromResolvedSym(self, retSymId),
        .hasStaticInitializer = false,
    });
    try @call(.never_inline, c.sema.resolvedFuncSymMap.put, .{c.alloc, funcKey, rfsymId});

    if (createdSym) {
        c.sema.resolvedSyms.items[rsymId].inner.func.rFuncSymId = rfsymId;
    } else {
        // Mark sym as overloaded.
        c.sema.resolvedSyms.items[rsymId].inner.func.rFuncSymId = cy.NullId;
    }

    return ResolvedSymResult{
        .rSymId = rsymId,
        .rFuncSymId = rfsymId,
    };
}

/// Given the local sym path, add a resolved func sym entry.
/// Assumes parent local sym is resolved.
pub fn resolveLocalFuncSym(self: *cy.CompileChunk, rParentSymId: ?ResolvedSymId, nameId: NameSymId,
    declId: FuncDeclId, rFuncSigId: ResolvedFuncSigId, exported: bool
) !ResolvedSymResult {
    const func = self.semaFuncDecls.items[declId];

    const key = AbsResolvedSymKey{
        .absResolvedSymKey = .{
            .rParentSymId = rParentSymId orelse cy.NullId,
            .nameId = nameId,
        },
    };

    if (key.absResolvedSymKey.rParentSymId == self.semaResolvedRootSymId) {
        // Root symbol, check that it's not a local alias.
        const lkey = RelLocalSymKey{
            .relLocalSymKey = .{
                .nameId = nameId,
                .rFuncSigId = cy.NullId,
            },
        };
        if (self.localSyms.contains(lkey)) {
            const name = func.getNameNode(self);
            return self.reportErrorAt("The symbol `{}` was already declared.", &.{v(getName(self.compiler, key.absResolvedSymKey.nameId))}, name);
        }
    }

    const res = try setResolvedFunc(self, key, rFuncSigId, declId, exported);
    if (builtin.mode == .Debug and cy.verbose) {
        const name = try allocAbsResolvedSymName(self.compiler, res.rSymId);
        defer self.alloc.free(name);
        const sigStr = try getResolvedFuncSigTempStr(self.compiler, rFuncSigId);
        log.debug("resolved static func: func {s}{s}", .{name, sigStr});
    }

    self.semaFuncDecls.items[declId].inner.staticFunc = .{
        .semaResolvedSymId = res.rSymId,
        .semaResolvedFuncSymId = res.rFuncSymId,
    };

    return res;
}

fn endFuncBlock(self: *cy.CompileChunk, numParams: u32) !void {
    const sblock = curBlock(self);
    const numCaptured = @intCast(u8, sblock.params.items.len - numParams);
    if (numCaptured > 0) {
        for (sblock.params.items) |varId| {
            const svar = self.vars.items[varId];
            if (svar.isCaptured) {
                const pId = self.capVarDescs.get(varId).?.user;
                const pvar = &self.vars.items[pId];

                if (!pvar.isBoxed) {
                    pvar.isBoxed = true;
                    pvar.lifetimeRcCandidate = true;
                }
            }
        }
    }
    try endBlock(self);
}

fn endFuncSymBlock(self: *cy.CompileChunk, numParams: u32) !void {
    const sblock = curBlock(self);
    const numCaptured = @intCast(u8, sblock.params.items.len - numParams);
    if (builtin.mode == .Debug and numCaptured > 0) {
        stdx.panicFmt("Captured var in static func.", .{});
    }
    try endBlock(self);
}

fn pushCallArgs(chunk: *cy.CompileChunk, argHead: cy.NodeId) ![]const Type {
    const start = chunk.compiler.typeStack.items.len;
    var nodeId = argHead;
    while (nodeId != cy.NullId) {
        const arg = chunk.nodes[nodeId];
        const argT = try semaExpr(chunk, nodeId, false);
        try chunk.compiler.typeStack.append(chunk.alloc, argT);
        nodeId = arg.next;
    }
    return chunk.compiler.typeStack.items[start..];
}

pub const ResolvedFuncSigId = u32;
pub const ResolvedFuncSig = struct {
    /// Last elem is the return type sym.
    paramPtr: [*]const ResolvedSymId,
    retSymId: ResolvedSymId,
    paramLen: u16,
    isTyped: bool,

    pub fn params(self: ResolvedFuncSig) []const ResolvedSymId {
        return self.paramPtr[0..self.paramLen];
    }

    pub fn numParams(self: ResolvedFuncSig) u8 {
        return @intCast(u8, self.paramLen);
    }

    pub fn getRetTypeSymId(self: ResolvedFuncSig) ResolvedSymId {
        return self.retSymId;
    }

    pub fn deinit(self: *ResolvedFuncSig, alloc: std.mem.Allocator) void {
        alloc.free(self.params());
    }
};

pub const NameAny = 0;
pub const NameBoolean = 1;
pub const NameNumber = 2;
pub const NameInt = 3;
pub const NameString = 4;
pub const NameRawstring = 5;
pub const NameSymbol = 6;
pub const NameList = 7;
pub const NameMap = 8;
pub const NamePointer = 9;
pub const NameNone = 10;
pub const NameError = 11;
pub const NameFiber = 12;
pub const NameMetatype = 13;

pub const FuncDeclId = u32;

pub const FuncDecl = struct {
    nodeId: cy.NodeId,

    paramHead: Nullable(cy.NodeId),

    /// Resolved func signature.
    rFuncSigId: ResolvedFuncSigId,

    /// Resolved return type sym. NullId indicates no declaration.
    rRetTypeSymId: Nullable(ResolvedSymId),

    /// Sema block is attached to func decl so it can be accessed from anywhere with the node id.
    /// If the func decl has an initializer then this is repurposed to point to the decl's node id.
    semaBlockId: u32 = cy.NullId,

    inner: extern union {
        staticFunc: extern struct {
            semaResolvedSymId: u32 = cy.NullId,
            semaResolvedFuncSymId: u32 = cy.NullId,
        },
        lambda: extern struct {
            rFuncSigId: u32 = cy.NullId,
        },
    },

    /// Used by funcDeclInit to generate static initializer dependencies.
    rFuncSymId: Nullable(ResolvedFuncSymId) = cy.NullId,

    rSymId: ResolvedSymId,

    /// pc to start of end locals procedure.
    genEndLocalsPc: u32 = cy.NullId,

    /// Number of params in the function signature.
    numParams: u8,

    /// Whether this is a static function.
    isStatic: bool,

    pub fn hasReturnTypeSpec(self: *const FuncDecl) bool {
        return self.rRetTypeSymId != cy.NullId;
    }

    fn getReturnType(self: *const FuncDecl, c: *cy.CompileChunk) !Type {
        if (!self.hasReturnTypeSpec()) {
            return types.AnyType;
        } else {
            return try types.typeFromResolvedSym(c, self.rRetTypeSymId);
        }
    }

    pub fn getReturnNode(self: *const FuncDecl, chunk: *const cy.CompileChunk) cy.NodeId {
        const node = chunk.nodes[self.nodeId];
        const header = chunk.nodes[node.head.func.header];
        return header.head.funcHeader.ret;
    }

    fn getNameNode(self: *const FuncDecl, chunk: *const cy.CompileChunk) cy.NodeId {
        const node = chunk.nodes[self.nodeId];
        const header = chunk.nodes[node.head.func.header];
        return header.head.funcHeader.name;
    }

    pub fn getName(self: *const FuncDecl, chunk: *const cy.CompileChunk) []const u8 {
        const node = chunk.nodes[self.nodeId];
        const header = chunk.nodes[node.head.func.header];
        const name = header.head.funcHeader.name;
        if (name == cy.NullId) {
            return "";
        } else {
            return chunk.getNodeTokenString(chunk.nodes[name]);
        }
    }

    pub fn getNameFromParser(self: *const FuncDecl, parser: *const cy.Parser) []const u8 {
        const node = parser.nodes.items[self.nodeId];
        const header = parser.nodes.items[node.head.func.header];
        const name = header.head.funcHeader.name;
        if (name == cy.NullId) {
            return "";
        } else {
            const nameN = parser.nodes.items[name];
            const token = parser.tokens.items[nameN.start_token];
            return parser.src[token.pos()..token.data.end_pos];
        }
    }
};

const ResolvedFuncSigKey = struct {
    paramPtr: [*]const ResolvedSymId,
    paramLen: u32,
    retSymId: ResolvedSymId,
};

pub const Model = struct {
    alloc: std.mem.Allocator,

    /// Unique name syms.
    nameSyms: std.ArrayListUnmanaged(Name),
    nameSymMap: std.StringHashMapUnmanaged(NameSymId),

    /// Resolved symbols are shared among all modules.
    /// Only resolved symbols are included in the execution runtime.
    /// When a symbol is missing, sema will attempt to find it within the resolved parent module.
    /// Each symbol is keyed by the absolute path to the symbol.
    resolvedSyms: std.ArrayListUnmanaged(ResolvedSym),
    resolvedSymMap: std.HashMapUnmanaged(AbsResolvedSymKey, ResolvedSymId, vm_.KeyU64Context, 80),

    /// Resolved function symbols that are included in the runtime.
    /// Each func symbol is keyed by the resolved sym and function signature.
    resolvedFuncSyms: std.ArrayListUnmanaged(ResolvedFuncSym),
    resolvedFuncSymMap: std.HashMapUnmanaged(AbsResolvedFuncSymKey, ResolvedFuncSymId, vm_.KeyU64Context, 80),

    /// Resolved signatures for functions.
    resolvedFuncSigs: std.ArrayListUnmanaged(ResolvedFuncSig),
    resolvedFuncSigMap: std.HashMapUnmanaged(ResolvedFuncSigKey, ResolvedFuncSigId, ResolvedFuncSigKeyContext, 80),

    /// Fast index to untyped func sig id by num params.
    /// `NullId` indicates a missing func sig id.
    /// TODO: If Cyber implements multiple return values, this would need to be a map.
    resolvedUntypedFuncSigs: std.ArrayListUnmanaged(ResolvedFuncSigId),

    /// Modules.
    modules: std.ArrayListUnmanaged(Module),
    /// Owned absolute specifier path to module.
    moduleMap: std.StringHashMapUnmanaged(ModuleId),

    pub fn init(alloc: std.mem.Allocator) Model {
        return .{
            .alloc = alloc,
            .nameSyms = .{},
            .nameSymMap = .{},
            .resolvedSyms = .{},
            .resolvedSymMap = .{},
            .resolvedFuncSyms = .{},
            .resolvedFuncSymMap = .{},
            .resolvedFuncSigs = .{},
            .resolvedFuncSigMap = .{},
            .resolvedUntypedFuncSigs = .{},
            .modules = .{},
            .moduleMap = .{},
        };
    }

    pub fn deinit(self: *Model, alloc: std.mem.Allocator, comptime reset: bool) void {
        if (reset) {
            self.resolvedSyms.clearRetainingCapacity();
            self.resolvedSymMap.clearRetainingCapacity();
            self.resolvedFuncSyms.clearRetainingCapacity();
            self.resolvedFuncSymMap.clearRetainingCapacity();
        } else {
            self.resolvedSyms.deinit(alloc);
            self.resolvedSymMap.deinit(alloc);
            self.resolvedFuncSyms.deinit(alloc);
            self.resolvedFuncSymMap.deinit(alloc);
        }

        for (self.modules.items) |*mod| {
            mod.deinit(alloc);
        }
        if (reset) {
            self.modules.clearRetainingCapacity();
            self.moduleMap.clearRetainingCapacity();
        } else {
            self.modules.deinit(alloc);
            self.moduleMap.deinit(alloc);
        }

        for (self.nameSyms.items) |name| {
            if (name.owned) {
                alloc.free(name.getName());
            }
        }
        if (reset) {
            self.nameSyms.clearRetainingCapacity();
            self.nameSymMap.clearRetainingCapacity();
        } else {
            self.nameSyms.deinit(alloc);
            self.nameSymMap.deinit(alloc);
        }

        for (self.resolvedFuncSigs.items) |*it| {
            it.deinit(alloc);
        }
        if (reset) {
            self.resolvedFuncSigs.clearRetainingCapacity();
            self.resolvedFuncSigMap.clearRetainingCapacity();
            self.resolvedUntypedFuncSigs.clearRetainingCapacity();
        } else {
            self.resolvedFuncSigs.deinit(alloc);
            self.resolvedFuncSigMap.deinit(alloc);
            self.resolvedUntypedFuncSigs.deinit(alloc);
        }
    }

    pub fn getResolvedSym(self: *Model, id: ResolvedSymId) ResolvedSym {
        return self.resolvedSyms.items[id];
    }

    pub fn addResolvedSym(self: *Model, key: AbsResolvedSymKey, symT: ResolvedSymType, data: ResolvedSymData) !ResolvedSymId {
        const id = @intCast(u32, self.resolvedSyms.items.len);
        try self.resolvedSyms.append(self.alloc, .{
            .symT = symT,
            .key = key,
            .inner = data,
            .exported = true,
        });
        try self.resolvedSymMap.put(self.alloc, key, id);
        return id;
    }

    pub fn getResolvedSymPtr(self: *Model, id: ResolvedSymId) *ResolvedSym {
        return &self.resolvedSyms.items[id];
    }

    pub fn getResolvedFuncSym(self: *Model, id: ResolvedFuncSymId) ResolvedFuncSym {
        return self.resolvedFuncSyms.items[id];
    }

    pub fn getResolvedFuncSymPtr(self: *Model, id: ResolvedFuncSymId) *ResolvedFuncSym {
        return &self.resolvedFuncSyms.items[id];
    }

    pub fn getResolvedFuncSig(self: *Model, id: ResolvedFuncSigId) ResolvedFuncSig {
        return self.resolvedFuncSigs.items[id];
    }

    pub fn getModule(self: *Model, id: ModuleId) Module {
        return self.modules.items[id];
    }

    pub fn getModulePtr(self: *Model, id: ModuleId) *Module {
        return &self.modules.items[id];
    }
};

pub const ResolvedFuncSigKeyContext = struct {
    pub fn hash(_: @This(), key: ResolvedFuncSigKey) u64 {
        var c = std.hash.Wyhash.init(0);
        c.update(@ptrCast([*]const u8, key.paramPtr)[0..key.paramLen*4]);
        c.update(std.mem.asBytes(&key.retSymId));
        return c.final();
    }
    pub fn eql(_: @This(), a: ResolvedFuncSigKey, b: ResolvedFuncSigKey) bool {
        return std.mem.eql(u32, a.paramPtr[0..a.paramLen], b.paramPtr[0..b.paramLen]);
    }
};

pub const U32SliceContext = struct {
    pub fn hash(_: @This(), key: []const u32) u64 {
        var c = std.hash.Wyhash.init(0);
        c.update(@ptrCast([*]const u8, key.ptr)[0..key.len*4]);
        return c.final();
    }
    pub fn eql(_: @This(), a: []const u32, b: []const u32) bool {
        return std.mem.eql(u32, a, b);
    }
};

test "Internals." {
    try t.eq(@sizeOf(LocalVar), 40);
    try t.eq(@sizeOf(ResolvedFuncSym), 32);
    try t.eq(@sizeOf(ResolvedFuncSig), 16);
    try t.eq(@sizeOf(ResolvedSym), 24);
    try t.eq(@sizeOf(Name), 16);
    try t.eq(@sizeOf(ModuleFuncNode), 16);
    try t.eq(@sizeOf(CompactResolvedSymId), 4);
    try t.eq(@sizeOf(ModuleSym), 24);
}
