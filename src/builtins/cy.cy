use meta

--| Utilities related to Cyber the language.
--| Can also access libcyber.

--| Sample usage:
--| ```cy
--| use cy
--|
--| res := cy.eval('return 1')!
--| ```

const Success      = 0
const Await        = 1
const ErrorCompile = 2
const ErrorPanic   = 3

const TypeVoid  = 0
const TypeBool  = 2
const TypeI64   = 6
const TypeInt   = TypeI64
const TypeF64   = 13
const TypeFloat = TypeF64
const TypeStr   = 23

--| Evaluates source code in an isolated VM.
--| If the last statement is an expression, a primitive or a string can be returned.
fn eval(src str) -> !Object:
    vm := VM()  
    res := vm.eval(src)
    if res.code != Success:
        if res.code == ErrorCompile:
            report := vm.compile_error_summary()
            eprint(report)
        else res.code == ErrorPanic:
            report := vm.panic_summary()
            eprint(report)
        else:
            eprint('Unknown error')
        return error.EvalError

    -- Let VM clean up heap.
    return res.value.to_host(res.val_t)

-- TODO: Implement Node as trait to save more memory.
type Node enum:
    case null                void
    case access_expr         AccessExpr
    case all                 void
    case as_expr             AsExpr
    case assign_stmt         void
    case at_lit              Literal
    case attribute           void
    case begin_stmt          void
    case bin_expr            BinExpr
    case binLit              void
    case borrow              BorrowExpr
    case breakStmt           void
    case case_stmt           CaseStmt
    case call_expr           CallExpr
    case catchStmt           void
    case const_decl          ConstDecl
    case continueStmt        void
    case cstruct_decl        StructDecl
    case ct_stmt             void
    case cunion_decl         CUnionDecl
    case custom_type_decl    CustomTypeDecl
    case dec_lit             Literal
    case dec_u               void
    case deref               void
    case dollar              void
    case dollar_lit          void
    case dot                 void
    case dot_lit             Literal
    case else_block          void
    case elseif_block        void
    case enum_decl           EnumDecl
    case enum_member         EnumMember
    case error_lit           void
    case ex_borrow           ExBorrow
    case expand_lit          void
    case exprStmt            void
    case falseLit            void
    case for_iter_stmt       void
    case for_range_stmt      void
    case float_lit           Literal
    case func_decl           FuncDecl
    case func_param          FuncParam
    case fn_type             FuncType
    case fnsym_type          FuncType
    case generic_expand      GenericExpand
    case generic_vector_type GenericVectorType
    case global_decl         GlobalDecl
    case group               Group
    case hex_lit             Literal
    case ident               Ident
    case if_expr             void
    case if_stmt             void
    case if_unwrap_stmt      void
    case impl_decl           ImplDecl
    case import_stmt         ImportStmt
    case index_expr          IndexExpr
    case infer_param         PrefixLit
    case init_expr           void
    case init_lit            InitLit
    case keyValue            void
    case label_decl          void
    case lambda_cont_expr    void
    case lambda_cont         void
    case lambda_expr         void
    case lambda_multi        void
    case move_expr           void
    case namedArg            void
    case noneLit             void
    case octLit              void
    case op_assign_stmt      void
    case option_type         OptionType
    case partial_vector_type void
    case pass_stmt           void
    case ptr                 PtrExpr
    case range               Range
    case raw_string_lit      Literal
    case raw_string_multi_lit void
    case ref                 RefExpr
    case return_expr_stmt    ReturnExprStmt
    case return_stmt         void
    case root                Root
    case seqDestructure      void
    case slice_type          SliceType
    case span_type           SpanType
    case special_string_lit  void
    case sq_string_lit       Literal
    case sq_string_multi_lit Literal
    case string_lit          Literal
    case string_multi_lit    Literal
    case stringt             void
    case stringt_multi       void
    case stringt_part        void
    case stringt_expr        void
    case struct_decl         StructDecl
    case struct_field        void
    case switch_expr         SwitchBlock 
    case switch_stmt         SwitchBlock
    case template            TemplateDecl
    case trait_decl          TraitDecl
    case true_lit            void
    case tryStmt             void
    case type_alias_decl     TypeAliasDecl
    case type_const_decl     TypeConstDecl
    case unary_expr          Unary
    case undef_lit           void
    case union_case          void
    case unwrap              void
    case unwrap_choice       void
    case unwrap_or           void
    case unwrap_or_block     void
    case unwrap_res          void
    case unwrap_res_or       void
    case unwrap_res_or_block void
    case use_alias           void
    case var_decl            VarDecl
    case vector_type         VectorType
    case void_lit            Token
    case whileCondStmt       void
    case whileInfStmt        void
    case while_unwrap_stmt   void
    case with                With
    case yield_stmt          void

fn (&Node) decl_name() -> str:
    return switch $:
        case .func_decl |node| => node.name.name()
        case .const_decl |node| => node.name.name()
        case .type_alias_decl |node| => node.name.name()
        case .global_decl |node| => node.name.name()
        else => panic('Not a decl node: %{meta.choice_tag(Node, $)}')

fn (&Node) name() -> str:
    return switch $:
        case .raw_string_lit |node| => node.value
        case .at_lit |node| => node.value
        case .ident |node| => node.name
        else => panic('No name: %{meta.choice_tag(Node, $)}')

-- Returns whether a two statements are adjacent to one another.
-- Two lines are adjacent if separated by a new line and optional indentation.
fn is_adjacent_stmt(src str, a_end int, b_start int) -> bool:
    if b_start < a_end:
        return false

    i := a_end
    if src[i] == '\r':
        i += 1
        if src[i] != '\n':
            return false
        i += 1
    else src[i] == '\n':
        i += 1
    else:
        return false

    while i < b_start:
        if src[i] != ' ' and src[i] != '\t':
            return false
        i += 1
    return true

fn (&Node) end() -> int:
    switch $:
        -- .null           => cy.NullId,
        -- .all            => self.cast(.all).pos + 1,
        case .access_expr |node|: return node.right.end()
        case .as_expr |node|: return node.expr.end()
        -- .assign_stmt    => self.cast(.assign_stmt).right.end(),
        -- .at             => self.cast(.at).pos + 1,
        case .at_lit |node|: return node.end()
        -- .attribute      => {
        --     const attr = self.cast(.attribute);
        --     if (attr.value) |value| {
        --         return value.end();
        --     }
        --     return attr.pos + 1;
        -- },
        case .bin_expr |node|: return node.right.end()
        -- .binLit         => self.cast(.binLit).end(),
        case .borrow |node|: return node.child.end()
        -- .breakStmt      => self.cast(.breakStmt).pos + 5,
        case .call_expr |node|: return node.end
        case .case_stmt |node|:
            switch node.body_data:
                case .expr |data|:
                    return data.end()
                case .block |data|:
                    return data.last().end()
                case .fallthrough:
                    switch node.data:
                        else:
                            return $pos() + 4
        -- .catchStmt      => {
        --     const catch_stmt = self.cast(.catchStmt);
        --     return catch_stmt.stmts[catch_stmt.stmts.len-1].end();
        -- },
        -- .comment => return self.cast(.comment).end(),
        -- .const_decl     => {
        --     const decl = self.cast(.const_decl);
        --     if (decl.right) |right| {
        --         return right.end();
        --     }
        --     return decl.type.?.end();
        -- },
        -- .continueStmt   => self.cast(.continueStmt).pos + 8,
        -- .cstruct_decl   => {
        --     const decl = self.cast(.cstruct_decl);
        --     const field: *Node = @ptrCast(decl.fields.ptr[decl.fields.len-1]);
        --     return field.end();
        -- },
        -- .ct_stmt        => self.cast(.ct_stmt).child.end(),
        case .cunion_decl |node|:
            if node.cases.len() > 0:
                return node.cases.last().end()
            else:
                return node.name.end()
        case .custom_type_decl |node|: return node.name.end()
        case .dec_lit |node|: return node.end()
        -- .dec_u          => self.cast(.dec_u).end(),
        -- .deref          => self.cast(.deref).end,
        -- // .dot_array_lit  => self.cast(.dot_array_lit).pos,
        -- .dot_lit        => self.cast(.dot_lit).name.end(),
        -- .else_block     => {
        --     const block = self.cast(.else_block);
        --     return block.stmts[block.stmts.len-1].end();
        -- },
        -- .elseif_block     => {
        --     const block = self.cast(.elseif_block);
        --     return block.stmts[block.stmts.len-1].end();
        -- },
        case .enum_decl |node|: return node.members.last().end()
        -- .enumMember     => {
        --     const member = self.cast(.enumMember);
        --     if (member.typeSpec) |type_| {
        --         return type_.end();
        --     }
        --     return member.name.end();
        -- },
        -- .error_lit      => self.cast(.error_lit).name.end(),
        case .ex_borrow |node|: return node.child.end()
        -- .expand_lit     => self.cast(.expand_lit).end,
        -- .exprStmt       => self.cast(.exprStmt).child.end(),
        -- .falseLit       => self.cast(.falseLit).pos + 5,
        case .float_lit |node|: return node.end()
        -- .for_iter_stmt  => {
        --     const stmt = self.cast(.for_iter_stmt);
        --     return stmt.stmts[stmt.stmts.len-1].end();
        -- },
        -- .for_range_stmt => {
        --     const stmt = self.cast(.for_range_stmt);
        --     return stmt.stmts[stmt.stmts.len-1].end();
        -- },
        -- .funcDecl       => {
        --     const decl = self.cast(.funcDecl);
        --     if (decl.stmts.len > 0) {
        --         return decl.stmts.ptr[decl.stmts.len-1].end();
        --     }
        --     if (decl.ret) |ret| {
        --         return ret.end();
        --     }
        --     return decl.name.end();
        -- },
        case .func_param |node|:
            if node.type |_type|:
                return _type.end()
            return node.name_type.end()

        case .fn_type |node|:
            if node.ret |ret|:
                return ret.end()
            return node.pos

        case .fnsym_type |node|:
            if node.ret |ret|:
                return ret.end()
            return node.pos

        case .generic_vector_type |node|: return node.elem.end()
        case .generic_expand |node|: return node.end
        -- .global_decl    => {
        --     const decl = self.cast(.global_decl);
        --     if (decl.right) |right| {
        --         return right.end();
        --     }
        --     return decl.typeSpec.end();
        -- },
        case .group |node|: return node.end
        case .hex_lit |node|: return node.end()
        case .ident |node|: return node.end()
        -- .if_expr        => self.cast(.if_expr).else_expr.end(),
        -- .if_stmt        => {
        --     const stmt = self.cast(.if_stmt);
        --     if (stmt.else_blocks.len > 0) {
        --         return stmt.else_blocks[stmt.else_blocks.len-1].end();
        --     }
        --     return stmt.stmts[stmt.stmts.len-1].end();
        -- },
        -- .if_unwrap_stmt => {
        --     const stmt = self.cast(.if_unwrap_stmt);
        --     if (stmt.else_blocks.len > 0) {
        --         return stmt.else_blocks[stmt.else_blocks.len-1].end();
        --     }
        --     return stmt.stmts[stmt.stmts.len-1].end();
        -- },
        -- // .impl_with      => self.cast(.impl_with).pos,
        -- .impl_decl      => self.cast(.impl_decl).trait.end(),
        -- .import_stmt    => {
        --     const import_stmt = self.cast(.import_stmt);
        --     if (import_stmt.spec) |spec| {
        --         return spec.end();
        --     }
        --     return import_stmt.name.end();
        -- },
        case .infer_param |node|: return node.name.end()
        case .index_expr |node|: return node.end
        -- .init_expr      => self.cast(.init_expr).lit.end,
        case .init_lit |node|: return node.end
        -- .keyValue       => self.cast(.keyValue).value.end(),
        -- .label_decl     => @panic("unexpected"),
        -- .lambda_cont_expr => {
        --     const stmt = self.cast(.lambda_cont_expr);
        --     return stmt.stmts[stmt.stmts.len-1].end();
        -- },
        -- .lambda_cont    => {
        --     const lambda = self.cast(.lambda_cont);
        --     return lambda.sig_end;
        -- },
        -- .lambda_expr    => self.cast(.lambda_expr).expr.end(),
        -- .lambda_multi   => {
        --     const lambda = self.cast(.lambda_multi);
        --     return lambda.stmts[lambda.stmts.len-1].end();
        -- },
        -- .move_expr      => self.cast(.move_expr).expr.end(),
        -- .namedArg       => self.cast(.namedArg).arg.end(),
        -- .noneLit        => self.cast(.noneLit).pos + 4,
        -- .octLit         => self.cast(.octLit).end(),
        -- .op_assign_stmt => self.cast(.op_assign_stmt).right.end(),
        case .option_type |node|: return node.child.end()
        -- .partial_vector_type => self.cast(.partial_vector_type).child.end(),
        -- .passStmt       => self.cast(.passStmt).pos + 4,
        -- .ptr            => self.cast(.ptr).child.end(),
        -- .range          => {
        --     const range = self.cast(.range);
        --     if (range.end) |end_| {
        --         return end_.end();
        --     }
        --     return range.op_end;
        -- },
        -- .sq_string_lit => self.cast(.sq_string_lit).end(),
        -- .sq_string_multi_lit => self.cast(.sq_string_multi_lit).end(),
        -- .raw_string_lit => self.cast(.raw_string_lit).end(),
        -- .raw_string_multi_lit => self.cast(.raw_string_multi_lit).end(),
        case .ref |node|: return node.child.end()
        -- .returnExprStmt => self.cast(.returnExprStmt).child.end(),
        -- .returnStmt     => self.cast(.returnStmt).pos + 6,
        -- .root           => {
        --     const root = self.cast(.root);
        --     if (root.stmts.len > 0) {
        --         return root.stmts.ptr[root.stmts.len-1].end();
        --     } else {
        --         return 0;
        --     }
        -- },
        -- .seqDestructure => self.cast(.seqDestructure).end,
        case .slice_type |node|: return node.child.end()
        case .span_type |node|: return node.child.end()
        -- .special_string_lit => self.cast(.special_string_lit).lit.end(),
        -- .string_multi_lit => {
        --     return self.cast(.string_multi_lit).end();
        -- },
        -- .stringt        => {
        --     const str = self.cast(.stringt);
        --     return str.parts[str.parts.len-1].end();
        -- },
        -- .stringt_multi  => {
        --     const str = self.cast(.stringt_multi);
        --     return str.parts[str.parts.len-1].end();
        -- },
        -- .stringt_part   => {
        --     const part = self.cast(.stringt_part);
        --     return part.end();
        -- },
        -- .stringt_expr   => self.cast(.stringt_expr).child.end(),
        -- .string_lit     => self.cast(.string_lit).end(),
        -- .struct_decl    => {
        --     const decl = self.cast(.struct_decl);
        --     if (decl.fields.len > 0) {
        --         const field: *Node = @ptrCast(decl.fields.ptr[decl.fields.len-1]);
        --         return field.end();
        --     } else {
        --         return decl.name.end();
        --     }
        -- },
        -- .struct_field   => {
        --     const field = self.cast(.struct_field);
        --     if (field.init) |init| {
        --         return init.end();
        --     }
        --     if (field.typeSpec) |type_| {
        --         return type_.end();
        --     }
        --     return field.name.end();
        -- },
        case .switch_expr |node|:
            return node.cases.last().end()
        -- .switch_stmt    => {
        --     const block = self.cast(.switch_stmt);
        --     return block.cases[block.cases.len-1].end();
        -- },
        case .template |node|: return node.child_decl.end()
        case .trait_decl |node|:
            return node.funcs.last().end()
        -- .trueLit        => self.cast(.trueLit).pos + 4,
        -- .tryStmt        => {
        --     const stmt = self.cast(.tryStmt);
        --     const catch_: *Node = @ptrCast(stmt.catchStmt);
        --     return catch_.end();
        -- },
        -- .type_alias_decl => self.cast(.type_alias_decl).target.end(),
        case .type_const_decl |node|:
            return node.stmts.last().end()
        case .unary_expr |node|: return node.child.end()
        -- .undef_lit      => self.cast(.undef_lit).pos + 5,
        -- .union_case     => {
        --     const case = self.cast(.union_case);
        --     return case.payload_t.end();
        -- },
        -- .unwrap         => self.cast(.unwrap).end,
        -- .unwrap_choice  => self.cast(.unwrap_choice).right.end(),
        -- // .unwrap_or      => self.cast(.unwrap_or).opt.pos(),
        -- .unwrap_or      => self.cast(.unwrap_or).default.end(),
        -- .unwrap_or_block => {
        --     const unwrap = self.cast(.unwrap_or_block);
        --     return unwrap.else_stmts[unwrap.else_stmts.len-1].end();
        -- },
        -- .unwrap_res     => self.cast(.unwrap_res).end,
        -- .unwrap_res_or  => self.cast(.unwrap_res_or).default.end(),
        -- .unwrap_res_or_block => {
        --     const unwrap = self.cast(.unwrap_res_or_block);
        --     return unwrap.else_stmts[unwrap.else_stmts.len-1].end();
        -- },
        -- .use_alias      => {
        --     const node = self.cast(.use_alias);
        --     return node.target.end();
        -- },
        -- .var_decl       => {
        --     const decl = self.cast(.var_decl);
        --     return decl.right.end();
        -- },
        case .vector_type |node|: return node.child.end()
        case .void_lit |node|: return node.pos + 1
        -- .whileInfStmt   => {
        --     const stmt = self.cast(.whileInfStmt);
        --     return stmt.stmts[stmt.stmts.len-1].end();
        -- },
        -- .whileCondStmt  => {
        --     const while_stmt = self.cast(.whileCondStmt);
        --     return while_stmt.stmts[while_stmt.stmts.len-1].end();
        -- },
        -- // .whileOptStmt   => self.cast(.whileOptStmt).pos,
        -- .while_unwrap_stmt => {
        --     const while_stmt = self.cast(.while_unwrap_stmt);
        --     return while_stmt.stmts[while_stmt.stmts.len-1].end();
        -- },
        -- .yield_stmt     => {
        --     const yield = self.cast(.yield_stmt);
        --     if (yield.child) |child| {
        --         return child.end();
        --     } else {
        --         return yield.pos + 5;
        --     }
        -- },
        else: panic("TODO end: %{meta.choice_tag(Node, $)}")

fn (&Node) pos() -> int:
    switch $.*:
        -- .null           => cy.NullId,
        -- .all            => self.cast(.all).pos,
        case .access_expr |node|: return node.left.pos()
        case .as_expr |node|: return node.pos
        -- .assign_stmt    => self.cast(.assign_stmt).left.pos(),
        case .at_lit |node|: return node.pos
        -- .attribute      => self.cast(.attribute).pos,
        case .bin_expr |node|: return node.left.pos()
        -- .binLit         => self.cast(.binLit).pos,
        case .borrow |node|: return node.pos
        -- .breakStmt      => self.cast(.breakStmt).pos,
        case .call_expr |node|: return node.callee.pos()
        case .case_stmt |node|: return node.pos
        -- .catchStmt      => self.cast(.catchStmt).pos,
        case .const_decl |node|:  return node.pos
        -- .continueStmt   => self.cast(.continueStmt).pos,
        case .cstruct_decl |node|:   return node.pos
        -- .ct_stmt        => self.cast(.ct_stmt).pos,
        case .cunion_decl |node|: return node.pos
        case .custom_type_decl |node|: return node.pos
        case .dec_lit |node|: return node.pos
        -- .dec_u          => self.cast(.dec_u).pos,
        -- .deref          => self.cast(.deref).left.pos(),
        -- .dot            => self.cast(.dot).pos,
        -- .dot_lit        => self.cast(.dot_lit).pos,
        -- .else_block     => self.cast(.else_block).pos,
        -- .elseif_block   => self.cast(.elseif_block).pos,
        case .enum_decl |node|: return node.pos
        -- .enumMember     => self.cast(.enumMember).name.pos(),
        -- .error_lit      => self.cast(.error_lit).pos,
        case .ex_borrow |node|: return node.pos
        -- .expand_lit     => self.cast(.expand_lit).pos,
        -- .exprStmt       => self.cast(.exprStmt).child.pos(),
        -- .falseLit       => self.cast(.falseLit).pos,
        -- .fixed_array_type => self.cast(.fixed_array_type).pos,
        case .float_lit |node|: return node.pos
        -- .for_iter_stmt  => self.cast(.for_iter_stmt).pos,
        -- .for_range_stmt => self.cast(.for_range_stmt).pos,
        case .func_decl |node|:   return node.pos
        case .func_param |node|:  return node.pos
        case .fn_type |node|: return node.pos
        case .fnsym_type |node|: return node.pos
        -- .generic_array_type => self.cast(.generic_array_type).pos,
        case .generic_expand |node|: return node.left.pos()
        case .generic_vector_type |node|: return node.pos
        case .global_decl |node|: return node.pos
        case .group |node|: return node.pos
        case .hex_lit |node|: return node.pos
        case .ident |node|:       return node.pos
        -- .if_expr        => self.cast(.if_expr).pos,
        -- .if_stmt        => self.cast(.if_stmt).pos,
        -- .if_unwrap_stmt => self.cast(.if_unwrap_stmt).pos,
        case .index_expr |node|: return node.left.pos()
        -- .init_expr      => self.cast(.init_expr).left.pos(),
        case .init_lit |node|: return node.pos
        -- .keyValue       => self.cast(.keyValue).key.pos(),
        -- .impl_decl      => self.cast(.impl_decl).pos,
        case .import_stmt |node|: return node.pos
        case .infer_param |node|: return node.pos
        -- .label_decl     => cy.NullId,
        -- .lambda_cont_expr => self.cast(.lambda_cont_expr).child.pos(),
        -- .lambda_cont    => self.cast(.lambda_cont).pos,
        -- .lambda_expr    => self.cast(.lambda_expr).pos,
        -- .lambda_multi   => self.cast(.lambda_multi).pos,
        -- .move_expr      => self.cast(.move_expr).pos,
        -- .namedArg       => self.cast(.namedArg).name_pos,
        -- .noneLit        => self.cast(.noneLit).pos,
        -- .octLit         => self.cast(.octLit).pos,
        -- .op_assign_stmt => self.cast(.op_assign_stmt).left.pos(),
        case .option_type |node|: return node.pos
        -- .passStmt       => self.cast(.passStmt).pos,
        -- .ptr            => self.cast(.ptr).pos,
        -- .range          => self.cast(.range).pos,
        -- .sq_string_lit => self.cast(.sq_string_lit).pos,
        -- .sq_string_multi_lit => self.cast(.sq_string_multi_lit).pos,
        case .ref |node|: return node.pos
        -- .returnExprStmt => self.cast(.returnExprStmt).pos,
        -- .returnStmt     => self.cast(.returnStmt).pos,
        -- .root           => 0,
        -- .rune_lit       => self.cast(.rune_lit).pos,
        -- .seqDestructure => self.cast(.seqDestructure).pos,
        case .slice_type |node|: return node.pos
        case .span_type |node|: return node.pos
        -- .string_lit     => self.cast(.string_lit).pos,
        -- .string_multi_lit => self.cast(.string_multi_lit).pos,
        -- .stringt        => self.cast(.stringt).pos,
        -- .stringt_multi  => self.cast(.stringt_multi).pos,
        -- .stringt_part   => self.cast(.stringt_part).pos,
        -- .stringt_expr   => self.cast(.stringt_expr).pos,
        case .struct_decl |node|:   return node.pos
        -- .struct_field   => self.cast(.struct_field).name.pos(),
        case .switch_expr |node|: return node.pos
        case .switch_stmt |node|: return node.pos
        -- .symbol_lit     => self.cast(.symbol_lit).pos,
        case .template |node|: return node.child_decl.pos()
        case .trait_decl |node|: return node.pos
        -- .trueLit        => self.cast(.trueLit).pos,
        -- .tryStmt        => self.cast(.tryStmt).pos,
        case .type_alias_decl |node|: return node.pos
        case .type_const_decl |node|: return node.pos
        case .unary_expr |node|: return node.pos
        -- .undef_lit      => self.cast(.undef_lit).pos,
        -- .unwrap         => self.cast(.unwrap).opt.pos(),
        -- .unwrap_choice  => self.cast(.unwrap_choice).left.pos(),
        -- .unwrap_or      => self.cast(.unwrap_or).opt.pos(),
        -- .unwrap_or_block => self.cast(.unwrap_or_block).opt.pos(),
        -- .unwrap_res     => self.cast(.unwrap_res).res.pos(),
        -- .unwrap_res_or  => self.cast(.unwrap_res_or).res.pos(),
        -- .unwrap_res_or_block => self.cast(.unwrap_res_or_block).res.pos(),
        -- .use_alias      => self.cast(.use_alias).pos,
        -- .var_decl       => self.cast(.var_decl).pos,
        case .vector_type |node|: return node.pos
        case .void_lit |node|: return node.pos
        -- .whileInfStmt   => self.cast(.whileInfStmt).pos,
        -- .whileCondStmt  => self.cast(.whileCondStmt).pos,
        -- .while_unwrap_stmt => self.cast(.while_unwrap_stmt).pos,
        -- .yield_stmt     => self.cast(.yield_stmt).pos,
        else: panic("TODO pos: %{meta.choice_tag(Node, $)}")

type ZNode

fn (&ZNode) type() -> Node.Tag:
    tag_ptr := as[Ptr[byte]](@unsafeCast(int, $) - 1)
    return @intToEnum(Node.Tag, tag_ptr.*)

type CaseData enum:
    case _case    CaseDataCase
    case _else    CaseDataElse

type CaseDataCase:
    conds []^Node
    capture ?^Node

type CaseDataElse:
    src i32

type CaseBodyData enum:
    case block []^Node
    case expr ^Node
    case fallthrough void

type CaseStmt:
    data CaseData
    body_data CaseBodyData
    pos i32

type ZCaseData cunion:
    case _case    ZNodePayload[CaseDataCase]
    case _else    ZNodePayload[CaseDataElse]

type ZCaseBodyData cunion:
    case block ZNodes
    case expr Ptr[ZNode]
    case fallthrough void

type ZCaseStmt:
    kind byte
    data ZCaseData
    body_data ZCaseBodyData
    body_kind byte
    pos i32

type AsExpr:
    expr ^Node
    target ?^Node
    pos i32

type CallExpr:
    callee ^Node
    args []^Node
    has_named_arg bool
    end i32

type FuncSigKind enum:
    case func
    case infer
    case method

type AttributeKind enum:
    case bind
    case extern
    case call
    case generator

type Attribute:
    kind  AttributeKind
    value ?^Node
    src   i32
    pos   i32

type ImplDecl:
    trait ^Node
    pos i32

type Unary:
    child ^Node
    op UnaryOp
    pos i32

type BinExpr:
    left ^Node
    right ^Node
    op BinaryExprOp
    op_pos i32

type BinaryExprOp enum:
    case index
    case plus
    case minus
    case star
    case pow
    case slash
    case percent
    case bitwiseAnd
    case bitwiseOr
    case bitwiseXor
    case bitwiseLeftShift
    case bitwiseRightShift
    case bang_equal
    case less
    case less_equal
    case greater
    case greater_equal
    case equal_equal
    case and_op
    case or_op
    case cast
    case range
    case reverse_range
    case dummy

type ImportStmt:
    name ^Node
    spec ?^Node
    pos  i32

type Literal:
    src   i32
    pos   i32
    value str

fn (&Literal) end() -> int:
    return $pos + $value.len()

type SwitchBlock:
    expr ^Node

    -- CaseStmt or CtStmt/CaseStmt
    cases []^Node
    pos i32

type Token:
    src i32
    pos i32

type Range:
    start ?^Node
    end ?^Node
    inc bool
    end_inclusive bool
    src i32
    pos i32
    op_end i32

type ConstDecl:
    name    ^Node
    parent  ?^Node
    attrs   []^Node
    type    ?^Node
    right   ?^Node
    hidden  bool
    pos     i32

type GlobalDecl:
    name      ^Node
    parent    ?^Node
    attrs     []^Node -- Attribute
    type      ^Node
    right     ?^Node
    hidden    bool
    pos       i32

type Ident:
    src  i32
    pos  i32
    name str

fn (&Ident) end() -> int:
    return $pos + $name.len()

type TypeConstDecl:
    name ^Node
    hidden bool
    attrs []^Node
    stmts []^Node
    pos i32

type TypeAliasDecl:
    name   ^Node
    parent ?^Node
    hidden bool
    attrs  []^Node
    target ^Node
    pos    i32

type TemplateDecl:
    -- FuncParam.
    params []^Node
    child_decl ^Node

type CustomTypeDecl:
    name ^Node
    attrs []^Node
    hidden bool
    pos i32

type CUnionDecl:
    name ^Node
    attrs []^Node
    impls []^Node
    cases []^Node
    pos i32

type StructDecl:
    name     ^Node
    attrs    []^Node
    impls    []^Node
    fields   []^Node
    num_embedded_fields i32
    is_tuple bool
    pos      i32

type EnumMember:
    name    ^Node
    payload ?^Node
    pos     i32

type EnumDecl:
    name         ^Node
    members      []^Node
    isChoiceType bool
    hidden       bool
    pos          i32

type With:
    params []^Node
    pos i32
    end i32

type FuncDecl:
    name   ^Node
    parent ?^Node
    attrs  []^Node
    with   ?^Node
    params []^Node
    ret    ?^Node
    scope_ret bool
    hidden bool
    stmts  []^Node
    sig_t  FuncSigKind
    pos    i32

type FuncParam:
    name_type ^Node
    type      ?^Node
    template_param bool
    scope_param bool
    sink_param  bool
    const_param bool
    pos i32

type AccessExpr:
    left ^Node
    right ^Node

type GenericExpand:
    left ^Node
    end i32

type Group:
    child ^Node
    pos i32
    end i32

type TraitDecl:
    name  ^Node
    attrs []^Node
    funcs []^Node
    pos   i32

type VectorType:
    child ^Node
    n ^Node
    pos i32

type FuncType:
    params []^Node
    ret ?^Node
    src i32
    pos i32
    extern ?^Node = none

type GenericVectorType:
    elem ^Node
    pos i32

type OptionType:
    child ^Node
    pos   i32

type SpanType:
    child ^Node
    pos i32

type SliceType:
    child ^Node
    pos i32

type BorrowExpr:
    child ^Node
    pos   i32

type PtrExpr:
    child ^Node
    pos   i32

type RefExpr:
    child ^Node
    pos   i32

type ExBorrow:
    child ^Node
    pos   i32

type ReturnExprStmt:
    child ^Node
    pos i32

type IndexExpr:
    left ^Node
    args []^Node
    end  i32

type InitLit:
    args []^Node
    src i32
    pos i32
    end i32

    array_like bool

type PrefixLit:
    name ^Node
    pos i32

type Root:
    stmts []^Node

type VarDecl:
    name   ^Node
    decl_t ?^Node
    right  ^Node
    pos    i32

type ZStr:
    ptr Ptr[byte]
    len int

type ZNodes:
    ptr Ptr[Ptr[ZNode]]
    len int

fn from_znodes(znodes ZNodes) -> []^Node:
    nodes := []^Node{}
    for 0..znodes.len |i|:
        nodes = nodes << from_znode(znodes.ptr[i]).?
    return nodes

fn from_znode(znode Ptr[ZNode]) -> ?^Node:
    if znode == none:
        return none

    kind := znode.type()
    if kind == .case_stmt:
        zcase := as[Ptr[ZCaseStmt]] znode

        data := CaseData._else({src=0})
        switch @intToEnum(CaseData.Tag, zcase.kind):
            case ._case:
                z := *zcase.data._case
                data = CaseData._case({
                    conds = from_znodes(z.conds),
                    capture = from_znode(z.capture),
                })
            case ._else:
                z := *zcase.data._else
                data = CaseData._else({src=z.src})

        body_data := CaseBodyData.fallthrough
        switch @intToEnum(CaseBodyData.Tag, zcase.body_kind):
            case .block:
                body_data = CaseBodyData.block(from_znodes(zcase.body_data.block))
            case .expr:
                body_data = CaseBodyData.expr(from_znode(zcase.body_data.expr).?)
            case .fallthrough:
                body_data = CaseBodyData.fallthrough

        return ^Node.case_stmt({
            data = data,
            body_data = body_data,
            pos = zcase.pos,
        })

    switch kind:
        #for meta.enum_values(Node.Tag) |Tag|:
            case Tag:
                return from_znode_auto(Tag, znode)
    -- else:
    --     eprint("unknown %{kind}")
    --     return ^Node.unknown({ tag = kind })

type ZNodePayload[T Any] const: 
    struct_t := type.info(T).!struct
    fields := [struct_t.fields.length]meta.StructField({name='', type=void, offset=0, state_offset=0})
    for 0..struct_t.fields.length |i|:
        field := struct_t.fields[i]
        ztype := void
        if field.type == []^Node:
            ztype = ZNodes
        else field.type == ?^Node:
            ztype = Ptr[ZNode]
        else field.type == ^Node:
            ztype = Ptr[ZNode]
        else field.type == str:
            ztype = ZStr
        else:
            field_t := type.info(field.type)
            switch field_t:
                case .enum:
                    ztype = byte
                else:
                    ztype = field.type

        fields[i] = {
            name = field.name,
            type = ztype,
            offset = 0,
            state_offset = 0,
        }
    return meta.new_struct(fields, {})

fn from_znode_auto(%Tag Node.Tag, znode Ptr[ZNode]) -> ^Node:
    #N := meta.CasePayload[Node, Tag]
    #if N == void:
        return ^meta.access(Node, meta.enum_name(Tag))
    #else:
        z := as[Ptr[ZNodePayload[N]]] znode

        var payload N = undef
        #struct_t := type.info(N).!struct
        #for 0..struct_t.fields.length |i|:
            begin: 
                #field := struct_t.fields[i]
                #if field.type == []^Node:
                    nodes := from_znodes(meta.access(z, field.name))
                    @ptr_init(*meta.access(payload, field.name), nodes)
                #else field.type == ?^Node:
                    node_opt := from_znode(meta.access(z, field.name))
                    @ptr_init(*meta.access(payload, field.name), node_opt)
                #else field.type == ^Node:
                    node := from_znode(meta.access(z, field.name)).?
                    @ptr_init(*meta.access(payload, field.name), node)
                #else field.type == str:
                    zstr := meta.access(z, field.name)
                    s := str(zstr.ptr[0..zstr.len])
                    @ptr_init(*meta.access(payload, field.name), s)
                #else:
                    #field_t := type.info(field.type)
                    #switch field_t:
                        #case .enum:
                            val := meta.access(z, field.name)
                            @ptr_init(*meta.access(payload, field.name), @intToEnum(field.type, val))
                        #else:
                            val := meta.access(z, field.name)
                            @ptr_init(*meta.access(payload, field.name), val)
                            -- meta.error('Unsupported: ' + field.name + ' ' + type.name(field.type))

        return ^meta.init_choice(Node, Tag, payload)

type ParseResult:
    src   str
    root  ?^Node
    comments []Comment

fn (&ParseResult) node_text(node ^Node) -> str:
    return $src[node.pos()..node.end()]

fn (&ParseResult) stmt_docs(stmt ^Node) -> ?str:
    comment_idx := binary_search_lower($comments.len(), stmt.pos(), &|pos, idx|):
        if pos < $comments[idx].pos:
            return .lt
        else pos > $comments[idx].pos:
            return .gt
        else:
            return .eq

    if comment_idx == $comments.len():
        return none

    if comment_idx > 0:
        comment_idx -= 1

    comment := $comments[comment_idx]
    comment_text := $src[comment.pos..comment.end]

    if comment_text.len() < 3 or !comment_text.starts_with('--|'):
        return none

    if !is_adjacent_stmt($src, comment.end, stmt.pos()):
        return none

    res := comment_text[3..].trim(' ')

    -- Accumulate adjacent comments.
    for comment_idx-1..>=0 |i|:
        comment = $comments[i]
        comment_text = $src[comment.pos..comment.end]
        if comment_text.len() < 3 or !comment_text.starts_with('--|'):
            break

        if !is_adjacent_stmt($src, comment.end, $comments[i+1].pos):
            break

        res = comment_text[3..].trim(' ') + ' ' + res

    return res

--| Parses Cyber source.
fn parse(src str) -> ParseResult:
    parser := new_parser()
    raw_root := parse_(parser.inner, src)
    comments := parser_comments(parser.inner)
    root := from_znode(raw_root)
    return {
        src = src,
        root = root,
        comments = comments,
    }

type Comment:
    pos int
    end int

fn parser_comments(parser Ptr[void]) -> []Comment:
    return _parser_comments(parser, type.id(RawBuffer[Comment]))

#[bind]
fn _parser_comments(parser Ptr[void], buffer_t int) -> []Comment

#[bind]
-fn parse_(parser Ptr[void], src str) -> Ptr[ZNode]:
    panic('Unsupported')

type ZParser:
    inner Ptr[void]

fn (&ZParser) @deinit():
    destroy_parser($inner)

fn new_parser() -> ZParser:
    return {inner=_new_parser()}

#[bind]
fn _new_parser() -> Ptr[void]

#[bind]
fn destroy_parser(parser Ptr[void]):
    panic('Unsupported')

--| Starts an isolated REPL session.
--| The callback `read_line(prefix string) string` is responsible for obtaining the input.
fn repl(read_line ReadLineFn) -> void:
    ctx := REPL.new()
    ctx.print_intro()
    while ctx.read(read_line) |code|:
        ctx.eval_print(code)

type Backend enum:
    case vm
    case jit
    case tcc
    case cc
    case llvm

type EvalConfig:
    single_run          bool    = false
    gen_all_debug_syms  bool    = false
    backend             Backend = .vm
    spawn_exe           bool    = false
    persist_main        bool    = false

type EvalResult:
    code  int
    val_t int
    value Value

type Value:
    ptr  Ptr[void]

-- The type ID returned is only recognized by the VM instance that created the value,
-- with the exception of predefined type IDs that is shared across VMs.
#[bind]
fn (&Value) object_type() -> int:
    panic('Unsupported')

fn (&Value) as_int() -> int:
    return (as[Ptr[int]] $ptr).*

fn (&Value) as_bool() -> bool:
    return (as[Ptr[bool]] $ptr).*

fn (&Value) as_float() -> float:
    return (as[Ptr[float]] $ptr).*

fn (&Value) as_str() -> str:
    s := as[Ptr[str]] $ptr
    return str(s.ptr[0..s.len()])

fn (&Value) to_host(val_t int) -> Object:
    switch val_t:
        case TypeBool:
            return Object(^$as_bool())
        case TypeInt:
            return Object(^$as_int())
        case TypeFloat:
            return Object(^$as_float())
        case TypeStr:
            return Object(^$as_str())
        else:
            return Object(^TypeValue{val_t=val_t, ptr=$ptr})

type TypeValue:
    val_t int
    ptr   Ptr[void]

type Thread:
    inner Ptr[void]

#[bind]
fn (&Thread) deinit_str(value Value)

type VM:
    inner Ptr[void]

--| Create an isolated VM.
#[bind]
fn VM :: @init() -> VM

#[bind]
fn (&VM) @deinit()

fn (&VM) eval(code str) -> EvalResult:
    return $eval('eval', code, {})

#[bind]
fn (&VM) eval(uri str, code str, config EvalConfig) -> EvalResult

#[bind]
fn (&VM) compile_error_summary() -> str:
    panic('Unsupported')

#[bind]
fn (&VM) panic_summary() -> str:
    panic('Unsupported')

#[bind]
fn (&VM) value_desc(val_t int, value Value) -> str:
    panic('Unsupported')

#[bind]
fn (&VM) main_thread() -> Thread:
    panic('Unsupported')

type REPL:
    vm     VM
    indent int

    -- Build multi-line input.
    input_buffer str

fn REPL :: new() -> REPL:
    ctx := REPL{
        vm = VM(),
        indent = 0,
        input_buffer = '',
    }

    -- TODO: Record inputs that successfully compiled. Can then be exported to file.

    -- Can include additional source for REPL initialization if needed.
    init_src := ''
    res := ctx.vm.eval(init_src)
    if res.code != Success:
        report := ctx.vm.compile_error_summary()
        eprint(report)
        panic('Unexpected.')
    return ctx

fn (&REPL) print_intro():
    print("%{meta.cy_full_version()} REPL")
    print("Commands: .exit")

type ReadLineFn = fn(prefix str) -> Future[str]

fn (&REPL) read(read_line ReadLineFn) -> ?str:
    while:
        prefix := $prefix()
        future := read_line(prefix)
        input := future.await()

        if input == '.exit':
            return none

        if input.ends_with(':'):
            $input_buffer += input
            $indent += 1
            continue

        if $input_buffer.len() == 0:
            return input

        if input.len() == 0:
            $indent -= 1
            if $indent > 0:
                continue
            else:
                -- Build input and submit.
                input = $input_buffer
                $input_buffer = ''
                return input
        else:
            $input_buffer += "\n"
            $input_buffer += ' '.repeat($indent * 4)
            $input_buffer += input
            continue

fn (&REPL) eval_print(code str):
    res := $vm.eval('eval', code, { persist_main=true })
    if res.code != Success:
        if res.code == ErrorCompile:
            report := $vm.compile_error_summary()
            eprint(report)
        else res.code == ErrorPanic:
            report := $vm.panic_summary()
            eprint(report)
        else:
            eprint('Unknown error')
        return

    if res.val_t != TypeVoid:
        dump := $vm.value_desc(res.val_t, res.value)
        print(dump)

fn (&REPL) prefix() -> str:
    head := if ($indent == 0) '> ' else '| '
    s := ' '.repeat($indent * 4)
    return s + head

type UnaryOp enum:
    case minus
    case lnot
    case bit_not
    case addr_of
    case dummy
