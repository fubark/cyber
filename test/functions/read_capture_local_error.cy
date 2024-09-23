var a = 123
fn foo():
    return a

-- Capture local from static function is not allowed.
-- TODO: Provide better error message. Since function declarations are now analyzed separately this info
--       isn't available but the parser could accumulate local var decls.

--cytest: error
--CompileError: Undeclared variable `a`.
--
--main:3:12:
--    return a
--           ^
--