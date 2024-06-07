use a './test_mods/stmt_error.cy'

--cytest: error
--CompileError: Top level statement is not allowed from imported module.
--
--@AbsPath(test/modules/test_mods/stmt_error.cy):1:1:
--print 123
--^
--