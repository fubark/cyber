use a 'test_mods/missing.cy'
b := a

--cytest: error
--CompileError: Import path does not exist: `@AbsPath(src/test/modules/test_mods/missing.cy)`
--
--@AbsPath(src/test/modules/import_not_found_error.cy):1:7:
--use a 'test_mods/missing.cy'
--      ^~~~~~~~~~~~~~~~~~~~~~
--