import a 'test_mods/missing.cy'
var b = a

--cytest: error
--CompileError: Import path does not exist: `@AbsPath(test/modules/test_mods/missing.cy)`
--
--in @AbsPath(test/modules/import_not_found_error.cy)
--