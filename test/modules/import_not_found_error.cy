import a 'test_mods/missing.cy'
var b = a

--cytest: error
--CompileError: Import path does not exist: `@AbsPath(test/modules/test_mods/missing.cy)`
--
--@AbsPath(test/modules/import_not_found_error.cy):1:11:
--import a 'test_mods/missing.cy'
--          ^
--