import a 'test_mods/a.cy'
var b = a.missing

--cytest: error
--CompileError: Can not find the symbol `missing` in `@AbsPath(test/modules/test_mods/a.cy)`.
--
--@AbsPath(test/modules/import_missing_sym_error.cy):2:11:
--var b = a.missing
--          ^
--