import os 'os'

var libPath ?String = none
if os.system == 'macos':
  -- rdynamic doesn't work atm for MacOS.
  libPath = 'test/ffi/macos_lib.dylib'
else os.system == 'windows':
  libPath = 'test/ffi/win_lib.dll'

var ffi = os.newFFI()
ffi.cfunc('testAdd', [.int, .int], .int)
my lib = ffi.bindLib(libPath)
lib.testAdd(123, '321')

--cytest: error
--panic: Can not find compatible function for `testAdd(any, int, String) any` in `BindLib1`.
--Only `func testAdd(any, int, int) int` exists for the symbol `testAdd`.
--
--main:13:1 main:
--lib.testAdd(123, '321')
--^
--