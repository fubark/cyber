use os 'os'

var libPath ?string = none
if os.system == 'macos':
  -- rdynamic doesn't work atm for MacOS.
  libPath = 'test/ffi/macos_lib.dylib'
else os.system == 'windows':
  libPath = 'test/ffi/win_lib.dll'

var ffi = os.newFFI()
ffi.cfunc('testAdd', .{symbol.int, symbol.int}, symbol.int)
var lib = ffi.bindLib(libPath)
lib['testAdd'](123, '321')

--cytest: error
--panic: Incompatible call arguments `(int, string)`
--to the lambda `fn(int, int) int`.
--
--main:13:1 main:
--lib['testAdd'](123, '321')
--^
--