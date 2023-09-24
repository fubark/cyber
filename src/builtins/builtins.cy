@host func arrayFill(val any, n int) List
@host func asciiCode(val any) any
@host func bool(val any) boolean
@host func char(val any) any
@host func copy(val any) any
@host func errorReport() string
@host func isAlpha(val int) boolean
@host func isDigit(val int) boolean
@host func must(val any) any
@host func opaque(val any) pointer
@host func panic(err any) none
@host func parseCyber(src any) Map
@host func parseCyon(src any) any
@host func performGC() Map
@host func print(str any) none
@host func runestr(val int) string
@host func toCyon(val any) string
@host func typeid(val any) int
@host func valtag(val any) symbol
@host func typesym(val any) symbol
@host func typeof(val any) metatype