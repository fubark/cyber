@host
type boolean object:
  @host func '$call'(val any) boolean

@host
type 'error' object:
  @host func '$call'(val any) error
  @host func value(self) any

@host
type int object:
  @host func '$call'(val any) int
  @host func '$prefix~'(self) int
  @host func '$prefix-'(self) int
  @host func '$infix<'(self, o any) boolean
  @host func '$infix<='(self, o any) boolean
  @host func '$infix>'(self, o any) boolean
  @host func '$infix>='(self, o any) boolean
  @host func '$infix+'(self, o any) int
  @host func '$infix-'(self, o any) int
  @host func '$infix*'(self, o any) int
  @host func '$infix/'(self, o any) int
  @host func '$infix%'(self, o any) int
  @host func '$infix^'(self, o any) int
  @host func '$infix&'(self, o any) int
  @host func '$infix|'(self, o any) int
  @host func '$infix||'(self, o any) int
  @host func '$infix<<'(self, o any) int
  @host func '$infix>>'(self, o any) int

@host
type float object:
  @host func '$call'(val any) float
  @host func '$prefix-'(self) float
  @host func '$infix<'(self, o any) boolean
  @host func '$infix<='(self, o any) boolean
  @host func '$infix>'(self, o any) boolean
  @host func '$infix>='(self, o any) boolean
  @host func '$infix+'(self, o any) float
  @host func '$infix-'(self, o any) float
  @host func '$infix*'(self, o any) float
  @host func '$infix/'(self, o any) float
  @host func '$infix%'(self, o any) float
  @host func '$infix^'(self, o any) float

@host
type List object:
  @host func '$index'(self, idx any) any
  @host func '$setIndex'(self, idx any, val any) none
  @host func add(self, val any) none
  @host func append(self, val any) none
  @host func concat(self, list List) none
  @host func insert(self, idx int, val any) any
  @host func iterator(self) any
  @host func joinString(self, sep any) string
  @host func len(self) int
  @host func seqIterator(self) any
  @host func remove(self, idx int) any
  @host func resize(self, size int) any
  @host func sort(self, lessFn any) any

@host
type ListIterator object:
  @host func next(self) any
  @host func nextSeq(self) any

@host
type Map object:
  @host func '$index'(self, key any) any
  @host func '$setIndex'(self, key any, val any) none
  @host func remove(self, key any) none
  @host func size(self) int
  @host func iterator(self) any
  @host func seqIterator(self) any

@host
type MapIterator object:
  @host func next(self) any
  @host func nextSeq(self) any
  
-- type string trait:
--   func append(self, str any) string
--   func charAt(self, idx int) any
--   func codeAt(self, idx int) any
--   func concat(self, str any) string
--   func endsWith(self, str any) boolean
--   func find(self, str any) any
--   func findAnyRune(self, runes any) any
--   func findRune(self, rune int) any
--   func index(self, str any) any
--   func indexChar(self, ch any) any
--   func indexCharSet(self, chars any) any
--   func indexCode(self, rune int) any
--   func insert(self, idx int, str any) string
--   func isAscii(self) boolean
--   func len(self) int
--   func less(self, str any) boolean
--   func lower(self) string
--   func replace(self, needle any, replacement any) string
--   func repeat(self, n int) any
--   func runeAt(self, idx int) any
--   func slice(self, start int, end int) any
--   func $slice(self, start int, end any) any
--   func sliceAt(self, idx int) any
--   func $index(self, idx int) any
--   func split(self, sep any) List
--   func startsWith(self, str any) boolean
--   func trim(self, mode symbol, str any) any
--   func upper(self) string

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