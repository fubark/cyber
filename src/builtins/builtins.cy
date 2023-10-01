--| Creates a list with initial capacity of `n` and values set to `val`.
--| If the value is an object, it is shallow copied `n` times.
@host func arrayFill(val any, n int) List

--| Copies a primitive value or creates a shallow copy of an object value.
@host func copy(val any) any

--| Prints the result of `toCyon` on a value.
@host func dump(val any) string

@host func errorReport() string

--| Returns whether a rune is an alphabetic letter.
@host func isAlpha(val int) boolean

--| Returns whether a rune is a digit.
@host func isDigit(val int) boolean

--| If `val` is an error, `panic(val)` is invoked. Otherwise, `val` is returned.
@host func must(val any) any

--| Stop execution in the current fiber and starts unwinding the call stack. See [Unexpected Errors]({{<relref "/docs/toc/errors#unexpected-errors">}}).
@host func panic(err any) none

--| Parses Cyber source string into structured map object.
--| Currently, only metadata about static declarations is made available but this will be extended to include an AST.
@host func parseCyber(src any) Map

--| Parses a CYON string into a value.
@host func parseCyon(src any) any

--| Runs the garbage collector once to detect reference cycles and abandoned objects.
--| Returns the statistics of the run in a map value.
@host func performGC() Map

--| Prints a value. The host determines how it is printed.
@host func print(str any) none

--| Converts a rune to a string.
@host func runestr(val int) string

--| Encodes a value to CYON string.
@host func toCyon(val any) string

--| Returns the value's type as a `metatype` object.
@host func typeof(val any) metatype

--| Returns the value's type as one of the predefined symbols:
--| .float, .int, .boolean, .object, .list, .map, .string, .rawstring, .function, .fiber, .pointer, .symbol, .metatype, .none, .error
@host func typesym(val any) symbol

@host
type boolean object:
  --| Converts a value to either `true` or `false`.
  @host func '$call'(val any) boolean

@host
type 'error' object:
  --| Create an error from an enum or symbol.
  @host func '$call'(val any) error
  @host func value(self) any

@host
type int object:
  --| Converts a value to an 48-bit integer.
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
  --| Converts the value to a `float`. Panics if type conversion fails.
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

  --| Appends a value to the end of the list.
  @host func append(self, val any) none

  --| Concats the elements of another list to the end of this list.
  @host func concat(self, list List) none

  --| Inserts a value at index `idx`.
  @host func insert(self, idx int, val any) any

  --| Returns a new iterator over the list elements.
  @host func iterator(self) any

  --| Returns a new string that joins the elements with `separator`.
  @host func joinString(self, sep any) string

  --| Returns the number of elements in the list.
  @host func len(self) int

  --| Returns a new sequence iterator over the list elements.
  @host func seqIterator(self) any

  --| Removes an element at index `idx`.
  @host func remove(self, idx int) any

  --| Resizes the list to `len` elements. If the new size is bigger, `none` values
  --| are appended to the list. If the new size is smaller, elements at the end of the list are removed.
  @host func resize(self, size int) any

  --| Sorts the list with the given `less` function.
  --| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
  @host func sort(self, lessFn any) any

@host
type ListIterator object:
  @host func next(self) any
  @host func nextSeq(self) any

@host
type Map object:
  @host func '$index'(self, key any) any
  @host func '$setIndex'(self, key any, val any) none

  --| Removes the element with the given key `key`.
  @host func remove(self, key any) none

  --| Returns the number of key-value pairs in the map.
  @host func size(self) int

  --| Returns a new iterator over the map elements.
  @host func iterator(self) any

  --| Returns a new sequence iterator over the map elements.
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

@host
type pointer object:
  --| Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI.
  @host func '$call'(val any) pointer
  
  --| Returns the memory address as an `int`. The value may be negative since it's
  --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
  @host func 'value'(self) int