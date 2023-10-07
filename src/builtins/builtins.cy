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
  @host meth value() any

@host
type int object:
  --| Converts a value to an 48-bit integer.
  @host func '$call'(val any) int
  @host meth '$prefix~'() int
  @host meth '$prefix-'() int
  @host meth '$infix<'(o any) boolean
  @host meth '$infix<='(o any) boolean
  @host meth '$infix>'(o any) boolean
  @host meth '$infix>='(o any) boolean
  @host meth '$infix+'(o any) int
  @host meth '$infix-'(o any) int
  @host meth '$infix*'(o any) int
  @host meth '$infix/'(o any) int
  @host meth '$infix%'(o any) int
  @host meth '$infix^'(o any) int
  @host meth '$infix&'(o any) int
  @host meth '$infix|'(o any) int
  @host meth '$infix||'(o any) int
  @host meth '$infix<<'(o any) int
  @host meth '$infix>>'(o any) int

@host
type float object:
  --| Converts the value to a `float`. Panics if type conversion fails.
  @host func '$call'(val any) float
  @host meth '$prefix-'() float
  @host meth '$infix<'(o any) boolean
  @host meth '$infix<='(o any) boolean
  @host meth '$infix>'(o any) boolean
  @host meth '$infix>='(o any) boolean
  @host meth '$infix+'(o any) float
  @host meth '$infix-'(o any) float
  @host meth '$infix*'(o any) float
  @host meth '$infix/'(o any) float
  @host meth '$infix%'(o any) float
  @host meth '$infix^'(o any) float

@host
type List object:
  @host meth '$index'(idx any) any
  @host meth '$setIndex'(idx any, val any) none
  @host meth add(val any) none

  --| Appends a value to the end of the list.
  @host meth append(val any) none

  --| Concats the elements of another list to the end of this list.
  @host meth concat(list List) none

  --| Inserts a value at index `idx`.
  @host meth insert(idx int, val any) any

  --| Returns a new iterator over the list elements.
  @host meth iterator() any

  --| Returns a new string that joins the elements with `separator`.
  @host meth joinString(sep any) string

  --| Returns the number of elements in the list.
  @host meth len() int

  --| Returns a new sequence iterator over the list elements.
  @host meth seqIterator() any

  --| Removes an element at index `idx`.
  @host meth remove(idx int) any

  --| Resizes the list to `len` elements. If the new size is bigger, `none` values
  --| are appended to the list. If the new size is smaller, elements at the end of the list are removed.
  @host meth resize(size int) any

  --| Sorts the list with the given `less` function.
  --| If element `a` should be ordered before `b`, the function should return `true` otherwise `false`.
  @host meth sort(lessFn any) any

@host
type ListIterator object:
  @host meth next() any
  @host meth nextSeq() any

@host
type Map object:
  @host meth '$index'(key any) any
  @host meth '$setIndex'(key any, val any) none

  --| Removes the element with the given key `key`.
  @host meth remove(key any) none

  --| Returns the number of key-value pairs in the map.
  @host meth size() int

  --| Returns a new iterator over the map elements.
  @host meth iterator() any

  --| Returns a new sequence iterator over the map elements.
  @host meth seqIterator() any

@host
type MapIterator object:
  @host meth next() any
  @host meth nextSeq() any
  
-- type string trait:
--   meth append(str any) string
--   meth charAt(idx int) any
--   meth codeAt(idx int) any
--   meth concat(str any) string
--   meth endsWith(str any) boolean
--   meth find(str any) any
--   meth findAnyRune(runes any) any
--   meth findRune(rune int) any
--   meth insert(idx int, str any) string
--   meth isAscii() boolean
--   meth len() int
--   meth less(str any) boolean
--   meth lower() string
--   meth replace(needle any, replacement any) string
--   meth repeat(n int) any
--   meth runeAt(idx int) any
--   meth slice(start int, end int) any
--   meth $slice(start int, end any) any
--   meth sliceAt(idx int) any
--   meth $index(idx int) any
--   meth split(sep any) List
--   meth startsWith(str any) boolean
--   meth trim(mode symbol, str any) any
--   meth upper() string

@host
type pointer object:
  --| Converts a `int` to a `pointer` value, or casts to a `pointer`. This is usually used with FFI.
  @host func '$call'(val any) pointer
  
  --| Returns the memory address as an `int`. The value may be negative since it's
  --| bitcasted from an unsigned 48-bit integer but it retains the original pointer bits.
  @host meth value() int