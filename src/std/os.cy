--| The current cpu arch's tag name.
@host var Root.cpu string

--| The current arch's endianness: .little, .big
@host var Root.endian symbol

--| Standard error file descriptor.
@host var Root.stderr any

--| Standard input file descriptor.
@host var Root.stdin any

--| Standard output file descriptor.
@host var Root.stdout any

--| The current operating system's tag name.
@host var Root.system string

--| Default SIMD vector bit size.
@host var Root.vecBitSize int

--| Attempts to access a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.
--| Return true or an error.
@host func access(path string, mode symbol) any

--| Returns the command line arguments in a `List`.
--| Each argument is validated and returned as a UTF-8 `string` or `array` if the validation failed.
@host func args() List

--| Returns the path of a locally cached file of `url`.
--| If no such file exists locally, it's fetched from `url`.
@host func cacheUrl(url string) any

--| Copies a file to a destination path.
@host func copyFile(srcPath string, dstPath string) none

--| Creates the directory at `path`. Returns `true` if successful.
@host func createDir(path string) none

--| Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.
@host func createFile(path string, truncate bool) File

--| Returns a null terminated C string.
@host func cstr(s any) pointer

--| Returns the current working directory.
@host func cwd() string

--| Returns the given path with its last component removed.
@host func dirName(path string) any

--| Runs a shell command and returns the stdout/stderr.
@host func execCmd(args List) Map

--| Returns the current executable's path.
@host func exePath() string

--| Exits the program with a status code.
@host func exit(status int) none

--| Fetches the contents at `url` using the HTTP GET request method.
@host func fetchUrl(url string) any

--| Frees the memory located at `ptr`.
@host func free(ptr pointer) none

--| Returns an environment value by key.
@host func getEnv(key string) any

--| Returns all environment entries as a `Map`.
@host func getEnvAll() Map

--| Reads stdin to the EOF as a `string`.
@host func getAllInput() string

--| Reads stdin until a new line as a `string`. This is intended to read user input from the command line.
--| For bulk reads from stdin, use `os.stdin`.
@host func getInput() string

--| Allocates `size` bytes of memory and returns a pointer.
@host func malloc(size int) pointer

--| Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01.
@host func milliTime() float

--| Returns a new FFI context for declaring C mappings and binding a dynamic library.
@host func newFFI() FFI

--| Invokes `openDir(path, false)`.
@host func openDir(path string) any

--| Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.
@host func openDir(path string, iterable bool) Dir

--| Opens a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.
@host func openFile(path string, mode symbol) File

--| Given expected `ArgOption`s, returns a map of the options and a `rest` entry which contains the non-option arguments.
@host func parseArgs(options List) Map

--| Reads the file contents from `path` with UTF-8 encoding.
@host func readFile(path string) string

--| Returns the absolute path of the given path.
@host func realPath(path string) any

--| Removes an empty directory at `path`. Returns `true` if successful.
@host func removeDir(path string) none

--| Removes the file at `path`. Returns `true` if successful.
@host func removeFile(path string) none

--| Sets an environment value by key.
@host func setEnv(key string, val string) none

--| Pauses the current thread for given milliseconds.
@host func sleep(ms float) none

--| Removes an environment value by key.
@host func unsetEnv(key string) none

--| Writes a string value to a file.
@host func writeFile(path string, contents any) none

@host
type File object:

    --| Closes the file handle. File ops invoked afterwards will return `error.Closed`.
    @host func close() none
    @host func iterator() any
    @host func next() any

    --| Reads at most `n` bytes as an `array`. `n` must be at least 1.
    --| A result with length 0 indicates the end of file was reached.
    @host func read(n int) array

    --| Reads to the end of the file and returns the content as an `array`.
    @host func readToEnd() array

    --| Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.
    @host func seek(n int) any

    --| Seeks the read/write position by `pos` bytes from the current position.
    @host func seekFromCur(n int) any

    --| Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.
    @host func seekFromEnd(n int) any

    --| Returns info about the file as a `Map`.
    @host func stat() any

    --| Equivalent to `streamLines(4096)`.
    @host func streamLines() any

    --| Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`.
    --| The lines returned include the new line character(s).
    --| A buffer size of `bufSize` bytes is allocated for reading.
    --| If `\r` is found at the end of the read buffer, the line is returned instead of
    --| waiting to see if the next read has a connecting `\n`.
    @host func streamLines(bufSize int) any

    --| Writes a `string` or `array` at the current file position.
    --| The number of bytes written is returned.
    @host func write(val any) any

@host
type Dir object:

    --| Returns a new iterator over the directory entries.
    --| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
    @host func iterator() any

    --| Returns info about the file as a `Map`.
    @host func stat() any

    --| Returns a new iterator over the directory recursive entries.
    --| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
    @host func walk() any

@host
type DirIterator object:
    @host func next() any

@host
type FFI object:

    --| Creates an `ExternFunc` that contains a C function pointer with the given signature.
    --| The extern function is a wrapper that calls the provided user function.
    --| Once created, the extern function is retained and managed by the FFI context.
    @host func bindCallback(fn any, params List, ret symbol) ExternFunc

    --| Calls `bindLib(path, [:])`. 
    @host func bindLib(path any) any

    --| Creates a handle to a dynamic library and functions declared from `cfunc`.
    --| By default, an anonymous object is returned with the C-functions binded as the object's methods.
    --| If `config` contains `genMap: true`, a `Map` is returned instead with C-functions
    --| binded as function values.
    @host func bindLib(path any, config Map) any

    --| Returns a Cyber object's pointer. Operations on the pointer is unsafe,
    --| but it can be useful when passing it to C as an opaque pointer.
    --| The object is also retained and managed by the FFI context.
    @host func bindObjPtr(obj any) pointer

    --| Binds a Cyber type to a C struct.
    @host func cbind(mt metatype, fields List) none

    --| Declares a C function which will get binded to the library handle created from `bindLib`.
    @host func cfunc(name string, params List, ret any) none

    --| Allocates memory for a C struct or primitive with the given C type specifier.
    --| A `pointer` to the allocated memory is returned.
    --| Eventually this will return a `cpointer` instead which will be more idiomatic to use.
    @host func new(ctype symbol) pointer

    --| Releases the object from the FFI context.
    --| External code should no longer use the object's pointer since it's not guaranteed to exist
    --| or point to the correct object.
    @host func unbindObjPtr(obj any) none

type CFunc object:
    var sym
    var args
    var ret

type CStruct object:
    var fields
    var type 

type CArray object:
    var elem
    var n

type CDimArray object:
    var elem
    var dims
