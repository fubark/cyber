--| The current cpu arch's tag name.
@host var cpu string

--| The current arch's endianness: .little, .big
@host var endian symbol

--| Standard error file descriptor.
@host var stderr any

--| Standard input file descriptor.
@host var stdin any

--| Standard output file descriptor.
@host var stdout any

--| The current operating system's tag name.
@host var system string

--| Default SIMD vector bit size.
@host var vecBitSize int

--| Attempts to access a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.
--| Return true or an error.
@host func access(path any, mode symbol) any

--| Returns the command line arguments as a list.
--| Each argument is validated and returned as a UTF-8 `string` or `rawstring` if the validation failed.
@host func args() List

--| Calls `bindLib(path, decls, {})`. 
@host func bindLib(path any, decls List) any

--| Creates an FFI binding to a dynamic library and it's symbols.
--| By default, an anonymous object is returned with the C-functions binded as the object's methods.
--| If `config` contains `genMap: true`, a `Map` is returned instead with C-functions
--| binded as function values.
@host func bindLib(path any, decls List, config Map) any

--| Returns the path of a locally cached file of `url`.
--| If no such file exists locally, it's fetched from `url`.
@host func cacheUrl(url any) any

--| Copies a file to a destination path.
@host func copyFile(srcPath any, dstPath any) any

--| Creates the directory at `path`. Returns `true` if successful.
@host func createDir(path any) any

--| Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.
@host func createFile(path any, truncate boolean) any

--| Returns a null terminated C string.
@host func cstr(s any) pointer

--| Returns the current working directory.
@host func cwd() string

--| Returns the given path with its last component removed.
@host func dirName(path any) any

--| Runs a shell command and returns the stdout/stderr.
@host func execCmd(args List) any

--| Returns the current executable's path.
@host func exePath() string

--| Exits the program with a status code.
@host func exit(status int) none

--| Fetches the contents at `url` using the HTTP GET request method.
@host func fetchUrl(url any) any

--| Frees the memory located at `ptr`.
@host func free(ptr pointer) none

--| Returns a `rawstring` from a null terminated C string.
@host func fromCstr(ptr pointer) rawstring

--| Returns an environment value by key.
@host func getEnv(key any) any

--| Returns all environment entries as a `Map`.
@host func getEnvAll() Map

--| Reads stdin until a new line is reached. This is intended to read user input from the command line.
--| For bulk reads from stdin, use `os.stdin`.
@host func getInput() any

--| Allocates `size` bytes of memory and returns a pointer.
@host func malloc(size int) pointer

--| Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01.
@host func milliTime() float

--| Invokes `openDir(path, false)`.
@host func openDir(path any) any

--| Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.
@host func openDir(path any, iterable boolean) any

--| Opens a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.
@host func openFile(path any, mode symbol) any

--| Given expected `ArgOption`s, returns a map of the options and a `rest` entry which contains the non-option arguments. |
@host func parseArgs(options List) Map

--| Reads stdin to the EOF as a `rawstring`.
@host func readAll() any

--| Reads the file contents into a `rawstring` value.
@host func readFile(path any) any

@host func readLine() any

--| Returns the absolute path of the given path.
@host func realPath(path any) any

--| Removes an empty directory at `path`. Returns `true` if successful.
@host func removeDir(path any) any

--| Removes the file at `path`. Returns `true` if successful.
@host func removeFile(path any) any

--| Sets an environment value by key.
@host func setEnv(key any, val any) none

--| Pauses the current thread for given milliseconds.
@host func sleep(ms float) none

--| Removes an environment value by key.
@host func unsetEnv(key any) none

--| Writes a string value to a file.
@host func writeFile(path any, contents any) any

@host
type File object:
  --| Closes the file handle. File ops invoked afterwards will return `error.Closed`.
  @host meth close() none
  @host meth iterator() any
  @host meth next() any

  --| Reads at most `n` bytes as a `rawstring`. `n` must be at least 1.
  --| A result with length 0 indicates the end of file was reached.
  @host meth read(n int) any

  --| Reads to the end of the file and returns the content as a `rawstring`.
  @host meth readToEnd() any

  --| Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.
  @host meth seek(n int) any

  --| Seeks the read/write position by `pos` bytes from the current position.
  @host meth seekFromCur(n int) any

  --| Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.
  @host meth seekFromEnd(n int) any

  --| Returns info about the file as a `Map`.
  @host meth stat() any

  --| Equivalent to `streamLines(4096)`.
  @host meth streamLines() any

  --| Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`.
  --| The lines returned include the new line character(s).
  --| A buffer size of `bufSize` bytes is allocated for reading.
  --| If `\r` is found at the end of the read buffer, the line is returned instead of
  --| waiting to see if the next read has a connecting `\n`.
  @host meth streamLines(bufSize int) any

  --| Writes a `string` or `rawstring` at the current file position.
  --| The number of bytes written is returned.
  @host meth write(val any) any

@host
type Dir object:
  --| Returns a new iterator over the directory entries.
  --| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
  @host meth iterator() any

  --| Returns info about the file as a `Map`.
  @host meth stat() any

  --| Returns a new iterator over the directory recursive entries.
  --| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
  @host meth walk() any

@host
type DirIterator object:
  @host meth next() any