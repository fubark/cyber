--| The current cpu arch's tag name.
@host var .cpu string

--| The current arch's endianness: .little, .big
@host var .endian symbol

--| Standard error file descriptor.
@host var .stderr File

--| Standard input file descriptor.
@host var .stdin File

--| Standard output file descriptor.
@host var .stdout File

--| The current operating system's tag name.
@host var .system string

--| Default SIMD vector bit size.
@host var .vecBitSize int

--| Attempts to access a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.
--| Throws an error if unsuccessful.
@host fn access(path string, mode symbol) void

--| Returns the command line arguments in a `List[string]`.
@host fn args() List[string]

--| Returns the path of a locally cached file of `url`.
--| If no such file exists locally, it's fetched from `url`.
@host fn cacheUrl(url string) string

--| Copies a file to a destination path.
@host fn copyFile(srcPath string, dstPath string) void

--| Creates the directory at `path`. Returns `true` if successful.
@host fn createDir(path string) void

--| Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.
@host fn createFile(path string, truncate bool) File

--| Returns a null terminated C string.
@host fn cstr(s string) *void

--| Returns the current working directory.
@host fn cwd() string

--| Returns the given path with its last component removed.
@host fn dirName(path string) ?string

--| Runs a shell command and returns the stdout/stderr.
@host fn execCmd(args List[string]) Map

--| Returns the current executable's path.
@host fn exePath() string

--| Exits the program with a status code.
@host fn exit(status int) void

--| Fetches the contents at `url` using the HTTP GET request method.
@host fn fetchUrl(url string) string

--| Frees the memory located at `ptr`.
@host fn free(ptr *void) void

--| Returns an environment variable by key.
@host fn getEnv(key string) ?string

--| Returns all environment variables as a `Map`.
@host fn getEnvAll() Map

--| Allocates `size` bytes of memory and returns a pointer.
@host fn malloc(size int) *void

--| Return the calendar timestamp, in milliseconds, relative to UTC 1970-01-01.
--| For an high resolution timestamp, use `now()`.
@host fn milliTime() float

--| Returns a new FFI context for declaring C mappings and binding a dynamic library.
@host fn newFFI() FFI

--| Returns the current time (in high resolution seconds) since an arbitrary point in time.
@host fn now() float

--| Invokes `openDir(path, false)`.
@host fn openDir(path string) Dir

--| Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.
@host fn openDir(path string, iterable bool) Dir

--| Opens a file at the given `path` with the `.read`, `.write`, or `.readWrite` mode.
@host fn openFile(path string, mode symbol) File

--| Returns a result that can be used to query args. 
@host fn parseArgs() ArgsResult

--| Reads stdin to the EOF as a UTF-8 string.
--| To return the bytes instead, use `stdin.readAll()`.
@host fn readAll() string

--| Reads the file contents from `path` as a UTF-8 string.
--| To return the bytes instead, use `File.readAll()`.
@host fn readFile(path string) string

--| Reads stdin until a new line as a `string`. This is intended to read user input from the command line.
--| For bulk reads from stdin, use `stdin`.
@host fn readLine() string

--| Returns the absolute path of the given path.
@host fn realPath(path string) string

--| Removes an empty directory at `path`. Returns `true` if successful.
@host fn removeDir(path string) void

--| Removes the file at `path`. Returns `true` if successful.
@host fn removeFile(path string) void

--| Sets an environment variable by key.
@host fn setEnv(key string, val string) void

--| Pauses the current thread for given milliseconds.
@host fn sleep(ms float) void

--| Removes an environment variable by key.
@host fn unsetEnv(key string) void

--| Writes `contents` as a string or bytes to a file.
@host fn writeFile(path string, contents string) void

type ArgOption:
    name  string
    value string

type ArgsResult:
    opts List[ArgOption]

    -- A list that contains the non-option arguments.
    rest List[string]

    fn get(self, name string) ?string:
        for self.opts -> opt:
            if opt.name == name:
                return opt.value
        return none

    fn getAll(self, name string) List[string]:
        var res = List[string]{}
        for self.opts -> opt:
            if opt.name == name:
                res.append(opt.value)
        return res

@host
type File _:

    --| Closes the file handle. File ops invoked afterwards will return `error.Closed`.
    @host fn close(self) void
    @host fn iterator(self) File
    @host fn next(self) ?string

    --| Reads at most `n` bytes as an `Array`. `n` must be at least 1.
    --| A result with length 0 indicates the end of file was reached.
    @host fn read(self, n int) string

    --| Reads to the end of the file and returns the content as an `Array`.
    @host fn readAll(self) string

    --| Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.
    @host fn seek(self, n int) void

    --| Seeks the read/write position by `pos` bytes from the current position.
    @host fn seekFromCur(self, n int) void

    --| Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.
    @host fn seekFromEnd(self, n int) void

    --| Returns info about the file as a `Map`.
    @host fn stat(self) Map

    --| Equivalent to `streamLines(4096)`.
    @host fn streamLines(self) File

    --| Returns an iterable that streams lines ending in `\n`, `\r`, `\r\n`, or the `EOF`.
    --| The lines returned include the new line character(s).
    --| A buffer size of `bufSize` bytes is allocated for reading.
    --| If `\r` is found at the end of the read buffer, the line is returned instead of
    --| waiting to see if the next read has a connecting `\n`.
    @host='File.streamLines2'
    fn streamLines(self, bufSize int) File

    --| Writes a `string` at the current file position.
    --| The number of bytes written is returned.
    @host fn write(self, val string) int

@host
type Dir _:

    --| Returns a new iterator over the directory entries.
    --| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
    @host fn iterator(self) DirIterator

    --| Returns info about the file as a `Map`.
    @host fn stat(self) Map

    --| Returns a new iterator over the directory recursive entries.
    --| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
    @host fn walk(self) DirIterator

@host
type DirIterator _:
    @host fn next(self) ?DirEntry

type DirEntry:
    --| The path of the file or directory relative to the walker's root directory.
    path string

    --| The name of the file or directory.
    name string

    --| The type of the entry: .file | .dir | .unknown
    type symbol

@host
type FFI _:

    --| Creates an `ExternFunc` that contains a C function pointer with the given signature.
    --| The extern function is a wrapper that calls the provided user function.
    --| Once created, the extern function is retained and managed by the FFI context.
    @host fn bindCallback(self, func any, params List[dyn], ret symbol) ExternFunc

    --| Calls `bindLib(path, [:])`. 
    @host fn bindLib(self, path ?string) Map

    --| Creates a handle to a dynamic library and functions declared from `cfunc`.
    --| A map of function names to C-functions is returned.
    --| There are currently no valid options in `config`.
    @host='FFI.bindLib2'
    fn bindLib(self, path ?string, config Map) Map

    --| Returns a Cyber object's pointer. Operations on the pointer is unsafe,
    --| but it can be useful when passing it to C as an opaque pointer.
    --| The object is also retained and managed by the FFI context.
    @host fn bindObjPtr(self, obj any) *void

    --| Binds a Cyber type to a C struct.
    @host fn cbind(self, mt type, fields List[dyn]) void

    --| Declares a C function which will get binded to the library handle created from `bindLib`.
    @host fn cfunc(self, name string, params List[dyn], ret any) void

    --| Allocates memory for a C struct or primitive with the given C type specifier.
    --| A `pointer` to the allocated memory is returned.
    --| Eventually this will return a `cpointer` instead which will be more idiomatic to use.
    @host fn new(self, ctype symbol) *void

    --| Releases the object from the FFI context.
    --| External code should no longer use the object's pointer since it's not guaranteed to exist
    --| or point to the correct object.
    @host fn unbindObjPtr(self, obj any) void

@host type CArray:
    elem any
    n    any

@host type CDimArray:
    elem any
    dims any
