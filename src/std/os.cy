use io
use lc 'libc.cy'
use c
use meta

--| Contains system level functions.
--| Sample usage:
--| ```cy
--| use os
--| 
--| env := os.getEnvAll()
--| for env |e|:
--|     print('%{e.key} -> %{e.value}')
--| ```

#[cond=meta.system() == .macos]
use macos 'os.macos.cy'

#[cond=meta.system() == .linux]
use linux 'os.linux.cy'

type c_int = c.c_int

--| Default SIMD vector bit size.
#[bind] global vecBitSize int

--| Returns the standard error handle.
#[bind]
fn stderr() -> File

--| Returns the standard input handle.
#[bind]
fn stdin() -> File

--| Returns the standard output handle.
#[bind]
fn stdout() -> File

const AccessExists  = c_int(0)  -- F_OK
const AccessRead    = c_int(4)  -- R_OK
const AccessWrite   = c_int(2)  -- W_OK
const AccessExecute = c_int(1)  -- X_OK
type AccessMode = c_int

--| Default mode when creating a directory.
const DefaultDirMode = lc.mode_t(0o755)

--| Default mode when creating a file.
const DefaultFileMode = lc.mode_t(0o666)

const ns_per_us = 1000
const ns_per_ms = 1000 * ns_per_us
const ns_per_s = 1000 * ns_per_ms

--| Attempts to access a file at the given `path` with `mode`.
fn accessFile(path str, mode AccessMode) -> !void:
    cpath := str.initz(path)
    code := lc.access(cpath.ptr, mode)
    if code != 0:
        return fromErrno()

--| Returns the command line arguments.
fn args() -> []str:
    return _args(type.id(RawBuffer[byte]))

#[bind] fn _args(buffer_t int) -> []str

fn baseName(path str) -> str:
    if path.len() == 0:
        return ''
    idx := path.lastIndexOf('/') ?else: return path
    return path[idx + 1..]

--| Returns the path of a locally cached file of `url`.
--| If no such file exists locally, it's fetched from `url`.
#[bind] fn cacheUrl(url str) -> str

--| Copies a file to a destination path.
fn copy_file(src_path str, dst_path str) -> !void:
    #if meta.system() == .macos:
        src := open_file(src_path)!
        dst := createFile(dst_path)!
        if lc.macos.fcopyfile(src.fd, dst.fd, none, lc.macos.COPYFILE_DATA) != 0:
            return fromErrno()
    #else:
        panic('Unsupported')

--| Creates the directory at `path`. Returns `true` if successful.
fn createDir(path str) -> !void:
    cpath := str.initz(path)
    if lc.mkdir(cpath.ptr, DefaultDirMode) != 0:
        return fromErrno()

fn createFile(path str) -> !File:
    return createFile(path, true)

--| Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.
fn createFile(path str, truncate bool) -> !File:
    flags := lc.O_WRONLY || lc.O_CREAT || lc.O_CLOEXEC
    if truncate:
        flags ||= lc.O_TRUNC
    cpath := str.initz(path)
    fd := lc.open(cpath.ptr, flags, DefaultFileMode)
    if fd == -1:
        return fromErrno()
    return {fd=fd}

--| Returns the current working directory.
fn cwd() -> !str:
    buf := [lc.PATH_MAX]byte(0)
    if lc.getcwd(*buf[0], as buf.len()) == none:
        return fromErrno()
    return c.from_strz(*buf[0])

--| Change the current working directory.
fn chdir(path str) -> !void:
    cpath := str.initz(path)
    if lc.chdir(cpath.ptr) == -1:
        return fromErrno()

--| Returns the given path with its last component removed.
fn dirname(path str) -> ?str:
    if path.len() == 0:
        return none

    end_index := path.len() - 1
    while path[end_index] == '/':
        if end_index == 0:
            return none
        end_index -= 1

    while path[end_index] != '/':
        if end_index == 0:
            return none
        end_index -= 1

    if end_index == 0 and path[0] == '/':
        return path[0..1]

    if end_index == 0:
        return none

    return path[0..end_index]

--| Runs a shell command and returns the stdout/stderr.
#[bind]
fn exec(args []str) -> ExecResult

type ExecResult:
    out  str
    err  str
    code int

--| Exits the program with a status code.
fn exit(status int) -> never:
    lc.exit(c_int(status))

--| Fetches the contents at `url` using the HTTP GET request method.
#[bind] fn fetchUrl(url str) -> str

--| Checks whether the file at the given `path` exists.
fn fileExists(path str) -> bool:
    accessFile(path, AccessExists) !else:
        return false
    return true

fn fileInfo(path str) -> !FileInfo:
    cpath := str.initz(path)
    #if meta.system() == .linux:
        return linux.file_info(cpath.ptr)
    #else:
        cstat := lc.Stat{}
        if lc.stat(cpath.ptr, &cstat) != 0:
            return fromErrno()
        return fileInfo(&cstat)

fn fileInfo(stat Ptr[lc.Stat]) -> FileInfo:
    var res FileInfo = undef
    #if meta.system() == .macos:
        res = macos.fromStat(stat)
    #else:
        panic('unsupported')
    return res

--| Frees the memory located at `ptr`.
fn free(ptr Ptr[void]):
    lc.free(ptr)

fn fromErrno() -> error:
    switch lc.errno:
        case lc.EPERM : return error.OperationDenied
        case lc.ENOENT: return error.FileNotFound
        case lc.EFAULT: return error.BadAddress
        case lc.EBADF : return error.BadFile
        case lc.EACCES: return error.AccessDenied
        case lc.EINVAL: return error.InvalidArgument
        else:
            eprint("errno: %{lc.errno}")
            return error.Unknown

--| Returns an environment variable by key.
fn get_env(key str) -> ?str:
    ckey := str.initz(key)
    cval := lc.getenv(ckey.ptr)
    if cval == none:
        return none
    return c.from_strz(cval)

--| Returns all environment variables as a map.
fn getEnvAll() -> ^Map[str, str]:
    panic('TODO')

--| Allocates `size` bytes of memory and returns a pointer.
fn malloc(size int) -> !Ptr[void]:
    ptr := lc.malloc(size)
    if ptr == none:
        return fromErrno()
    return ptr

--| Returns the number of CPU cores on the current machine.
fn num_cpus() -> int:
    #if meta.system() == .macos:
        var count c.c_int = 0
        var count_len c.size_t = type.size(c.c_int)
        res := lc.macos.sysctlbyname('hw.logicalcpu', &count, &count_len, none, 0)
        if res == -1:
            return fromErrno()
        return count
    #else meta.system() == .linux:
        var cpu_set lc.cpu_set_t = undef
        if lc.sched_getaffinity(0, type.size(lc.cpu_set_t), *cpu_set) != 0:
            return fromErrno()
        sum := 0
        for cpu_set |x|:
            --sum += @pop_count(x)
            panic('TODO: pop_count')
        return sum
    #else:
        meta.unsupported()

fn open_dir(path str) -> !Dir:
    return open_dir(path, true)

--| Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.
fn open_dir(path str, iterable bool) -> !Dir:
    cpath := str.initz(path)
    fd := lc.open(cpath.ptr, lc.O_RDONLY || lc.O_DIRECTORY || lc.O_CLOEXEC, lc.mode_t(0))
    if fd == -1:
        return fromErrno()
    return {fd=fd}

--| Returns the current time in nanoseconds since the Epoch.
fn nanoTime() -> !int:
    tp := lc.timespec{}
    -- TODO: simplify with a custom `clockid_t` type to lc.clock_gettime(.CLOCK_REALTIME, *tp)
    #if meta.system() == .macos:
        if lc.clock_gettime(lc.macos.CLOCK_REALTIME, *tp) != 0:
            return fromErrno()
    #else meta.system() == .linux:
        if lc.clock_gettime(lc.linux.CLOCK_REALTIME, *tp) != 0:
            return fromErrno() 
    #else:
        panic('unsupported')
    return tp.sec * ns_per_s + tp.nsec

--| High resolution timestamp. Returns a relative up-time in nanoseconds.
fn now() -> !int:
    tp := lc.timespec{}
    #if meta.system() == .macos:
        if lc.clock_gettime(lc.macos.CLOCK_UPTIME_RAW, *tp) != 0:
            return fromErrno()
    #else meta.system() == .linux:
        if lc.clock_gettime(lc.linux.CLOCK_BOOTTIME, *tp) != 0:
            return fromErrno()
    #else:
        panic('unsupported')
    return tp.sec * ns_per_s + tp.nsec

fn open_file(path str) -> !File:
    return open_file(path, .read)

--| Opens a file at the given `path` with an `OpenMode`.
fn open_file(path str, mode OpenMode) -> !File:
    pathz := str.initz(path)
    access := switch mode:
        case .read       => lc.O_RDONLY
        case .write      => lc.O_WRONLY
        case .read_write => lc.O_RDWR
    fd := lc.open(pathz.ptr, access || lc.O_CLOEXEC, lc.mode_t(0))
    if fd == -1:
        return fromErrno()
    return {fd=fd}

--| Returns a result that can be used to query args. 
--| `opts`: The name of options to match excluding the hyphen prefix. eg. `-path`
fn parseArgs(opts []str) -> ArgsResult:
    cmd_args := args()

    arg_opts := []ArgOption{}
    rest := []str{}

    for 0..cmd_args.len() |i|:
        arg := cmd_args[i]
        if arg[0] == '-':
            name_pair := arg[1..]
            val := ''
            name_end := name_pair.len()
            if name_pair.index('=') |ii|:
                name_end = ii
                val = name_pair[ii+1..]
            name := name_pair[0..name_end]
            if !opts.contains(name):
                rest <<= arg
                continue

            arg_opts <<= {name=name, value=val}
        else:
            rest <<= arg

    return ArgsResult{
        opts = arg_opts,
        rest = rest,
    }

--| Reads the file contents from `path`.
fn read_file(path str) -> ![]byte:
    file := open_file(path)!
    return file.read_all()

fn read_file_str(path str) -> !str:
    return str(read_file(path)!)

--| Removes an empty directory at `path`.
fn removeDir(path str) -> !void:
    cpath := str.initz(path)
    if lc.rmdir(cpath.ptr) != 0:
        return fromErrno()

--| Removes the file at `path`.
fn removeFile(path str) -> !void:
    cpath := str.initz(path)
    if lc.unlink(cpath.ptr) != 0:
        return fromErrno()

--| Returns the current executable's path.
fn resolve_exe_path() -> !str:
    #if meta.system() == .macos:
        buf := [lc.PATH_MAX]byte(0)
        len := i32(buf.len())
        if lc.macos._NSGetExecutablePath(*buf[0], *len) != 0:
            return fromErrno()
        path := c.from_strz(*buf[0])
        return resolve_path(path)
    #else:
        panic('Unsupported')

--| Returns the absolute path of the given path.
fn resolve_path(path str) -> !str:
    buf := [lc.PATH_MAX]byte(0)
    pathz := str.initz(path)
    fd := lc.open(pathz.ptr, lc.O_RDONLY || lc.O_CLOEXEC || lc.O_NONBLOCK, lc.mode_t(0))
    if fd == -1:
        return fromErrno()
    file := File{fd=fd}

    #if meta.system() == .macos:
        if lc.fcntl(file.fd, lc.F_GETPATH, *buf[0]) != 0:
            return fromErrno()
        return c.from_strz(*buf[0])
    #else meta.system() == .linux:
        proc_path := str.initz('/proc/self/fd/%{fd}')
        len := lc.readlink(proc_path.ptr, *buf[0], lc.PATH_MAX)
        if len < 0:
            return fromErrno()
        return str(buf[0..len])
    #else:
        panic('unsupported')

--| Sets an environment variable by key.
fn set_env(key str, val str) -> !void:
    ckey := str.initz(key)
    cval := str.initz(val)
    if lc.setenv(ckey.ptr, cval.ptr, 1) != 0:
        return fromErrno()

--| Pauses the current thread for `nsecs` nanoseconds.
fn sleep(nsecs int) -> !void:
    s := nsecs / ns_per_s
    ns := nsecs % ns_per_s
    req := lc.timespec{
        sec = s,
        nsec = ns,
    }
    rem := lc.timespec{}
    if lc.nanosleep(*req, *rem) != 0:
        return fromErrno()

--| Removes an environment variable by key.
fn unset_env(key str) -> !void:
    ckey := str.initz(key)
    if lc.unsetenv(ckey.ptr) != 0:
        return fromErrno()

--| Writes `contents` as a string or bytes to a file.
fn write_file(path str, contents str) -> !void:
    file := createFile(path)!
    file.write_all(contents)!

fn write_file(path str, contents []byte) -> !void:
    file := createFile(path)!
    file.write_all(contents)!

type ArgOption:
    name  str
    value str

type ArgsResult:
    opts []ArgOption

    -- `args` that contains the non-option arguments.
    rest []str

fn (&ArgsResult) at(name str) -> str:
    return $get(name) ?else panic("Missing option `%{name}`.")

fn (&ArgsResult) contains(name str) -> bool:
    return $get(name) != none

fn (&ArgsResult) get(name str) -> ?str:
    for $opts |opt|:
        if opt.name == name:
            return opt.value
    return none

fn (&ArgsResult) getAll(name str) -> []str:
    res := []str{}
    for $opts |opt|:
        if opt.name == name:
            res <<= opt.value
    return res
    
type File:
    with io.Reader
    with io.Writer
    with NoCopy
    fd     lc.fd_t
    closed bool = false

fn (&File) @deinit():
    $close()

--| Closes the file handle. File ops invoked afterwards will return `error.Closed`.
fn (&File) close() -> void:
    if $closed:
        return

    if lc.close($fd) != 0:
        panic("Unexpected. %{fromErrno()}")
    $closed = true

--| Returns info about the file.
fn (&File) info() -> !FileInfo:
    cstat := lc.Stat{}
    #if meta.system() == .linux:
        return linux.file_info($fd)
    #else:
        if lc.fstat($fd, &cstat) != 0:
            return fromErrno()
        return fileInfo(&cstat)

--| Returns number of bytes read. If 0 is returned, then the file has reached the end.
fn (&File) read(buf [&]byte) -> !int:
    read := lc.read($fd, as buf.base, buf.length)
    if read == -1:
        return fromErrno()
    return read

fn (&File) read(bytes int) -> ![]byte:
    buf := []byte(bytes, 0)
    read := $read(buf.span())!
    buf = buf.size_down(read)
    return buf

--| Reads to the end of the file.
fn (&File) read_all() -> ![]byte:
    res := []byte{}
    buf := [4096]byte(0)
    while:
        read := $read_fill(buf[..])!
        res <<= buf[0..read]
        if read < buf.len():
            break
    return res

--| Returns number of bytes read to the buffer `to`.
--| If read bytes is less than `to.len()`, the end of file was reached.
fn (&File) read_fill(as_buf AsSpan[byte]) -> !int:
    buf := as_buf.span()
    i := 0
    while i < buf.len():
        read := $read(buf[i..])!
        if read == 0:
            break
        i += read
    return i

--| Reads until a new line as a `str`.
--| For bulk line reading, consider using `Scanner.nextLine`.
fn (&File) readLine() -> !str:
    return error.TODO
    -- const input = try std.io.getStdIn().reader().readUntilDelimiterAlloc(vm.alloc, '\n', 10e8);
    -- defer vm.alloc.free(input);
    -- // TODO: Use allocOwnedString
    -- return vm.heap.newStr(input);

--| Seeks the read/write position by `pos` bytes from the current position.
fn (&File) seek(n int) -> !void:
    res := lc.lseek($fd, lc.off_t(n), lc.SEEK_CUR)
    if res == -1:
        return fromErrno()

--| Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.
fn (&File) seekFromEnd(n int) -> !void:
    res := lc.lseek($fd, lc.off_t(n), lc.SEEK_END)
    if res == -1:
        return fromErrno()

--| Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.
fn (&File) seekFromStart(n int) -> !void:
    res := lc.lseek($fd, lc.off_t(n), lc.SEEK_SET)
    if res == -1:
        return fromErrno()

--| Writes a string to the current file position.
fn (&File) write(val str) -> !int:
    return $write(val.as_ptr_span().span())!

--| Writes `bytes` to the file and returns number of bytes written.
fn (&File) write(buf [&]byte) -> !int:
    written := lc.write($fd, as buf.base, buf.length)
    if written == -1:
        return fromErrno()
    return as[int] written

fn (&File) write_all(val str) -> !void:
    slice := val.ptr[0..val.len()]
    $write_all(val.as_ptr_span())!

fn (&File) write_all(as_val AsSpan[byte]) -> !void:
    val := as_val.span()
    i := 0
    while i < val.length:
        i += $write(val[i..])!

type Dir:
    with NoCopy
    fd     lc.fd_t
    closed bool = false

fn (&Dir) @deinit():
    $close()

fn (&Dir) close():
    if $closed:
        return

    res := lc.close($fd)
    if res != 0:
        panic("Unexpected. %{fromErrno()}")
    $closed = true

--| Returns info about the directory.
fn (&Dir) info() -> !FileInfo:
    cstat := lc.Stat{}

    #if meta.system() == .linux:
        return linux.file_info($fd)
    #else:
        if lc.fstat($fd, &cstat) != 0:
            return fromErrno()
        return fileInfo(&cstat)

--| Returns a new iterator over the directory entries.
--| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
fn (&Dir) iterator() -> DirIterator:
    var new DirIterator = undef
    #if meta.system() == .macos:
        new = {impl = macos.DirIteratorImpl()}
    #else meta.system() == .linux:
        new = {impl = linux.DirIteratorImpl()}
    #else:
        meta.unsupported()
    return new

--| Returns a new iterator over the directory recursive entries.
--| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
fn (&Dir) walk() -> DirIterator:
    panic('TODO')

type DirIterator:
    impl DirIteratorImpl

fn (&DirIterator) next(dir &Dir) -> !?DirEntry:
    return $impl.next(dir)

type DirIteratorImpl = switch meta.system():
    case .macos => macos.DirIteratorImpl
    case .linux => linux.DirIteratorImpl
    else => void

type DirEntry:
    kind FileKind
    name str

type DynLib

--| Creates a handle to a dynamic library.
#[bind] fn openLib(path ?str) -> DynLib:
    panic('TODO')

-- #[bind] fn FFI.bindLibTcc(self, path ?str) -> TccState

type FileInfo:
    inode int
    size  int
    mode  i32
    kind  FileKind

    -- Last access time since UNIX epoch.
    atime_sec  int
    atime_nsec int

    -- Last modification time since UNIX epoch.
    mtime_sec  int
    mtime_nsec int

    -- Last status/metadata change time since UNIX epoch. 
    ctime_sec  int
    ctime_nsec int

type FileKind enum:
    case block_device
    case character_device
    case directory
    case named_pipe
    case sym_link
    case file
    case unix_domain_socket
    case whiteout
    case door
    case event_port
    case unknown

type OpenMode enum:
    case read
    case write
    case read_write
