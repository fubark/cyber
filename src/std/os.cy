use io
use lc 'libc.cy'
use c
use meta
use win32 'win32.cy'

--| Contains system level functions.
--|
--| ## Platform Support:
--| - **macOS**: Full POSIX support via libc
--| - **Linux**: Full POSIX support (future)
--| - **Windows**: Win32 API support
--|
--| ## Architecture:
--| This module provides a cross-platform API for OS operations.
--| Platform-specific implementations are in separate modules:
--| - os.macos.cy: macOS-specific implementations
--| - os.windows.cy: Windows-specific implementations
--|
--| Sample usage:
--| ```cy
--| use os
--| 
--| env := os.getEnvAll()
--| for env |e|:
--|     print('%{e.key} -> %{e.value}')
--| ```

-- Conditional platform-specific imports
#[cond=meta.system() == .macos]
use macos 'os.macos.cy'

#[cond=meta.system() == .linux]
use linux 'os.linux.cy'

#[cond=meta.system() == .windows]
use windows 'os.windows.cy'

type c_int = c.c_int

--| Platform-specific file descriptor type
type FileDescriptor = switch meta.system():
    case .macos => lc.fd_t
    else => int  -- Windows stores HANDLE as int

-- ============================================================================
-- Windows Helper Functions
-- ============================================================================
-- Core Windows helper functions (fromWin32Error, utf8ToUtf16Le, utf16LeToUtf8,
-- fileKindFromAttributes) are now in os.windows_impl.cy to avoid duplication.
-- Use windows.functionName() to call them.

-- Windows file/directory helper functions
#[cond=meta.system() == .windows]
fn createFileWindows(path str, truncate bool) -> !File:
    path16 := windows.utf8ToUtf16Le(path)!
    access := win32.GENERIC_READ || win32.GENERIC_WRITE
    share := win32.FILE_SHARE_READ || win32.FILE_SHARE_WRITE
    disposition := if (truncate) win32.CREATE_ALWAYS else win32.OPEN_ALWAYS
    handle := win32.CreateFileW(path16.ptr, access, share, none, disposition, win32.FILE_ATTRIBUTE_NORMAL, none)
    -- INVALID_HANDLE_VALUE is (HANDLE)-1, check by casting to int
    if as[int] handle == -1:
        return windows.fromWin32Error()
    return File{fd=as[int] handle}

#[cond=meta.system() == .windows]
fn openFileWindows(path str) -> !File:
    path16 := windows.utf8ToUtf16Le(path)!
    share := win32.FILE_SHARE_READ || win32.FILE_SHARE_WRITE
    handle := win32.CreateFileW(path16.ptr, win32.GENERIC_READ, share, none, win32.OPEN_EXISTING, win32.FILE_ATTRIBUTE_NORMAL, none)
    -- INVALID_HANDLE_VALUE is (HANDLE)-1, check by casting to int
    if as[int] handle == -1:
        return windows.fromWin32Error()
    return File{fd=as[int] handle}

#[cond=meta.system() == .windows]
fn createDirWindows(path str) -> !void:
    path16 := windows.utf8ToUtf16Le(path)!
    if win32.CreateDirectoryW(path16.ptr, none) == win32.FALSE:
        return windows.fromWin32Error()

#[cond=meta.system() == .windows]
fn removeDirWindows(path str) -> !void:
    path16 := windows.utf8ToUtf16Le(path)!
    if win32.RemoveDirectoryW(path16.ptr) == win32.FALSE:
        return windows.fromWin32Error()

#[cond=meta.system() == .windows]
fn removeFileWindows(path str) -> !void:
    path16 := windows.utf8ToUtf16Le(path)!
    if win32.DeleteFileW(path16.ptr) == win32.FALSE:
        return windows.fromWin32Error()

#[cond=meta.system() == .windows]
fn getCwdWindows() -> !str:
    size := win32.GetCurrentDirectoryW(0, none)
    if size == 0:
        return windows.fromWin32Error()
    buf := []win32.WCHAR(size, 0)
    result := win32.GetCurrentDirectoryW(size, buf.ptr)
    if result == 0 or result > size:
        return windows.fromWin32Error()
    return windows.utf16LeToUtf8(buf)

#[cond=meta.system() == .windows]
fn setCwdWindows(path str) -> !void:
    path16 := windows.utf8ToUtf16Le(path)!
    if win32.SetCurrentDirectoryW(path16.ptr) == win32.FALSE:
        return windows.fromWin32Error()

#[cond=meta.system() == .windows]
fn getExePathWindows() -> !str:
    buf := []win32.WCHAR(32768, 0)
    size := win32.GetModuleFileNameW(none, buf.ptr, win32.DWORD(buf.len()))
    if size == 0:
        return windows.fromWin32Error()
    return windows.utf16LeToUtf8(buf)

#[cond=meta.system() == .windows]
fn accessFileWindows(path str) -> !void:
    path16 := windows.utf8ToUtf16Le(path)!
    attrs := win32.GetFileAttributesW(path16.ptr)
    if attrs == win32.DWORD(4294967295):
        return windows.fromWin32Error()

#[cond=meta.system() == .windows]
fn getEnvWindows(key str) -> ?str:
    -- Simple ASCII-only implementation using byte arrays
    -- Convert key to UTF-16LE using bytes (2 bytes per char)
    key_len := key.len()
    key_bytes := []byte((key_len + 1) * 2, 0)
    for 0..key_len |i|:
        key_bytes[i * 2] = key[i]
        key_bytes[i * 2 + 1] = 0
    -- Null terminator already set by initialization
    
    -- First call to get required buffer size
    size := win32.GetEnvironmentVariableW(as key_bytes.ptr, none, 0)
    if size == 0:
        return none  -- Variable not found
    
    -- Allocate buffer for value (2 bytes per WCHAR)
    val_bytes := []byte(size * 2, 0)
    result := win32.GetEnvironmentVariableW(as key_bytes.ptr, as val_bytes.ptr, size)
    if result == 0:
        return none
    
    -- Convert result back to UTF-8 (ASCII only)
    -- Find actual length (look for null terminator)
    var val_len int = 0
    while val_len < size and (val_bytes[val_len * 2] != 0 or val_bytes[val_len * 2 + 1] != 0):
        val_len += 1
    
    -- Extract ASCII characters
    result_bytes := []byte(val_len, 0)
    for 0..val_len |i|:
        result_bytes[i] = val_bytes[i * 2]
    return str(result_bytes)

#[cond=meta.system() == .windows]
fn getEnvAllWindows() -> ^Map[str, str]:
    env_block := win32.GetEnvironmentStringsW()
    if env_block == none:
        return ^Map[str, str]{}
    
    result := ^Map[str, str]{}
    var offset int = 0
    
    -- Environment block is double-null-terminated list of null-terminated strings
    -- Each string is in format "KEY=VALUE"
    while env_block[offset] != 0:
        -- Find end of current string (null terminator)
        var str_end int = offset
        while env_block[str_end] != 0:
            str_end += 1
        
        -- Create a slice from offset to str_end for utf16LeToUtf8
        str_len := str_end - offset
        slice := []win32.WCHAR(str_len, 0)
        for 0..str_len |i|:
            slice[i] = env_block[offset + i]
        
        -- Convert UTF-16LE to UTF-8 string
        entry := windows.utf16LeToUtf8(slice) !else:
            -- Skip invalid entries
            offset = str_end + 1
            continue
        
        -- Find '=' separator
        var eq_pos int = 0
        while eq_pos < entry.len() and entry[eq_pos] != '=':
            eq_pos += 1
        
        if eq_pos < entry.len():
            key := entry[0..eq_pos]
            val := entry[(eq_pos + 1)..]
            result[key] = val
        
        -- Move to next string (past null terminator)
        offset = str_end + 1
    
    _ = win32.FreeEnvironmentStringsW(env_block)
    return result

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
const DefaultDirMode = switch meta.system():
    case .macos => lc.mode_t(0o755)
    else => 0  -- Windows doesn't use POSIX mode_t

--| Default mode when creating a file.
const DefaultFileMode = switch meta.system():
    case .macos => lc.mode_t(0o666)
    else => 0  -- Windows doesn't use POSIX mode_t

const ns_per_us = 1000
const ns_per_ms = 1000 * ns_per_us
const ns_per_s = 1000 * ns_per_ms

--| Attempts to access a file at the given `path` with `mode`.
fn accessFile(path str, mode AccessMode) -> !void:
    #if meta.system() == .windows:
        return accessFileWindows(path)
    #else:
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
        #if meta.system() == .windows:
            src16 := windows.utf8ToUtf16Le(src_path)!
            dst16 := windows.utf8ToUtf16Le(dst_path)!
            if win32.CopyFileW(src16.ptr, dst16.ptr, win32.FALSE) == win32.FALSE:
                return windows.fromWin32Error()
        #else:
            panic('copy_file not supported on this platform')

--| Creates the directory at `path`.
fn createDir(path str) -> !void:
    #if meta.system() == .windows:
        return createDirWindows(path)
    #else:
        cpath := str.initz(path)
        if lc.mkdir(cpath.ptr, DefaultDirMode) != 0:
            return fromErrno()

fn createFile(path str) -> !File:
    return createFile(path, true)

--| Creates and opens the file at `path`. If `truncate` is true, an existing file will be truncated.
fn createFile(path str, truncate bool) -> !File:
    #if meta.system() == .windows:
        return createFileWindows(path, truncate)
    #else:
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
    #if meta.system() == .windows:
        return getCwdWindows()
    #else:
        buf := [lc.PATH_MAX]byte(0)
        if lc.getcwd(*buf[0], as buf.len()) == none:
            return fromErrno()
        return c.from_strz(*buf[0])

--| Change the current working directory.
fn chdir(path str) -> !void:
    #if meta.system() == .windows:
        return setCwdWindows(path)
    #else:
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
    #if meta.system() == .windows:
        win32.ExitProcess(r32(status))
    #else:
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
    #else meta.system() == .macos:
        cstat := lc.Stat{}
        if lc.stat(cpath.ptr, &cstat) != 0:
            return fromErrno()
        return fileInfo(&cstat)
    #else:
        #if meta.system() == .windows:
            path16 := windows.utf8ToUtf16Le(path)!
            data := win32.WIN32_FILE_ATTRIBUTE_DATA{
                dwFileAttributes=undef, ftCreationTime=undef,
                ftLastAccessTime=undef, ftLastWriteTime=undef,
                nFileSizeHigh=undef, nFileSizeLow=undef,
            }
            if win32.GetFileAttributesExW(path16.ptr, win32.DWORD(win32.GetFileExInfoStandard), &data) == win32.FALSE:
                return windows.fromWin32Error()
            return fileInfoFromWin32Attr(&data)
        #else:
            panic('fileInfo not supported on this platform')


fn fileInfo(stat Ptr[lc.Stat]) -> FileInfo:
    var res FileInfo = undef
    #if meta.system() == .macos:
        res = macos.fromStat(stat)
    #else:
        panic('fileInfo not supported on this platform')
    return res

--| Convert Windows file attributes to FileInfo.
#[cond=meta.system() == .windows]
fn fileInfoFromWin32Attr(data Ptr[win32.WIN32_FILE_ATTRIBUTE_DATA]) -> FileInfo:
    size := (as[int] data.nFileSizeHigh << 32) || as[int] data.nFileSizeLow
    is_dir := (as[int] data.dwFileAttributes && as[int] win32.FILE_ATTRIBUTE_DIRECTORY) != 0
    kind := if (is_dir) FileKind.directory else FileKind.file
    -- Convert FILETIME to Unix timestamp
    mtime_ns := fileTimeToNanos(&data.ftLastWriteTime)
    atime_ns := fileTimeToNanos(&data.ftLastAccessTime)
    ctime_ns := fileTimeToNanos(&data.ftCreationTime)
    return FileInfo{
        inode = 0,  -- Windows doesn't have inodes
        size = size,
        mode = 0,   -- Windows doesn't have Unix mode
        kind = kind,
        atime_sec = atime_ns / 1000000000,
        atime_nsec = atime_ns % 1000000000,
        mtime_sec = mtime_ns / 1000000000,
        mtime_nsec = mtime_ns % 1000000000,
        ctime_sec = ctime_ns / 1000000000,
        ctime_nsec = ctime_ns % 1000000000,
    }

--| Convert Windows FILETIME to nanoseconds since Unix epoch.
#[cond=meta.system() == .windows]
fn fileTimeToNanos(ft Ptr[win32.FILETIME]) -> int:
    intervals := (as[int] ft.dwHighDateTime << 32) || as[int] ft.dwLowDateTime
    unix_intervals := intervals - 116444736000000000
    return unix_intervals * 100

--| Convert Windows BY_HANDLE_FILE_INFORMATION to FileInfo.
#[cond=meta.system() == .windows]
fn fileInfoFromHandle(info Ptr[win32.BY_HANDLE_FILE_INFORMATION]) -> FileInfo:
    size := (as[int] info.nFileSizeHigh << 32) || as[int] info.nFileSizeLow
    is_dir := (as[int] info.dwFileAttributes && as[int] win32.FILE_ATTRIBUTE_DIRECTORY) != 0
    kind := if (is_dir) FileKind.directory else FileKind.file
    -- Convert FILETIME to Unix timestamp
    mtime_ns := fileTimeToNanos(&info.ftLastWriteTime)
    atime_ns := fileTimeToNanos(&info.ftLastAccessTime)
    ctime_ns := fileTimeToNanos(&info.ftCreationTime)
    return FileInfo{
        inode = 0,  -- Windows doesn't have inodes
        size = size,
        mode = 0,   -- Windows doesn't have Unix mode
        kind = kind,
        atime_sec = atime_ns / 1000000000,
        atime_nsec = atime_ns % 1000000000,
        mtime_sec = mtime_ns / 1000000000,
        mtime_nsec = mtime_ns % 1000000000,
        ctime_sec = ctime_ns / 1000000000,
        ctime_nsec = ctime_ns % 1000000000,
    }

--| Frees the memory located at `ptr`.
fn free(ptr Ptr[void]):
    #if meta.system() == .macos:
        lc.free(ptr)
    #else:
        #if meta.system() == .windows:
            heap := win32.GetProcessHeap()
            _ = win32.HeapFree(heap, 0, ptr)
        #else:
            panic('free not supported on this platform')

fn fromErrno() -> error:
    #if meta.system() == .macos:
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
    #else:
        return error.Unknown

--| Returns an environment variable by key.
fn get_env(key str) -> ?str:
    #if meta.system() == .macos:
        ckey := str.initz(key)
        cval := lc.getenv(ckey.ptr)
        if cval == none:
            return none
        return c.from_strz(cval)
    #else:
        #if meta.system() == .windows:
            return getEnvWindows(key)
        #else:
            panic('get_env not supported on this platform')

--| Returns all environment variables as a map.
fn getEnvAll() -> ^Map[str, str]:
    #if meta.system() == .windows:
        return getEnvAllWindows()
    #else:
        panic('getEnvAll not yet implemented on this platform')

--| Allocates `size` bytes of memory and returns a pointer.
fn malloc(size int) -> !Ptr[void]:
    #if meta.system() == .macos:
        ptr := lc.malloc(size)
        if ptr == none:
            return fromErrno()
        return ptr
    #else:
        #if meta.system() == .windows:
            heap := win32.GetProcessHeap()
            ptr := win32.HeapAlloc(heap, 0, size)
            if ptr == none:
                return windows.fromWin32Error()
            return ptr
        #else:
            panic('malloc not supported on this platform')

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
        if lc.linux.sched_getaffinity(0, type.size(lc.cpu_set_t), *cpu_set) != 0:
            return fromErrno()
        sum := 0
        for cpu_set |x|:
            --sum += @pop_count(x)
            panic('TODO: pop_count')
        return sum
    #else:
        #if meta.system() == .windows:
            info := win32.SYSTEM_INFO{
                wProcessorArchitecture=undef, wReserved=undef, dwPageSize=undef,
                lpMinimumApplicationAddress=undef, lpMaximumApplicationAddress=undef,
                dwActiveProcessorMask=undef, dwNumberOfProcessors=undef,
                dwProcessorType=undef, dwAllocationGranularity=undef,
                wProcessorLevel=undef, wProcessorRevision=undef,
            }
            win32.GetSystemInfo(&info)
            return as[int] info.dwNumberOfProcessors
        #else:
            panic('num_cpus not supported on this platform')

fn open_dir(path str) -> !Dir:
    return open_dir(path, true)

--| Opens a directory at the given `path`. `iterable` indicates that the directory's entries can be iterated.
fn open_dir(path str, iterable bool) -> !Dir:
    #if meta.system() == .macos:
        cpath := str.initz(path)
        fd := lc.open(cpath.ptr, lc.O_RDONLY || lc.O_DIRECTORY || lc.O_CLOEXEC, lc.mode_t(0))
        if fd == -1:
            return fromErrno()
        return {fd=fd}
    #else:
        #if meta.system() == .windows:
            -- Verify directory exists
            path16 := windows.utf8ToUtf16Le(path)!
            attrs := win32.GetFileAttributesW(path16.ptr)
            if attrs == win32.DWORD(4294967295):  -- INVALID_FILE_ATTRIBUTES
                return windows.fromWin32Error()
            -- On Windows, fd_t is void. We must provide fd field but it's unused.
            -- Return Dir directly (no local var) since Dir has NoCopy trait
            return Dir{fd=undef, path=path}
        #else:
            panic('open_dir not supported on this platform')

--| Returns the current time in nanoseconds since the Epoch.
fn nanoTime() -> !int:
    #if meta.system() == .macos:
        tp := lc.timespec{}
        if lc.clock_gettime(lc.macos.CLOCK_REALTIME, *tp) != 0:
            return fromErrno()
        return tp.sec * ns_per_s + tp.nsec
    #else meta.system() == .linux:
        tp := lc.timespec{}
        if lc.clock_gettime(lc.linux.CLOCK_REALTIME, *tp) != 0:
            return fromErrno() 
        return tp.sec * ns_per_s + tp.nsec
    #else:
        #if meta.system() == .windows:
            ft := win32.FILETIME{dwLowDateTime=undef, dwHighDateTime=undef}
            win32.GetSystemTimeAsFileTime(&ft)
            -- FILETIME is 100-nanosecond intervals since Jan 1, 1601
            -- Unix epoch is Jan 1, 1970 = 116444736000000000 intervals
            intervals := (as[int] ft.dwHighDateTime << 32) || as[int] ft.dwLowDateTime
            unix_intervals := intervals - 116444736000000000
            return unix_intervals * 100  -- Convert to nanoseconds
        #else:
            panic('nanoTime not supported on this platform')

--| High resolution timestamp. Returns a relative up-time in nanoseconds.
fn now() -> !int:
    #if meta.system() == .macos:
        tp := lc.timespec{}
        if lc.clock_gettime(lc.macos.CLOCK_UPTIME_RAW, *tp) != 0:
            return fromErrno()
        return tp.sec * ns_per_s + tp.nsec
    #else meta.system() == .linux:
        tp := lc.timespec{}
        if lc.clock_gettime(lc.linux.CLOCK_BOOTTIME, *tp) != 0:
            return fromErrno()
        return tp.sec * ns_per_s + tp.nsec
    #else:
        #if meta.system() == .windows:
            counter := win32.LARGE_INTEGER{QuadPart=0}
            freq := win32.LARGE_INTEGER{QuadPart=0}
            if win32.QueryPerformanceCounter(&counter) == win32.FALSE:
                return windows.fromWin32Error()
            if win32.QueryPerformanceFrequency(&freq) == win32.FALSE:
                return windows.fromWin32Error()
            -- Convert to nanoseconds avoiding overflow:
            -- Instead of (counter * ns_per_s) / freq which overflows,
            -- compute: secs * ns_per_s + (remainder * ns_per_s) / freq
            secs := counter.QuadPart / freq.QuadPart
            remainder := counter.QuadPart % freq.QuadPart
            return secs * ns_per_s + (remainder * ns_per_s) / freq.QuadPart
        #else:
            panic('now not supported on this platform')


fn open_file(path str) -> !File:
    return open_file(path, .read)

--| Opens a file at the given `path` with an `OpenMode`.
fn open_file(path str, mode OpenMode) -> !File:
    #if meta.system() == .windows:
        path16 := windows.utf8ToUtf16Le(path)!
        access := switch mode:
            case .read       => win32.GENERIC_READ
            case .write      => win32.GENERIC_WRITE
            case .read_write => win32.GENERIC_READ || win32.GENERIC_WRITE
        share := win32.FILE_SHARE_READ || win32.FILE_SHARE_WRITE
        handle := win32.CreateFileW(path16.ptr, access, share, none, win32.OPEN_EXISTING, win32.FILE_ATTRIBUTE_NORMAL, none)
        -- INVALID_HANDLE_VALUE is (HANDLE)-1, check by casting to int
        if as[int] handle == -1:
            return windows.fromWin32Error()
        return File{fd=as[int] handle}
    #else:
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
    #if meta.system() == .windows:
        return removeDirWindows(path)
    #else:
        cpath := str.initz(path)
        if lc.rmdir(cpath.ptr) != 0:
            return fromErrno()

--| Removes the file at `path`.
fn removeFile(path str) -> !void:
    #if meta.system() == .windows:
        return removeFileWindows(path)
    #else:
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
        #if meta.system() == .windows:
            return getExePathWindows()
        #else:
            panic('Unsupported')

--| Returns the absolute path of the given path.
fn resolve_path(path str) -> !str:
    #if meta.system() == .macos:
        buf := [lc.PATH_MAX]byte(0)
        pathz := str.initz(path)
        fd := lc.open(pathz.ptr, lc.O_RDONLY || lc.O_CLOEXEC || lc.O_NONBLOCK, lc.mode_t(0))
        if fd == -1:
            return fromErrno()
        file := File{fd=fd}

        if lc.fcntl(file.fd, lc.F_GETPATH, *buf[0]) != 0:
            return fromErrno()
        return c.from_strz(*buf[0])
    #else meta.system() == .linux:
        buf := [lc.PATH_MAX]byte(0)
        pathz := str.initz(path)
        fd := lc.open(pathz.ptr, lc.O_RDONLY || lc.O_CLOEXEC || lc.O_NONBLOCK, lc.mode_t(0))
        if fd == -1:
            return fromErrno()
        proc_path := str.initz('/proc/self/fd/%{fd}')
        len := lc.readlink(proc_path.ptr, *buf[0], lc.PATH_MAX)
        if len < 0:
            return fromErrno()
        return str(buf[0..len])
    #else:
        #if meta.system() == .windows:
            path16 := windows.utf8ToUtf16Le(path)!
            -- First call to get required buffer size
            size := win32.GetFullPathNameW(path16.ptr, 0, none, none)
            if size == 0:
                return windows.fromWin32Error()
            -- Allocate buffer and get full path
            buf := []win32.WCHAR(size, 0)
            result := win32.GetFullPathNameW(path16.ptr, size, buf.ptr, none)
            if result == 0:
                return windows.fromWin32Error()
            return windows.utf16LeToUtf8(buf)
        #else:
            panic('resolve_path not supported on this platform')


--| Sets an environment variable by key.
fn set_env(key str, val str) -> !void:
    #if meta.system() == .macos:
        ckey := str.initz(key)
        cval := str.initz(val)
        if lc.setenv(ckey.ptr, cval.ptr, 1) != 0:
            return fromErrno()
    #else:
        #if meta.system() == .windows:
            key16 := windows.utf8ToUtf16Le(key)!
            val16 := windows.utf8ToUtf16Le(val)!
            if win32.SetEnvironmentVariableW(key16.ptr, val16.ptr) == win32.FALSE:
                return windows.fromWin32Error()
        #else:
            panic('set_env not supported on this platform')

--| Pauses the current thread for `nsecs` nanoseconds.
fn sleep(nsecs int) -> !void:
    #if meta.system() == .macos:
        s := nsecs / ns_per_s
        ns := nsecs % ns_per_s
        req := lc.timespec{
            sec = s,
            nsec = ns,
        }
        rem := lc.timespec{}
        if lc.nanosleep(*req, *rem) != 0:
            return fromErrno()
    #else:
        #if meta.system() == .windows:
            -- Windows Sleep takes milliseconds
            ms := nsecs / 1000000
            if ms == 0 and nsecs > 0:
                ms = 1  -- Minimum 1ms for non-zero sleep
            win32.Sleep(win32.DWORD(ms))
        #else:
            panic('sleep not supported on this platform')

--| Removes an environment variable by key.
fn unset_env(key str) -> !void:
    #if meta.system() == .macos:
        ckey := str.initz(key)
        if lc.unsetenv(ckey.ptr) != 0:
            return fromErrno()
    #else:
        #if meta.system() == .windows:
            key16 := windows.utf8ToUtf16Le(key)!
            -- Setting value to none removes the variable
            _ = win32.SetEnvironmentVariableW(key16.ptr, none)
            -- Ignore errors - variable may not exist
        #else:
            panic('unset_env not supported on this platform')

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
    fd     FileDescriptor
    closed bool = false

fn (&File) @deinit():
    $close()

--| Closes the file handle. File ops invoked afterwards will return `error.Closed`.
fn (&File) close() -> void:
    if $closed:
        return

    #if meta.system() == .macos:
        if lc.close($fd) != 0:
            panic("Unexpected. %{fromErrno()}")
        $closed = true
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            if win32.CloseHandle(handle) == win32.FALSE:
                panic("Unexpected. %{windows.fromWin32Error()}")
            $closed = true
        #else:
            panic('File.close not supported on this platform')

--| Returns info about the file.
fn (&File) info() -> !FileInfo:
    #if meta.system() == .linux:
        return linux.file_info($fd)
    #else meta.system() == .macos:
        cstat := lc.Stat{}
        if lc.fstat($fd, &cstat) != 0:
            return fromErrno()
        return fileInfo(&cstat)
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            info := win32.BY_HANDLE_FILE_INFORMATION{
                dwFileAttributes=undef, ftCreationTime=undef,
                ftLastAccessTime=undef, ftLastWriteTime=undef,
                dwVolumeSerialNumber=undef, nFileSizeHigh=undef,
                nFileSizeLow=undef, nNumberOfLinks=undef,
                nFileIndexHigh=undef, nFileIndexLow=undef,
            }
            if win32.GetFileInformationByHandle(handle, &info) == win32.FALSE:
                return windows.fromWin32Error()
            return fileInfoFromHandle(&info)
        #else:
            panic('File.info not supported on this platform')

--| Returns number of bytes read. If 0 is returned, then the file has reached the end.
fn (&File) read(buf [&]byte) -> !int:
    #if meta.system() == .macos:
        read := lc.read($fd, as buf.base, buf.length)
        if read == -1:
            return fromErrno()
        return read
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            var bytes_read win32.DWORD = 0
            result := win32.ReadFile(handle, as buf.base, win32.DWORD(buf.length), &bytes_read, none)
            if result == win32.FALSE:
                return windows.fromWin32Error()
            return as[int] bytes_read
        #else:
            panic('File.read not supported on this platform')

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
    #if meta.system() == .macos:
        res := lc.lseek($fd, lc.off_t(n), lc.SEEK_CUR)
        if res == -1:
            return fromErrno()
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            var distance win32.LARGE_INTEGER = undef
            distance.QuadPart = n
            var new_pos win32.LARGE_INTEGER = undef
            result := win32.SetFilePointerEx(handle, distance, &new_pos, win32.FILE_CURRENT)
            if result == win32.FALSE:
                return windows.fromWin32Error()
        #else:
            panic('File.seek not supported on this platform')

--| Seeks the read/write position by `pos` bytes from the end. Positive `pos` is invalid.
fn (&File) seekFromEnd(n int) -> !void:
    #if meta.system() == .macos:
        res := lc.lseek($fd, lc.off_t(n), lc.SEEK_END)
        if res == -1:
            return fromErrno()
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            var distance win32.LARGE_INTEGER = undef
            distance.QuadPart = n
            var new_pos win32.LARGE_INTEGER = undef
            result := win32.SetFilePointerEx(handle, distance, &new_pos, win32.FILE_END)
            if result == win32.FALSE:
                return windows.fromWin32Error()
        #else:
            panic('File.seekFromEnd not supported on this platform')

--| Seeks the read/write position to `pos` bytes from the start. Negative `pos` is invalid.
fn (&File) seekFromStart(n int) -> !void:
    #if meta.system() == .macos:
        res := lc.lseek($fd, lc.off_t(n), lc.SEEK_SET)
        if res == -1:
            return fromErrno()
    #else:
        #if meta.system() == .windows:
            -- Validate negative position before calling Windows API
            if n < 0:
                return error.InvalidArgument
            handle := as[win32.HANDLE] $fd
            var distance win32.LARGE_INTEGER = undef
            distance.QuadPart = n
            var new_pos win32.LARGE_INTEGER = undef
            result := win32.SetFilePointerEx(handle, distance, &new_pos, win32.FILE_BEGIN)
            if result == win32.FALSE:
                return windows.fromWin32Error()
        #else:
            panic('File.seekFromStart not supported on this platform')

--| Writes a string to the current file position.
fn (&File) write(val str) -> !int:
    #if meta.system() == .macos:
        written := lc.write($fd, as val.ptr, val.len())
        if written == -1:
            return fromErrno()
        return written
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            var bytes_written win32.DWORD = 0
            result := win32.WriteFile(handle, as val.ptr, win32.DWORD(val.len()), &bytes_written, none)
            if result == win32.FALSE:
                return windows.fromWin32Error()
            return as[int] bytes_written
        #else:
            panic('File.write not supported on this platform')

--| Writes `buf` to the file and returns the number of bytes written.
fn (&File) write(buf []byte) -> !int:
    #if meta.system() == .macos:
        written := lc.write($fd, as buf.ptr, buf.len())
        if written == -1:
            return fromErrno()
        return written
    #else:
        #if meta.system() == .windows:
            handle := as[win32.HANDLE] $fd
            var bytes_written win32.DWORD = 0
            result := win32.WriteFile(handle, as buf.ptr, win32.DWORD(buf.len()), &bytes_written, none)
            if result == win32.FALSE:
                return windows.fromWin32Error()
            return as[int] bytes_written
        #else:
            panic('File.write not supported on this platform')

fn (&File) write_all(val str) -> !void:
    slice := val.ptr[0..val.len()]
    $write_all(val.as_ptr_span())!

fn (&File) write_all(as_val AsSpan[byte]) -> !void:
    val := as_val.span()
    i := 0
    while i < val.length:
        -- Create a proper []byte slice from the span
        remaining := val[i..]
        buf := []byte(remaining.length, 0)
        -- Copy bytes from span to buffer
        for 0..remaining.length |j|:
            buf[j] = remaining[j]
        i += $write(buf)!

type Dir:
    with NoCopy
    fd     lc.fd_t
    closed bool = false
    path str = ''  -- Store path for FindFirstFileW on Windows

fn (&Dir) @deinit():
    $close()

fn (&Dir) close():
    if $closed:
        return

    #if meta.system() == .macos:
        res := lc.close($fd)
        if res != 0:
            panic("Unexpected. %{fromErrno()}")
        $closed = true
    #else:
        #if meta.system() == .windows:
            -- Windows Dir doesn't use fd directly, iterator handles are closed separately
            $closed = true
        #else:
            panic('Dir.close not supported on this platform')

--| Returns info about the directory.
fn (&Dir) info() -> !FileInfo:
    #if meta.system() == .linux:
        return linux.file_info($fd)
    #else meta.system() == .macos:
        cstat := lc.Stat{}
        if lc.fstat($fd, &cstat) != 0:
            return fromErrno()
        return fileInfo(&cstat)
    #else:
        #if meta.system() == .windows:
            -- Use path to get directory info
            return fileInfo($path)
        #else:
            panic('Dir.info not supported on this platform')

--| Returns a new iterator over the directory entries.
--| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
fn (&Dir) iterator() -> DirIterator:
    var new DirIterator = undef
    #if meta.system() == .macos:
        new = {impl = macos.DirIteratorImpl()}
    #else meta.system() == .linux:
        new = {impl = linux.DirIteratorImpl()}
    #else:
        #if meta.system() == .windows:
            -- Initialize with struct literal, find_data will be filled by FindFirstFileW
            -- Note: We don't store dir_path to avoid string reference counting issues
            new = {impl = windows.DirIteratorImpl{find_data=undef}}
        #else:
            panic('DirIterator not supported on this platform')
    return new

--| Returns a new iterator over the directory recursive entries.
--| If this directory was not opened with the iterable flag, `error.NotAllowed` is returned instead.
fn (&Dir) walk() -> DirIterator:
    panic('TODO')

type DirIterator:
    impl DirIteratorImpl

--| Returns the next directory entry.
fn (&DirIterator) next(dir &Dir) -> !?DirEntry:
    #if meta.system() == .macos:
        return $impl.next(dir)
    #else:
        #if meta.system() == .windows:
            impl := as[&windows.DirIteratorImpl] &$impl
            return impl.next(dir)
        #else:
            panic('DirIterator.next not supported on this platform')


type DirIteratorImpl = switch meta.system():
    case .macos => macos.DirIteratorImpl
    case .linux => linux.DirIteratorImpl
    case .windows => windows.DirIteratorImpl
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
