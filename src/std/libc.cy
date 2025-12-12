use c
use meta

-- `fopen`, `fread`, `fwrite`, etc.
#c.include('<stdio.h>')

-- `open`, `fcntl`
#c.include('<fcntl.h>')

-- `access`, `rmdir`, `read`, `write`, `unlink`
#c.include('<unistd.h>')

-- `setenv`, `unsetenv`, `malloc`, `free`, `exit`
#c.include('<stdlib.h>')

-- `nanosleep`, `clock_gettime`
#c.include('<time.h>')

-- `sysctlbyname`
#c.include('<sys/sysctl.h>')

#if meta.is_vm_target():
    #c.bind_lib(none)

type size_t = c.size_t
type ssize_t = c.ssize_t
type c_int = c.c_int
type FILE = void

#[cond=meta.system() == .macos]
use macos 'libc.macos.cy'

#[cond=meta.system() == .linux]
use linux 'libc.linux.cy'

type clockid_t = switch meta.system():
    case .macos => i32
    case .linux => r32
    else => void

type fd_t = switch meta.system():
    case .macos => i32
    case .linux => i32
    case .wasi => i32
    else => void

type pid_t = switch meta.system():
    case .macos => i32
    case .linux => i32
    else => void

type cpu_set_t = switch meta.system():
    case .linux => linux.cpu_set_t
    else => void

type mode_t = switch meta.system():
    case .macos => i16
    case .linux => r32
    case .wasi => r32
    else => void

type O = switch meta.system():
    case .macos => i32
    case .linux => r32
    case .wasi => r32
    else => void

-- TODO: packed structs would improve ergonomics of many of these system config types.
const O_RDONLY O = switch meta.system():
    case .macos => 0x0000
    case .linux => 0x0000
    else => 0
const O_WRONLY O = switch meta.system():
    case .macos => 0x0001
    case .linux => 0x0001
    else => 0
const O_RDWR O = switch meta.system():
    case .macos => 0x0002
    case .linux => 0x0002
    else => 0
const O_NONBLOCK O = switch meta.system():
    case .macos => 0x0004
    case .linux => 0x0800
    else => 0
const O_CREAT O = switch meta.system():
    case .macos => 0x0200
    case .linux => 0x0040
    else => 0
const O_TRUNC O = switch meta.system():
    case .macos => 0x0400
    case .linux => 0x0200
    else => 0
const O_CLOEXEC O = switch meta.system():
    case .macos => 0x1000000
    case .linux => 0x80000
    else => 0
const O_DIRECTORY O = switch meta.system():
    case .macos => 0x100000
    case .linux => 0x10000
    else => 0

type off_t = switch meta.system():
    case .macos => i64
    case .linux => i64
    else => i64

const PATH_MAX = switch meta.system():
    case .macos => 1024
    case .linux => 4096
    else => 0

type Stat = switch meta.system():
    case .macos => macos.Stat
    case .linux => linux.Stat
    else => void

type wasi_timespec cstruct:
    sec  i64
    nsec isize

type timespec = switch meta.system():
    case .macos => macos.timespec
    case .linux => linux.timespec
    case .wasi => wasi_timespec
    else => void

#[extern]
global errno c_int

const EPERM  = c_int(1)
const ENOENT = c_int(2)
const EBADF  = c_int(9)
const EACCES = c_int(13)
const EFAULT = c_int(14)
const EINVAL = c_int(22)

const SEEK_SET = c_int(0)
const SEEK_CUR = c_int(1)
const SEEK_END = c_int(2)

#[extern]
fn access(pathname Ptr[byte], mode c_int) -> c_int

#[extern]
fn chdir(path Ptr[byte]) -> c_int 

#[extern]
fn clock_gettime(clockid clockid_t, tp Ptr[timespec]) -> c_int

#[extern]
fn close(fd fd_t) -> c_int

#[extern]
fn exit(status c_int) -> never

#[extern]
fn fclose(stream Ptr[FILE]) -> c_int

#[extern]
fn fcntl(fd fd_t, op c_int, arg c.variadic) -> c_int

#[extern]
fn feof(stream Ptr[FILE]) -> c_int

#[extern]
fn ferror(stream Ptr[FILE]) -> c_int

#[extern]
fn fopen(path Ptr[byte], mode Ptr[byte]) -> Ptr[FILE]

#[extern]
fn fread(ptr Ptr[void], size size_t, n size_t, stream Ptr[FILE]) -> size_t

#[extern]
fn free(ptr Ptr[void]) -> void

#[extern]
fn fstat(fd fd_t, statbuf Ptr[void]) -> c_int

#[extern]
fn fwrite(ptr Ptr[void], size size_t, n size_t, stream Ptr[FILE]) -> size_t

#[extern]
fn getcwd(buf Ptr[byte], size size_t) -> Ptr[byte]

#[extern]
fn getenv(name Ptr[byte]) -> Ptr[byte]

#[extern]
fn lseek(fd fd_t, offset off_t, whence c_int) -> off_t

#[extern]
fn malloc(size size_t) -> Ptr[void]

#[extern]
fn mkdir(pathname Ptr[byte], mode mode_t) -> c_int

#[extern]
fn nanosleep(duration Ptr[timespec], rem Ptr[timespec]) -> c_int

#[extern]
fn open(pathname Ptr[byte], flags O, mode c.variadic) -> c_int

#[extern]
fn printf(format Ptr[byte], args c.variadic) -> c_int

#[extern]
fn read(fd fd_t, buf Ptr[void], count size_t) -> ssize_t

#[extern]
fn readlink(path Ptr[byte], buf Ptr[byte], bufsiz size_t) -> ssize_t

#[extern]
fn rmdir(pathname Ptr[byte]) -> c_int

#[extern]
fn setenv(name Ptr[byte], value Ptr[byte], overwrite c_int) -> c_int

#[extern]
fn stat(pathname Ptr[byte], statbuf Ptr[Stat]) -> c_int

#[extern]
fn unlink(pathname Ptr[byte]) -> c_int

#[extern]
fn unsetenv(name Ptr[byte]) -> c_int

#[extern]
fn write(fd fd_t, buf Ptr[void], count size_t) -> ssize_t
