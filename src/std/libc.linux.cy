use lc 'libc.cy'
use c

#if meta.is_vm_target():
    #c.bind_lib(none)

const CLOCK_REALTIME lc.clockid_t = 0
const CLOCK_MONOTONIC lc.clockid_t = 1
const CLOCK_BOOTTIME lc.clockid_t = 7

type dev_t = r64
type uid_t = r32
type gid_t = r32

type Stat cstruct:
    dev   dev_t
    __dev_padding r32
    __ino_truncated r32
    mode  r32
    nlink r32
    uid   uid_t
    gid   gid_t
    rdev  dev_t
    __rdev_padding r32
    size  lc.off_t
    blksize i32
    blocks  i64
    atim   timespec
    mtim   timespec
    ctim   timespec
    ino   r64

-type ssize_t = c.ssize_t

type timespec cstruct:
    sec  ssize_t
    nsec ssize_t

type statx_t cstruct:
    mask r32
    blksize r32
    attributes r64
    nlink r32
    uid uid_t
    gid gid_t
    mode r16
    __pad1 r16
    ino r64
    size r64
    blocks r64
    attributes_mask r64
    atime statx_timestamp
    btime statx_timestamp
    ctime statx_timestamp
    mtime statx_timestamp
    rdev_major r32
    rdev_minor r32
    dev_major r32
    dev_minor r32
    __pad2 [14]r64

type statx_timestamp cstruct:
    sec i64
    nsec r32
    __pad1 r32
	
#[extern]
fn statx(dirfd c.c_int, path Ptr[byte], flags c.c_int, mask c.c_uint, statxbuf Ptr[statx_t]) -> c.c_int

const STATX_TYPE c.c_uint = 0x0001
const STATX_MODE c.c_uint = 0x0002
const STATX_ATIME c.c_uint = 0x0020
const STATX_MTIME c.c_uint = 0x0040
const STATX_CTIME c.c_uint = 0x0080

const AT_FDCWD lc.fd_t = -100
const AT_EMPTY_PATH lc.fd_t = 0x1000

const IFMT   = r16(0o170000)
const IFIFO  = r16(0o010000)
const IFCHR  = r16(0o020000)
const IFDIR  = r16(0o040000)
const IFBLK  = r16(0o060000)
const IFREG  = r16(0o100000)
const IFLNK  = r16(0o120000)
const IFSOCK = r16(0o140000)

const CPU_SETSIZE = 128
type cpu_set_t = [CPU_SETSIZE / 8]c.size_t

#[extern]
fn getdents64(fd lc.fd_t, dirp Ptr[void], count c.size_t) -> c.ssize_t

type dirent64 cstruct:
    ino     r64
    off     r64
    reclen  r16
    type    byte
    name    byte

const DT_UNKNOWN = byte(0)
const DT_FIFO    = byte(1)
const DT_CHR     = byte(2)
const DT_DIR     = byte(4)
const DT_BLK     = byte(6)
const DT_REG     = byte(8)
const DT_LNK     = byte(10)
const DT_SOCK    = byte(12)

#[extern]
fn sched_getaffinity(pid lc.pid_t, cpusetsize c.size_t, mask Ptr[cpu_set_t]) -> c.c_int
