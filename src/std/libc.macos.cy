use c
use os
use lc 'libc.cy'
use meta

-- `_NSGetExecutablePath`
#c.include('<mach-o/dyld.h>')

#c.include('<dirent.h>')

-- `fcopyfile`
#c.include('<copyfile.h>')

#if meta.is_vm_target():
    #c.bind_lib(none)

const COPYFILE_ACL   = copyfile_flags_t(1<<0)
const COPYFILE_STAT  = copyfile_flags_t(1<<1)
const COPYFILE_XATTR = copyfile_flags_t(1<<2)
const COPYFILE_DATA  = copyfile_flags_t(1<<3)

const DT_UNKNOWN = byte(0)
const DT_FIFO    = byte(1)
const DT_CHR     = byte(2)
const DT_DIR     = byte(4)
const DT_BLK     = byte(6)
const DT_REG     = byte(8)
const DT_LNK     = byte(10)
const DT_SOCK    = byte(12)
const DT_WHT     = byte(14)

const IFMT   = i16(0o170000)
const IFIFO  = i16(0o010000)
const IFCHR  = i16(0o020000)
const IFDIR  = i16(0o040000)
const IFBLK  = i16(0o060000)
const IFREG  = i16(0o100000)
const IFLNK  = i16(0o120000)
const IFSOCK = i16(0o140000)

#[extern]
fn fcopyfile(from lc.fd_t, dst lc.fd_t, state copyfile_state_t, flags copyfile_flags_t) -> c_int

#[extern]
fn __getdirentries64(fd lc.fd_t, buf Ptr[byte], nbytes c.size_t, basep Ptr[lc.off_t]) -> ssize_t

#[extern]
fn _NSGetExecutablePath(buf Ptr[byte], size Ptr[i32]) -> c_int

-type c_int = c.c_int

type copyfile_flags_t = i32
type copyfile_state_t = Ptr[void]

type dirent cstruct:
    ino     i64
    seekoff i64
    reclen  i16
    namlen  i16
    kind    byte
    name    [1024]byte

type gid_t = i32

type ino_t = i64

-type ssize_t = c.ssize_t

type Stat cstruct:
    dev   i32
    mode  i16
    nlink i16
    ino   ino_t
    uid   uid_t
    gid   gid_t
    rdev  i32
    atimespec     timespec
    mtimespec     timespec
    ctimespec     timespec
    birthtimespec timespec
    size    lc.off_t
    blocks  i64
    blksize i32
    flags   i32
    gen     i32
    lspare  i32
    qspare  [2]i64

type timespec cstruct:
    sec  ssize_t
    nsec ssize_t

type uid_t = i32

const CLOCK_REALTIME lc.clockid_t = 0
const CLOCK_MONOTONIC lc.clockid_t = 6
const CLOCK_MONOTONIC_RAW lc.clockid_t = 4
const CLOCK_MONOTONIC_RAW_APPROX lc.clockid_t = 5
const CLOCK_UPTIME_RAW lc.clockid_t = 8
const CLOCK_UPTIME_RAW_APPROX lc.clockid_t = 9
const CLOCK_PROCESS_CPUTIME_ID lc.clockid_t = 12
const CLOCK_THREAD_CPUTIME_ID lc.clockid_t = 16

const F_GETPATH = c_int(50)

#[extern]
fn sysctlbyname(name Ptr[byte], oldp Ptr[void], oldlenp Ptr[size_t], newp Ptr[void], newlen size_t) -> c_int
