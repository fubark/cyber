use lc 'libc'
use linux_lc 'libc.linux.cy'
use os
use c

fn file_info(fd lc.fd_t) -> !os.FileInfo:
    stat := linux_lc.statx_t{}
    if linux_lc.statx(fd, '', linux_lc.AT_EMPTY_PATH, linux_lc.STATX_TYPE || linux_lc.STATX_MODE || linux_lc.STATX_ATIME || linux_lc.STATX_MTIME || linux_lc.STATX_CTIME, *stat) != 0:
        return os.fromErrno()
    return from_statx(*stat)

fn file_info(path Ptr[byte]) -> !os.FileInfo:
    stat := linux_lc.statx_t{}
    if linux_lc.statx(linux_lc.AT_FDCWD, path, 0, linux_lc.STATX_TYPE || linux_lc.STATX_MODE || linux_lc.STATX_ATIME || linux_lc.STATX_MTIME || linux_lc.STATX_CTIME, *stat) != 0:
        return os.fromErrno()
    return from_statx(*stat)

fn from_statx(stat Ptr[linux_lc.statx_t]) -> os.FileInfo:
    mode := stat.mode && lc.linux.IFMT
    var kind os.FileKind = switch mode:
        case linux_lc.IFDIR  => .directory
        case linux_lc.IFCHR  => .character_device
        case linux_lc.IFBLK  => .block_device
        case linux_lc.IFREG  => .file
        case linux_lc.IFIFO  => .named_pipe
        case linux_lc.IFLNK  => .sym_link
        case linux_lc.IFSOCK => .unix_domain_socket
        else        => .unknown

    return {
        inode = stat.ino,
        size  = stat.size,
        mode  = stat.mode,
        kind  = kind,
        atime_sec  = stat.atime.sec,
        atime_nsec = stat.atime.nsec,
        mtime_sec  = stat.mtime.sec,
        mtime_nsec = stat.mtime.nsec,
        ctime_sec  = stat.ctime.sec,
        ctime_nsec = stat.ctime.nsec,
    }

type DirIteratorImpl:
    buf  [1024]byte
    idx  c.size_t
    end  c.size_t
    init bool

fn DirIteratorImpl :: @init() -> DirIteratorImpl:
    return {
        idx = 0,
        end = 0,
        buf = [1024]byte(0),
        init = false,
    }

fn (&DirIteratorImpl) next(dir &os.Dir) -> !?os.DirEntry:
    while:
        if !self.init:
            res := lc.lseek(dir.fd, lc.off_t(0), lc.SEEK_SET)
            if res == -1:
                return os.fromErrno()
            self.init = true
        if self.idx >= self.end:
            rc := linux_lc.getdents64(dir.fd, *self.buf[0], as self.buf.len())
            if rc == 0: return none
            if rc == -1: return os.fromErrno()
            self.idx = 0
            self.end = rc

        entry := as[Ptr[linux_lc.dirent64]] *self.buf[self.idx]
        self.idx += entry.reclen
        name := c.from_strz(*entry.name)
        if name == '.':
            continue

        if name == '..':
            continue

        var kind os.FileKind = switch entry.type:
            case linux_lc.DT_BLK  => .block_device
            case linux_lc.DT_CHR  => .character_device
            case linux_lc.DT_DIR  => .directory
            case linux_lc.DT_FIFO => .named_pipe
            case linux_lc.DT_LNK  => .sym_link
            case linux_lc.DT_REG  => .file
            case linux_lc.DT_SOCK => .unix_domain_socket
            else         => .unknown

        return os.DirEntry{
            name = name,
            kind = kind,
        }
