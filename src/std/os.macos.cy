use os
use c
use lc 'libc.cy'
use meta

fn fromStat(cstat Ptr[lc.Stat]) -> os.FileInfo:
    #if meta.system() == .macos:
        cmode := cstat.mode && lc.macos.IFMT
        var kind os.FileKind = switch cmode:
            case lc.macos.IFBLK  => .block_device
            case lc.macos.IFCHR  => .character_device
            case lc.macos.IFDIR  => .directory
            case lc.macos.IFIFO  => .named_pipe
            case lc.macos.IFLNK  => .sym_link
            case lc.macos.IFREG  => .file
            case lc.macos.IFSOCK => .unix_domain_socket
            else        => .unknown

        return {
            inode = cstat.ino,
            size  = cstat.size,
            mode  = i32(cstat.mode),
            kind  = kind,
            atime_sec  = cstat.atimespec.sec,
            atime_nsec = cstat.atimespec.nsec,
            mtime_sec  = cstat.mtimespec.sec,
            mtime_nsec = cstat.mtimespec.nsec,
            ctime_sec  = cstat.ctimespec.sec,
            ctime_nsec = cstat.ctimespec.nsec,
        }
    #else:
        panic('fromStat not supported on this platform')

type DirIteratorImpl:
    idx  c.size_t
    end  c.size_t
    seek i64
    buf  [1024]byte
    init bool

fn DirIteratorImpl :: @init() -> DirIteratorImpl:
    #if meta.system() == .macos:
        return {
            idx = 0,
            end = 0,
            seek = 0,
            buf = [1024]byte(0),
            init = false,
        }
    #else:
        panic('DirIteratorImpl not supported on this platform')

fn (&DirIteratorImpl) next(dir &os.Dir) -> !?os.DirEntry:
    #if meta.system() == .macos:
        while:
            if !$init:
                res := lc.lseek(dir.fd, lc.off_t(0), lc.SEEK_SET)
                if res == -1:
                    return os.fromErrno()
                $init = true
            if $idx >= $end:
                rc := lc.macos.__getdirentries64(dir.fd, *$buf[0], as $buf.len(), *$seek)
                if rc == 0: return none
                if rc == -1: return os.fromErrno()
                $idx = 0
                $end = rc

            entry := as[Ptr[lc.macos.dirent]] *$buf[$idx]
            $idx += entry.reclen
            name := str(entry.name[0..entry.namlen])
            if name == '.':
                continue

            if name == '..':
                continue

            if entry.ino == 0:
                continue
            -- if name == '.' or name == '..' or entry.ino == 0:
            --     continue

            var kind os.FileKind = switch entry.kind:
                case lc.macos.DT_BLK  => .block_device
                case lc.macos.DT_CHR  => .character_device
                case lc.macos.DT_DIR  => .directory
                case lc.macos.DT_FIFO => .named_pipe
                case lc.macos.DT_LNK  => .sym_link
                case lc.macos.DT_REG  => .file
                case lc.macos.DT_SOCK => .unix_domain_socket
                case lc.macos.DT_WHT  => .whiteout
                else         => .unknown

            return os.DirEntry{
                kind = kind,
                name = name,
            }
    #else:
        panic('DirIteratorImpl.next not supported on this platform')
