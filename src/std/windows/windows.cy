use c

#c.include('<windows.h>')

type HANDLE = Ptr[void]

type DWORD = r32

type BOOL = i32

type WCHAR = r16

type LPCWSTR = Ptr[WCHAR]

type LPWSTR = Ptr[WCHAR]

type LPDWORD = Ptr[DWORD]

--| 64-bit integer for file operations.
type LARGE_INTEGER cstruct:
    QuadPart i64

--| File time structure (100-nanosecond intervals since 1601).
type FILETIME cstruct:
    dwLowDateTime  DWORD
    dwHighDateTime DWORD

--| Directory entry information from FindFirstFileW/FindNextFileW.
type WIN32_FIND_DATAW cstruct:
    dwFileAttributes DWORD
    ftCreationTime   FILETIME
    ftLastAccessTime FILETIME
    ftLastWriteTime  FILETIME
    nFileSizeHigh    DWORD
    nFileSizeLow     DWORD
    dwReserved0      DWORD
    dwReserved1      DWORD
    cFileName        [260]WCHAR
    cAlternateFileName [14]WCHAR

const TRUE = BOOL(1)

const FALSE = BOOL(0)

-- File access flags. 
const GENERIC_READ = DWORD(0x80000000)
const GENERIC_WRITE = DWORD(0x40000000)
const GENERIC_EXECUTE = DWORD(0x20000000)
const GENERIC_ALL = DWORD(0x10000000)

-- File share mode
const FILE_SHARE_READ = DWORD(0x00000001)
const FILE_SHARE_WRITE = DWORD(0x00000002)
const FILE_SHARE_DELETE = DWORD(0x00000004)

-- File creation disposition
const CREATE_NEW = DWORD(1)
const CREATE_ALWAYS = DWORD(2)

const OPEN_EXISTING = DWORD(3)
const OPEN_ALWAYS = DWORD(4)

const TRUNCATE_EXISTING = DWORD(5)

-- File attributes
const FILE_ATTRIBUTE_NORMAL = DWORD(0x00000080)
const FILE_ATTRIBUTE_DIRECTORY = DWORD(0x00000010)
const FILE_ATTRIBUTE_READONLY = DWORD(0x00000001)

--| File attribute: reparse point (symlink).
const FILE_ATTRIBUTE_REPARSE_POINT = DWORD(0x00000400)

-- File flags
const FILE_FLAG_BACKUP_SEMANTICS = DWORD(0x02000000)

-- Error codes
const ERROR_SUCCESS = DWORD(0)
const ERROR_FILE_NOT_FOUND = DWORD(2)
const ERROR_PATH_NOT_FOUND = DWORD(3)
const ERROR_ACCESS_DENIED = DWORD(5)
const ERROR_INVALID_HANDLE = DWORD(6)
const ERROR_NOT_ENOUGH_MEMORY = DWORD(8)
const ERROR_INVALID_PARAMETER = DWORD(87)
const ERROR_SHARING_VIOLATION = DWORD(32)
const ERROR_ALREADY_EXISTS = DWORD(183)

--| Error code: no more files in directory.
const ERROR_NO_MORE_FILES = DWORD(18)

--| Special handle value indicating invalid handle.
--| In Windows this is (HANDLE)-1, which is 0xFFFFFFFFFFFFFFFF on 64-bit.
--| Note: We use none (null pointer) in the code instead of this constant.
const INVALID_HANDLE_VALUE HANDLE = as r64.ones

-- Standard handles
-- Note: These are defined as (DWORD)-10, -11, -12 in Windows
-- In unsigned 32-bit, -10 = 0xFFFFFFF6, -11 = 0xFFFFFFF5, -12 = 0xFFFFFFF4
const STD_INPUT_HANDLE = DWORD(0xFFFFFFF6)
const STD_OUTPUT_HANDLE = DWORD(0xFFFFFFF5)
const STD_ERROR_HANDLE = DWORD(0xFFFFFFF4)

--| Seek origin constants for SetFilePointerEx.
const FILE_BEGIN = DWORD(0)
const FILE_CURRENT = DWORD(1)
const FILE_END = DWORD(2)

