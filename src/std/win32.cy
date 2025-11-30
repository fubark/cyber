use c
use meta

--| Windows Win32 API bindings - Windows-specific system calls.
--|
--| ## Platform Support:
--| - **Windows**: Full Win32 API support (kernel32.dll)
--| - **macOS/Linux**: Types only (no function implementations)
--|
--| ## Architecture:
--| This module provides bindings to Windows Win32 API functions from kernel32.dll.
--| It follows the same pattern as libc.cy but for Windows-specific APIs.
--|
--| ## Key Differences from POSIX:
--| - String encoding: UTF-16LE (WCHAR*) instead of UTF-8 (char*)
--| - File handles: HANDLE (opaque pointer) instead of int fd
--| - Error reporting: GetLastError() (DWORD) instead of errno (int)
--| - API naming: CreateFileW, ReadFile, WriteFile (Win32 convention)
--|
--| ## Usage:
--| For cross-platform operations, use the `os` module instead.
--| This module is for Windows-specific low-level programming.

-- Windows header includes (only on Windows platform)
#if meta.system() == .windows:
    #c.include('<windows.h>')

-- Library binding: kernel32.dll on Windows, none on other platforms
-- Uses inline conditional to avoid nested #if blocks
#if meta.is_vm_target():
    #c.bind_lib(if (meta.system() == .windows) 'kernel32.dll' else none)

-- Basic Windows types (available on all platforms for type introspection)
type HANDLE = switch meta.system():
    case .windows => Ptr[void]
    else => void

type DWORD = switch meta.system():
    case .windows => r32
    else => void

type BOOL = switch meta.system():
    case .windows => i32
    else => void

type WCHAR = switch meta.system():
    case .windows => r16
    else => void

type LPCWSTR = switch meta.system():
    case .windows => Ptr[WCHAR]
    else => void

type LPWSTR = switch meta.system():
    case .windows => Ptr[WCHAR]
    else => void

type LPDWORD = switch meta.system():
    case .windows => Ptr[DWORD]
    else => void

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

-- Windows constants
-- Note: INVALID_HANDLE_VALUE cannot be represented as a compile-time constant
-- in Cyber (it's (HANDLE)(LONG_PTR)-1). Use none for invalid handles and
-- check for none in comparisons. The Windows API will handle none appropriately.

const TRUE = switch meta.system():
    case .windows => BOOL(1)
    else => 0

const FALSE = switch meta.system():
    case .windows => BOOL(0)
    else => 0

-- File access flags (unsigned values for proper bit operations)
const GENERIC_READ = switch meta.system():
    case .windows => DWORD(0x80000000)
    else => 0

const GENERIC_WRITE = switch meta.system():
    case .windows => DWORD(0x40000000)
    else => 0

const GENERIC_EXECUTE = switch meta.system():
    case .windows => DWORD(0x20000000)
    else => 0

const GENERIC_ALL = switch meta.system():
    case .windows => DWORD(0x10000000)
    else => 0

-- File share mode
const FILE_SHARE_READ = switch meta.system():
    case .windows => DWORD(0x00000001)
    else => 0

const FILE_SHARE_WRITE = switch meta.system():
    case .windows => DWORD(0x00000002)
    else => 0

const FILE_SHARE_DELETE = switch meta.system():
    case .windows => DWORD(0x00000004)
    else => 0

-- File creation disposition
const CREATE_NEW = switch meta.system():
    case .windows => DWORD(1)
    else => 0

const CREATE_ALWAYS = switch meta.system():
    case .windows => DWORD(2)
    else => 0

const OPEN_EXISTING = switch meta.system():
    case .windows => DWORD(3)
    else => 0

const OPEN_ALWAYS = switch meta.system():
    case .windows => DWORD(4)
    else => 0

const TRUNCATE_EXISTING = switch meta.system():
    case .windows => DWORD(5)
    else => 0

-- File attributes
const FILE_ATTRIBUTE_NORMAL = switch meta.system():
    case .windows => DWORD(0x00000080)
    else => 0

const FILE_ATTRIBUTE_DIRECTORY = switch meta.system():
    case .windows => DWORD(0x00000010)
    else => 0

const FILE_ATTRIBUTE_READONLY = switch meta.system():
    case .windows => DWORD(0x00000001)
    else => 0

--| File attribute: reparse point (symlink).
const FILE_ATTRIBUTE_REPARSE_POINT = switch meta.system():
    case .windows => DWORD(0x00000400)
    else => 0

-- File flags
const FILE_FLAG_BACKUP_SEMANTICS = switch meta.system():
    case .windows => DWORD(0x02000000)
    else => 0

-- Error codes (subset of common errors)
const ERROR_SUCCESS = switch meta.system():
    case .windows => DWORD(0)
    else => 0

const ERROR_FILE_NOT_FOUND = switch meta.system():
    case .windows => DWORD(2)
    else => 0

const ERROR_PATH_NOT_FOUND = switch meta.system():
    case .windows => DWORD(3)
    else => 0

const ERROR_ACCESS_DENIED = switch meta.system():
    case .windows => DWORD(5)
    else => 0

const ERROR_INVALID_HANDLE = switch meta.system():
    case .windows => DWORD(6)
    else => 0

const ERROR_NOT_ENOUGH_MEMORY = switch meta.system():
    case .windows => DWORD(8)
    else => 0

const ERROR_INVALID_PARAMETER = switch meta.system():
    case .windows => DWORD(87)
    else => 0

const ERROR_SHARING_VIOLATION = switch meta.system():
    case .windows => DWORD(32)
    else => 0

const ERROR_ALREADY_EXISTS = switch meta.system():
    case .windows => DWORD(183)
    else => 0

--| Error code: no more files in directory.
const ERROR_NO_MORE_FILES = switch meta.system():
    case .windows => DWORD(18)
    else => 0

--| Special handle value indicating invalid handle.
--| In Windows this is (HANDLE)-1, which is 0xFFFFFFFFFFFFFFFF on 64-bit.
--| Note: We use none (null pointer) in the code instead of this constant.
-- const INVALID_HANDLE_VALUE = none

-- Standard handles
-- Note: These are defined as (DWORD)-10, -11, -12 in Windows
-- In unsigned 32-bit, -10 = 0xFFFFFFF6, -11 = 0xFFFFFFF5, -12 = 0xFFFFFFF4
const STD_INPUT_HANDLE = switch meta.system():
    case .windows => DWORD(0xFFFFFFF6)
    else => 0

const STD_OUTPUT_HANDLE = switch meta.system():
    case .windows => DWORD(0xFFFFFFF5)
    else => 0

const STD_ERROR_HANDLE = switch meta.system():
    case .windows => DWORD(0xFFFFFFF4)
    else => 0

--| Seek origin constants for SetFilePointerEx.
const FILE_BEGIN = switch meta.system():
    case .windows => DWORD(0)
    else => 0

const FILE_CURRENT = switch meta.system():
    case .windows => DWORD(1)
    else => 0

const FILE_END = switch meta.system():
    case .windows => DWORD(2)
    else => 0

-- Win32 API function declarations
-- Note: These are declared on all platforms but only bound on Windows

#[extern]
fn CreateFileW(lpFileName LPCWSTR, dwDesiredAccess DWORD,
               dwShareMode DWORD, lpSecurityAttributes Ptr[void],
               dwCreationDisposition DWORD, dwFlagsAndAttributes DWORD,
               hTemplateFile HANDLE) -> HANDLE

#[extern]
fn ReadFile(hFile HANDLE, lpBuffer Ptr[byte], nNumberOfBytesToRead DWORD,
            lpNumberOfBytesRead LPDWORD, lpOverlapped Ptr[void]) -> BOOL

#[extern]
fn WriteFile(hFile HANDLE, lpBuffer Ptr[byte], nNumberOfBytesToWrite DWORD,
             lpNumberOfBytesWritten LPDWORD, lpOverlapped Ptr[void]) -> BOOL

--| Move file pointer to new position.
--| Returns FALSE on error, check GetLastError().
#[extern]
fn SetFilePointerEx(hFile HANDLE, liDistanceToMove LARGE_INTEGER,
                    lpNewFilePointer Ptr[LARGE_INTEGER], dwMoveMethod DWORD) -> BOOL

#[extern]
fn CloseHandle(hObject HANDLE) -> BOOL

#[extern]
fn GetLastError() -> DWORD

#[extern]
fn SetLastError(dwErrCode DWORD) -> void

#[extern]
fn GetStdHandle(nStdHandle DWORD) -> HANDLE

#[extern]
fn CreateDirectoryW(lpPathName LPCWSTR, lpSecurityAttributes Ptr[void]) -> BOOL

#[extern]
fn RemoveDirectoryW(lpPathName LPCWSTR) -> BOOL

#[extern]
fn DeleteFileW(lpFileName LPCWSTR) -> BOOL

#[extern]
fn GetCurrentDirectoryW(nBufferLength DWORD, lpBuffer LPWSTR) -> DWORD

#[extern]
fn SetCurrentDirectoryW(lpPathName LPCWSTR) -> BOOL

#[extern]
fn GetFileAttributesW(lpFileName LPCWSTR) -> DWORD

--| Find first file matching pattern.
--| Returns INVALID_HANDLE_VALUE on error.
#[extern]
fn FindFirstFileW(lpFileName LPCWSTR, lpFindFileData Ptr[WIN32_FIND_DATAW]) -> HANDLE

--| Find next file in directory iteration.
--| Returns FALSE when no more files.
#[extern]
fn FindNextFileW(hFindFile HANDLE, lpFindFileData Ptr[WIN32_FIND_DATAW]) -> BOOL

--| Close directory search handle.
--| Returns FALSE on error.
#[extern]
fn FindClose(hFindFile HANDLE) -> BOOL

#[extern]
fn GetFullPathNameW(lpFileName LPCWSTR, nBufferLength DWORD,
                    lpBuffer LPWSTR, lpFilePart Ptr[LPWSTR]) -> DWORD

#[extern]
fn GetModuleFileNameW(hModule HANDLE, lpFilename LPWSTR, nSize DWORD) -> DWORD

#[extern]
fn ExitProcess(uExitCode r32) -> never

-- Environment variable functions
#[extern]
fn GetEnvironmentVariableW(lpName LPCWSTR, lpBuffer LPWSTR, nSize DWORD) -> DWORD

#[extern]
fn SetEnvironmentVariableW(lpName LPCWSTR, lpValue LPCWSTR) -> BOOL

--| Get all environment variables as a double-null-terminated string block.
#[extern]
fn GetEnvironmentStringsW() -> LPWSTR

--| Free environment strings block returned by GetEnvironmentStringsW.
#[extern]
fn FreeEnvironmentStringsW(lpszEnvironmentBlock LPWSTR) -> BOOL

-- System information
--| System information structure.
type SYSTEM_INFO cstruct:
    wProcessorArchitecture      r16
    wReserved                   r16
    dwPageSize                  DWORD
    lpMinimumApplicationAddress Ptr[void]
    lpMaximumApplicationAddress Ptr[void]
    dwActiveProcessorMask       int
    dwNumberOfProcessors        DWORD
    dwProcessorType             DWORD
    dwAllocationGranularity     DWORD
    wProcessorLevel             r16
    wProcessorRevision          r16

#[extern]
fn GetSystemInfo(lpSystemInfo Ptr[SYSTEM_INFO]) -> void

-- Timing functions
#[extern]
fn Sleep(dwMilliseconds DWORD) -> void

#[extern]
fn GetSystemTimeAsFileTime(lpSystemTimeAsFileTime Ptr[FILETIME]) -> void

#[extern]
fn QueryPerformanceCounter(lpPerformanceCount Ptr[LARGE_INTEGER]) -> BOOL

#[extern]
fn QueryPerformanceFrequency(lpFrequency Ptr[LARGE_INTEGER]) -> BOOL

-- File operations
--| File attribute data for GetFileAttributesExW.
type WIN32_FILE_ATTRIBUTE_DATA cstruct:
    dwFileAttributes DWORD
    ftCreationTime   FILETIME
    ftLastAccessTime FILETIME
    ftLastWriteTime  FILETIME
    nFileSizeHigh    DWORD
    nFileSizeLow     DWORD

--| Info level for GetFileAttributesExW.
const GetFileExInfoStandard = switch meta.system():
    case .windows => 0
    else => 0

#[extern]
fn GetFileAttributesExW(lpFileName LPCWSTR, fInfoLevelId DWORD, lpFileInformation Ptr[WIN32_FILE_ATTRIBUTE_DATA]) -> BOOL

#[extern]
fn CopyFileW(lpExistingFileName LPCWSTR, lpNewFileName LPCWSTR, bFailIfExists BOOL) -> BOOL

--| File information by handle.
type BY_HANDLE_FILE_INFORMATION cstruct:
    dwFileAttributes     DWORD
    ftCreationTime       FILETIME
    ftLastAccessTime     FILETIME
    ftLastWriteTime      FILETIME
    dwVolumeSerialNumber DWORD
    nFileSizeHigh        DWORD
    nFileSizeLow         DWORD
    nNumberOfLinks       DWORD
    nFileIndexHigh       DWORD
    nFileIndexLow        DWORD

#[extern]
fn GetFileInformationByHandle(hFile HANDLE, lpFileInformation Ptr[BY_HANDLE_FILE_INFORMATION]) -> BOOL

-- Heap memory functions
--| Get handle to the default process heap.
#[extern]
fn GetProcessHeap() -> HANDLE

--| Allocate memory from a heap.
--| dwFlags: 0 for default, HEAP_ZERO_MEMORY (0x08) to zero-initialize.
#[extern]
fn HeapAlloc(hHeap HANDLE, dwFlags DWORD, dwBytes int) -> Ptr[void]

--| Free memory allocated from a heap.
#[extern]
fn HeapFree(hHeap HANDLE, dwFlags DWORD, lpMem Ptr[void]) -> BOOL

-- Heap allocation flags
const HEAP_ZERO_MEMORY = switch meta.system():
    case .windows => DWORD(0x00000008)
    else => 0

-- Virtual memory functions
--| Allocate or reserve virtual memory.
#[extern]
fn VirtualAlloc(lpAddress Ptr[void], dwSize int, flAllocationType DWORD, flProtect DWORD) -> Ptr[void]

--| Free virtual memory.
#[extern]
fn VirtualFree(lpAddress Ptr[void], dwSize int, dwFreeType DWORD) -> BOOL

--| Change protection on virtual memory region.
#[extern]
fn VirtualProtect(lpAddress Ptr[void], dwSize int, flNewProtect DWORD, lpflOldProtect Ptr[DWORD]) -> BOOL

-- Virtual memory allocation type flags
const MEM_COMMIT = switch meta.system():
    case .windows => DWORD(0x00001000)
    else => 0

const MEM_RESERVE = switch meta.system():
    case .windows => DWORD(0x00002000)
    else => 0

const MEM_RELEASE = switch meta.system():
    case .windows => DWORD(0x00008000)
    else => 0

const MEM_DECOMMIT = switch meta.system():
    case .windows => DWORD(0x00004000)
    else => 0

-- Virtual memory protection flags
const PAGE_NOACCESS = switch meta.system():
    case .windows => DWORD(0x01)
    else => 0

const PAGE_READONLY = switch meta.system():
    case .windows => DWORD(0x02)
    else => 0

const PAGE_READWRITE = switch meta.system():
    case .windows => DWORD(0x04)
    else => 0

const PAGE_EXECUTE = switch meta.system():
    case .windows => DWORD(0x10)
    else => 0

const PAGE_EXECUTE_READ = switch meta.system():
    case .windows => DWORD(0x20)
    else => 0

const PAGE_EXECUTE_READWRITE = switch meta.system():
    case .windows => DWORD(0x40)
    else => 0
