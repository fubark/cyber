use c
use meta
use windows 'windows.cy'

--| Windows kernel32.dll API bindings
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

#if meta.is_vm_target():
    #c.bind_lib('kernel32.dll')

type HANDLE = windows.HANDLE
type DWORD = windows.DWORD
type FILETIME = windows.FILETIME
type LPCWSTR = windows.LPCWSTR
type LPDWORD = windows.LPDWORD
type BOOL = windows.BOOL
type LARGE_INTEGER = windows.LARGE_INTEGER
type LPWSTR = windows.LPWSTR
type WIN32_FIND_DATAW = windows.WIN32_FIND_DATAW

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
const GetFileExInfoStandard = 0

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
