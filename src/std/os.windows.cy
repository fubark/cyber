use os
use c
use meta

#[cond=meta.system() == .windows]
use kernel32 'windows/kernel32.cy'

--| Windows-specific OS implementations.
--|
--| ## Purpose:
--| Provides Windows-specific implementations of OS operations using Win32 API.
--| This module can be conditionally imported by os.cy on Windows platforms.
--|
--| ## Key Implementations:
--| - Error code translation (Win32 error codes to Cyber errors)
--| - UTF-8 to UTF-16LE string conversion
--| - File operations using Win32 API (standalone functions)
--| - Directory operations using Win32 API
--|
--| ## Design Pattern:
--| Follows the same pattern as os.macos.cy - uses os types as parameters/returns
--|
--| ## Circular Dependency Fix:
--| All file operations are standalone functions (e.g., readFileWindows(file, buf))
--| instead of method extensions (e.g., fn (&os.File) readWindows(buf)).
--| This allows the module to be imported without triggering immediate type resolution.

-- Convert Windows error codes to Cyber errors
fn fromWin32Error() -> error:
    err := kernel32.GetLastError()
    return switch err:
        case kernel32.ERROR_SUCCESS => error.Unknown  -- Should not happen
        case kernel32.ERROR_FILE_NOT_FOUND => error.FileNotFound
        case kernel32.ERROR_PATH_NOT_FOUND => error.FileNotFound
        case kernel32.ERROR_ACCESS_DENIED => error.AccessDenied
        case kernel32.ERROR_INVALID_HANDLE => error.BadFile
        case kernel32.ERROR_NOT_ENOUGH_MEMORY => error.OutOfMemory
        case kernel32.ERROR_INVALID_PARAMETER => error.InvalidArgument
        case kernel32.ERROR_SHARING_VIOLATION => error.AccessDenied
        case kernel32.ERROR_ALREADY_EXISTS => error.FileExists
        else => error.Unknown

-- Convert UTF-8 string to UTF-16LE for Windows APIs
-- Full Unicode implementation with surrogate pair support
-- Follows Unicode Standard and Windows WTF-16LE conventions
-- 
-- UTF-8 Encoding Rules:
-- 1-byte: 0xxxxxxx (ASCII, U+0000 to U+007F)
-- 2-byte: 110xxxxx 10xxxxxx (U+0080 to U+07FF)
-- 3-byte: 1110xxxx 10xxxxxx 10xxxxxx (U+0800 to U+FFFF, excluding surrogates)
-- 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx (U+10000 to U+10FFFF)
--
-- UTF-16LE Encoding Rules:
-- BMP (U+0000 to U+D7FF, U+E000 to U+FFFF): Single 16-bit code unit
-- Supplementary (U+10000 to U+10FFFF): Surrogate pair (high + low)
-- High surrogate: 0xD800-0xDBFF, Low surrogate: 0xDC00-0xDFFF
fn utf8ToUtf16Le(utf8 str) -> ![]kernel32.WCHAR:
    utf8_len := utf8.len()
    if utf8_len == 0:
        -- Empty string - just null terminator
        return []kernel32.WCHAR{0}
    
    -- Estimate buffer size (worst case: all ASCII = same length, +1 for null)
    -- For non-ASCII, we may need less space (multi-byte UTF-8 → single UTF-16)
    -- or more space (characters outside BMP need surrogate pairs)
    estimated_size := utf8_len + 1
    buf := []kernel32.WCHAR(estimated_size, 0)
    
    var utf8_idx int = 0
    var utf16_idx int = 0
    
    while utf8_idx < utf8_len:
        byte1 := utf8[utf8_idx]
        var codepoint int = 0
        var bytes_to_read int = 0
        
        -- Decode UTF-8 sequence to Unicode codepoint
        if (byte1 && 0x80) == 0:
            -- 1-byte sequence: 0xxxxxxx (ASCII)
            -- Mask: 0x80 = 10000000, result 0 = ASCII character
            -- Extract: byte1 & 0x7F = 01111111 (7 bits of payload)
            codepoint = int(byte1)
            bytes_to_read = 1
        else:
            if (byte1 && 0xE0) == 0xC0:
                -- 2-byte sequence: 110xxxxx 10xxxxxx
                -- Mask: 0xE0 = 11100000, result 110xxxxx = 2-byte start
                -- Extract: byte1 & 0x1F = 00011111 (5 bits from first byte)
                -- Extract: byte2 & 0x3F = 00111111 (6 bits from second byte)
                -- Combine: (5 bits << 6) | 6 bits = 11 bits total (U+0080 to U+07FF)
                if utf8_idx + 1 >= utf8_len:
                    return error.InvalidArgument  -- Truncated sequence
                byte2 := utf8[utf8_idx + 1]
                if (byte2 && 0xC0) != 0x80:
                    return error.InvalidArgument  -- Invalid continuation byte
                codepoint = (int(byte1 && 0x1F) << 6) || int(byte2 && 0x3F)
                bytes_to_read = 2
                -- Check for overlong encoding (must use shortest possible encoding)
                if codepoint < 0x80:
                    return error.InvalidArgument
            else:
                if (byte1 && 0xF0) == 0xE0:
                    -- 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
                    -- Mask: 0xF0 = 11110000, result 1110xxxx = 3-byte start
                    -- Extract: byte1 & 0x0F = 00001111 (4 bits from first byte)
                    -- Extract: byte2 & 0x3F = 00111111 (6 bits from second byte)
                    -- Extract: byte3 & 0x3F = 00111111 (6 bits from third byte)
                    -- Combine: (4 bits << 12) | (6 bits << 6) | 6 bits = 16 bits total
                    if utf8_idx + 2 >= utf8_len:
                        return error.InvalidArgument
                    byte2 := utf8[utf8_idx + 1]
                    byte3 := utf8[utf8_idx + 2]
                    if (byte2 && 0xC0) != 0x80 or (byte3 && 0xC0) != 0x80:
                        return error.InvalidArgument
                    codepoint = (int(byte1 && 0x0F) << 12) || (int(byte2 && 0x3F) << 6) || int(byte3 && 0x3F)
                    bytes_to_read = 3
                    -- Check for overlong encoding and surrogate range (U+D800-U+DFFF reserved)
                    if codepoint < 0x800 or (codepoint >= 0xD800 and codepoint <= 0xDFFF):
                        return error.InvalidArgument
                else:
                    if (byte1 && 0xF8) == 0xF0:
                        -- 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                        -- Mask: 0xF8 = 11111000, result 11110xxx = 4-byte start
                        -- Extract: byte1 & 0x07 = 00000111 (3 bits from first byte)
                        -- Extract: bytes 2-4 each contribute 6 bits
                        -- Combine: (3 bits << 18) | (6 bits << 12) | (6 bits << 6) | 6 bits = 21 bits total
                        if utf8_idx + 3 >= utf8_len:
                            return error.InvalidArgument
                        byte2 := utf8[utf8_idx + 1]
                        byte3 := utf8[utf8_idx + 2]
                        byte4 := utf8[utf8_idx + 3]
                        if (byte2 && 0xC0) != 0x80 or (byte3 && 0xC0) != 0x80 or (byte4 && 0xC0) != 0x80:
                            return error.InvalidArgument
                        codepoint = (int(byte1 && 0x07) << 18) || (int(byte2 && 0x3F) << 12) || (int(byte3 && 0x3F) << 6) || int(byte4 && 0x3F)
                        bytes_to_read = 4
                        -- Check for overlong encoding and valid Unicode range (U+10000 to U+10FFFF)
                        if codepoint < 0x10000 or codepoint > 0x10FFFF:
                            return error.InvalidArgument
                    else:
                        -- Invalid UTF-8 start byte (doesn't match any valid pattern)
                        return error.InvalidArgument
        
        -- Encode codepoint to UTF-16LE
        if codepoint < 0x10000:
            -- BMP character - single UTF-16 code unit
            -- Ensure buffer has space
            if utf16_idx >= buf.len():
                -- Grow buffer (double size for efficiency)
                new_buf := []kernel32.WCHAR(buf.len() * 2, 0)
                for 0..buf.len() |i|:
                    new_buf[i] = buf[i]
                buf = new_buf
            buf[utf16_idx] = kernel32.WCHAR(codepoint)
            utf16_idx += 1
        else:
            -- Supplementary character - surrogate pair needed
            -- Ensure buffer has space for 2 code units
            while utf16_idx + 1 >= buf.len():
                new_buf := []kernel32.WCHAR(buf.len() * 2, 0)
                for 0..buf.len() |i|:
                    new_buf[i] = buf[i]
                buf = new_buf
            
            -- Calculate surrogate pair using UTF-16 surrogate algorithm
            -- Formula: codepoint - 0x10000 = 20-bit value (10 high bits + 10 low bits)
            -- High surrogate: 0xD800 + (high 10 bits)
            -- Low surrogate:  0xDC00 + (low 10 bits)
            adjusted := codepoint - 0x10000
            high_surrogate := kernel32.WCHAR(0xD800 + (adjusted >> 10))      -- Extract high 10 bits
            low_surrogate := kernel32.WCHAR(0xDC00 + (adjusted && 0x3FF))    -- Extract low 10 bits
            
            buf[utf16_idx] = high_surrogate
            buf[utf16_idx + 1] = low_surrogate
            utf16_idx += 2
        
        utf8_idx += bytes_to_read
    
    -- Add null terminator (Windows APIs expect null-terminated strings)
    if utf16_idx >= buf.len():
        new_buf := []kernel32.WCHAR(utf16_idx + 1, 0)
        for 0..utf16_idx |i|:
            new_buf[i] = buf[i]
        buf = new_buf
    buf[utf16_idx] = kernel32.WCHAR(0)
    
    -- Return exact-sized buffer (copy to avoid wasted space)
    result := []kernel32.WCHAR(utf16_idx + 1, 0)
    for 0..(utf16_idx + 1) |i|:
        result[i] = buf[i]
    
    return result

-- Convert UTF-16LE string to UTF-8
-- Full Unicode implementation with surrogate pair support
-- Follows Unicode Standard and Windows WTF-16LE conventions
--
-- UTF-16LE Decoding Rules:
-- BMP characters (U+0000 to U+D7FF, U+E000 to U+FFFF): Single 16-bit unit
-- Surrogate pairs: High (0xD800-0xDBFF) + Low (0xDC00-0xDFFF) = Supplementary (U+10000-U+10FFFF)
-- Invalid: Unpaired surrogates, wrong order, or reserved ranges
--
-- UTF-8 Encoding Rules:
-- 1-byte: 0xxxxxxx (U+0000 to U+007F)
-- 2-byte: 110xxxxx 10xxxxxx (U+0080 to U+07FF)
-- 3-byte: 1110xxxx 10xxxxxx 10xxxxxx (U+0800 to U+FFFF)
-- 4-byte: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx (U+10000 to U+10FFFF)
fn utf16LeToUtf8(utf16 []kernel32.WCHAR) -> !str:
    -- Find null terminator (Windows APIs return null-terminated strings)
    var utf16_len int = 0
    while utf16_len < utf16.len() and utf16[utf16_len] != 0:
        utf16_len += 1
    
    if utf16_len == 0:
        return ''
    
    -- Estimate buffer size (worst case: each UTF-16 unit → 3 bytes UTF-8)
    -- BMP characters: max 3 bytes, Surrogate pairs: 4 bytes total
    estimated_size := utf16_len * 3
    buf := []byte(estimated_size, 0)
    
    var utf16_idx int = 0
    var utf8_idx int = 0
    
    while utf16_idx < utf16_len:
        unit1 := int(utf16[utf16_idx])
        var codepoint int = 0
        
        -- Check if this is a high surrogate (0xD800-0xDBFF)
        if unit1 >= 0xD800 and unit1 <= 0xDBFF:
            -- High surrogate - must be followed by low surrogate
            -- High surrogates indicate the start of a supplementary character
            if utf16_idx + 1 >= utf16_len:
                return error.InvalidArgument  -- Unpaired high surrogate (EOF)
            
            unit2 := int(utf16[utf16_idx + 1])
            if unit2 < 0xDC00 or unit2 > 0xDFFF:
                return error.InvalidArgument  -- Invalid low surrogate range
            
            -- Decode surrogate pair to codepoint using UTF-16 algorithm
            -- Formula: codepoint = 0x10000 + ((high - 0xD800) << 10) + (low - 0xDC00)
            -- Extract 10 bits from each surrogate and combine with 0x10000 offset
            high := unit1 - 0xD800      -- High surrogate: extract 10 payload bits
            low := unit2 - 0xDC00       -- Low surrogate: extract 10 payload bits
            codepoint = 0x10000 + (high << 10) + low
            utf16_idx += 2              -- Consumed both surrogate units
        else:
            if unit1 >= 0xDC00 and unit1 <= 0xDFFF:
                -- Unpaired low surrogate (invalid without preceding high surrogate)
                return error.InvalidArgument
            else:
                -- BMP character (not a surrogate, U+0000 to U+FFFF excluding surrogate range)
                codepoint = unit1
                utf16_idx += 1
        
        -- Encode codepoint to UTF-8 using variable-length encoding
        -- Ensure buffer has enough space (max 4 bytes per codepoint)
        while utf8_idx + 4 >= buf.len():
            new_buf := []byte(buf.len() * 2, 0)
            for 0..buf.len() |i|:
                new_buf[i] = buf[i]
            buf = new_buf
        
        if codepoint < 0x80:
            -- 1-byte sequence: 0xxxxxxx (ASCII)
            -- Direct encoding: 7 bits fit in single byte
            buf[utf8_idx] = byte(codepoint)
            utf8_idx += 1
        else:
            if codepoint < 0x800:
                -- 2-byte sequence: 110xxxxx 10xxxxxx
                -- First byte: 110 + 5 bits (codepoint >> 6)
                -- Second byte: 10 + 6 bits (codepoint & 0x3F)
                buf[utf8_idx] = byte(0xC0 || (codepoint >> 6))      -- 110xxxxx
                buf[utf8_idx + 1] = byte(0x80 || (codepoint && 0x3F)) -- 10xxxxxx
                utf8_idx += 2
            else:
                if codepoint < 0x10000:
                    -- 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
                    -- First byte: 1110 + 4 bits (codepoint >> 12)
                    -- Second byte: 10 + 6 bits ((codepoint >> 6) & 0x3F)
                    -- Third byte: 10 + 6 bits (codepoint & 0x3F)
                    buf[utf8_idx] = byte(0xE0 || (codepoint >> 12))               -- 1110xxxx
                    buf[utf8_idx + 1] = byte(0x80 || ((codepoint >> 6) && 0x3F)) -- 10xxxxxx
                    buf[utf8_idx + 2] = byte(0x80 || (codepoint && 0x3F))        -- 10xxxxxx
                    utf8_idx += 3
                else:
                    -- 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
                    -- First byte: 11110 + 3 bits (codepoint >> 18)
                    -- Second byte: 10 + 6 bits ((codepoint >> 12) & 0x3F)
                    -- Third byte: 10 + 6 bits ((codepoint >> 6) & 0x3F)
                    -- Fourth byte: 10 + 6 bits (codepoint & 0x3F)
                    buf[utf8_idx] = byte(0xF0 || (codepoint >> 18))               -- 11110xxx
                    buf[utf8_idx + 1] = byte(0x80 || ((codepoint >> 12) && 0x3F)) -- 10xxxxxx
                    buf[utf8_idx + 2] = byte(0x80 || ((codepoint >> 6) && 0x3F))  -- 10xxxxxx
                    buf[utf8_idx + 3] = byte(0x80 || (codepoint && 0x3F))        -- 10xxxxxx
                    utf8_idx += 4
    
    -- Return exact-sized string (copy to avoid wasted space)
    result := []byte(utf8_idx, 0)
    for 0..utf8_idx |i|:
        result[i] = buf[i]
    
    return str(result)

-- Windows file implementation
fn createFileWindows(path str, truncate bool) -> !os.File:
    path16 := utf8ToUtf16Le(path)!
    
    access := kernel32.GENERIC_READ || kernel32.GENERIC_WRITE
    share := kernel32.FILE_SHARE_READ || kernel32.FILE_SHARE_WRITE
    disposition := if (truncate) kernel32.CREATE_ALWAYS else kernel32.OPEN_ALWAYS
    
    handle := kernel32.CreateFileW(
        path16.ptr,
        access,
        share,
        none,
        disposition,
        kernel32.FILE_ATTRIBUTE_NORMAL,
        none  -- hTemplateFile (no template)
    )
    
    if handle == none:
        return fromWin32Error()
    
    return os.File{fd=as[int] handle}

-- Open existing file for reading
fn openFileWindows(path str) -> !os.File:
    path16 := utf8ToUtf16Le(path)!
    share := kernel32.FILE_SHARE_READ || kernel32.FILE_SHARE_WRITE
    
    handle := kernel32.CreateFileW(
        path16.ptr,
        kernel32.GENERIC_READ,
        share,
        none,
        kernel32.OPEN_EXISTING,
        kernel32.FILE_ATTRIBUTE_NORMAL,
        none  -- hTemplateFile (no template)
    )
    
    if handle == none:
        return fromWin32Error()
    
    return os.File{fd=as[int] handle}

-- Create directory on Windows
fn createDirWindows(path str) -> !void:
    path16 := utf8ToUtf16Le(path)!
    
    if kernel32.CreateDirectoryW(path16.ptr, none) == kernel32.FALSE:
        return fromWin32Error()

-- Remove directory on Windows
fn removeDirWindows(path str) -> !void:
    path16 := utf8ToUtf16Le(path)!
    
    if kernel32.RemoveDirectoryW(path16.ptr) == kernel32.FALSE:
        return fromWin32Error()

-- Remove file on Windows
fn removeFileWindows(path str) -> !void:
    path16 := utf8ToUtf16Le(path)!
    
    if kernel32.DeleteFileW(path16.ptr) == kernel32.FALSE:
        return fromWin32Error()

-- Get current directory on Windows
fn getCwdWindows() -> !str:
    -- First call to get required buffer size
    size := kernel32.GetCurrentDirectoryW(0, none)
    if size == 0:
        return fromWin32Error()
    
    -- Allocate buffer and get actual path
    buf := []kernel32.WCHAR(size, 0)
    result := kernel32.GetCurrentDirectoryW(size, buf.ptr)
    if result == 0 or result > size:
        return fromWin32Error()
    
    return utf16LeToUtf8(buf)!

-- Change current directory on Windows
fn setCwdWindows(path str) -> !void:
    path16 := utf8ToUtf16Le(path)!
    
    if kernel32.SetCurrentDirectoryW(path16.ptr) == kernel32.FALSE:
        return fromWin32Error()

-- Get executable path on Windows
fn getExePathWindows() -> !str:
    -- Windows MAX_PATH is 260, but we use larger buffer for long paths
    buf := []kernel32.WCHAR(32768, 0)
    
    size := kernel32.GetModuleFileNameW(none, buf.ptr, kernel32.DWORD(buf.len()))
    if size == 0:
        return fromWin32Error()
    
    return utf16LeToUtf8(buf[0..size])!

-- Check file access on Windows
fn accessFileWindows(path str) -> !void:
    path16 := utf8ToUtf16Le(path)!
    
    attrs := kernel32.GetFileAttributesW(path16.ptr)
    -- GetFileAttributesW returns INVALID_FILE_ATTRIBUTES (0xFFFFFFFF) on error
    -- Compare directly with the constant value as DWORD
    if attrs == kernel32.DWORD(4294967295):  -- 0xFFFFFFFF as decimal
        return fromWin32Error()

--| Read bytes from file into buffer.
--| Returns number of bytes actually read.
--| Returns 0 when end of file is reached.
fn readFileWindows(file &os.File, buf [&]byte) -> !int:
    handle := as[kernel32.HANDLE] file.fd
    var bytes_read kernel32.DWORD = 0
    
    result := kernel32.ReadFile(
        handle,
        as buf.base,
        kernel32.DWORD(buf.length),
        &bytes_read,
        none  -- No overlapped I/O
    )
    
    if result == kernel32.FALSE:
        return fromWin32Error()
    
    return as[int] bytes_read

--| Write bytes from buffer to file.
--| Returns number of bytes actually written.
fn writeFileWindows(file &os.File, buf []byte) -> !int:
    handle := as[kernel32.HANDLE] file.fd
    var bytes_written kernel32.DWORD = 0
    
    result := kernel32.WriteFile(
        handle,
        as buf.ptr,
        kernel32.DWORD(buf.len()),
        &bytes_written,
        none
    )
    
    if result == kernel32.FALSE:
        return fromWin32Error()
    
    return as[int] bytes_written

--| Write string to file (convenience wrapper).
fn writeFileStrWindows(file &os.File, val str) -> !int:
    handle := as[kernel32.HANDLE] file.fd
    var bytes_written kernel32.DWORD = 0
    result := kernel32.WriteFile(handle, as val.ptr, kernel32.DWORD(val.len()), &bytes_written, none)
    if result == kernel32.FALSE:
        return fromWin32Error()
    return as[int] bytes_written

--| Seek to position relative to current location.
fn seekFileWindows(file &os.File, offset int) -> !void:
    return seekFileWindowsInternal(file, offset, kernel32.FILE_CURRENT)

--| Seek to absolute position from start.
--| Negative offset is invalid.
fn seekFileFromStartWindows(file &os.File, offset int) -> !void:
    if offset < 0:
        return error.InvalidArgument
    return seekFileWindowsInternal(file, offset, kernel32.FILE_BEGIN)

--| Seek to position relative to end.
--| Positive offset is invalid.
fn seekFileFromEndWindows(file &os.File, offset int) -> !void:
    if offset > 0:
        return error.InvalidArgument
    return seekFileWindowsInternal(file, offset, kernel32.FILE_END)

--| Internal seek implementation using SetFilePointerEx.
fn seekFileWindowsInternal(file &os.File, offset int, whence kernel32.DWORD) -> !void:
    handle := as[kernel32.HANDLE] file.fd
    
    -- Create LARGE_INTEGER for 64-bit offset
    var distance kernel32.LARGE_INTEGER = undef
    distance.QuadPart = offset
    
    var new_pos kernel32.LARGE_INTEGER = undef
    
    result := kernel32.SetFilePointerEx(
        handle,
        distance,
        &new_pos,
        whence
    )
    
    if result == kernel32.FALSE:
        return fromWin32Error()

--| Close file handle and mark as closed.
--| Subsequent operations will fail.
fn closeFileWindows(file &os.File) -> !void:
    if file.closed:
        return
    
    handle := as[kernel32.HANDLE] file.fd
    
    if kernel32.CloseHandle(handle) == kernel32.FALSE:
        return fromWin32Error()
    
    file.closed = true

--| Windows directory iterator state.
--| Uses FindFirstFileW/FindNextFileW pattern.
--| Note: We don't store dir_path here to avoid string reference counting issues.
--| Instead, we access dir.path directly in next() since Dir is passed by reference.
type DirIteratorImpl:
    handle kernel32.HANDLE = none
    find_data kernel32.WIN32_FIND_DATAW
    first_call bool = true

--| Get next directory entry.
--| Returns none when no more entries.
--| Automatically skips '.' and '..' entries.
fn (&DirIteratorImpl) next(dir &os.Dir) -> !?os.DirEntry:
    if $first_call:
        -- First call - initialize FindFirstFileW
        -- Use dir.path directly to avoid string copy/double-free issues
        pattern := dir.path + "\\*"
        pattern16 := utf8ToUtf16Le(pattern)!
        $handle = kernel32.FindFirstFileW(pattern16.ptr, &$find_data)
        if $handle == none:
            return fromWin32Error()
        $first_call = false
    else:
        -- Subsequent calls - use FindNextFileW
        if kernel32.FindNextFileW($handle, &$find_data) == kernel32.FALSE:
            err := kernel32.GetLastError()
            -- Close handle when iteration completes (defense in depth)
            if $handle != none:
                _ = kernel32.FindClose($handle)
                $handle = none
            if err == kernel32.ERROR_NO_MORE_FILES:
                return none
            else:
                kernel32.SetLastError(err)
                return fromWin32Error()
    
    -- Skip "." and ".." entries
    if $find_data.cFileName[0] == 0x2E:  -- '.'
        if $find_data.cFileName[1] == 0:
            return $next(dir)  -- Skip "."
        else:
            if $find_data.cFileName[1] == 0x2E and $find_data.cFileName[2] == 0:
                return $next(dir)  -- Skip ".."
    
    -- Convert filename from UTF-16LE to UTF-8
    -- Find null terminator to get actual length
    var name_len int = 0
    while name_len < 260 and $find_data.cFileName[name_len] != 0:
        name_len += 1
    -- Create a slice from the fixed array
    name_slice := []kernel32.WCHAR(name_len, 0)
    for 0..name_len |i|:
        name_slice[i] = $find_data.cFileName[i]
    name := utf16LeToUtf8(name_slice)!
    
    -- Determine file kind from attributes
    kind := fileKindFromAttributes($find_data.dwFileAttributes)
    
    return os.DirEntry{
        name = name,
        kind = kind,
    }

--| Map Windows file attributes to FileKind enum.
fn fileKindFromAttributes(attrs kernel32.DWORD) -> os.FileKind:
    if (attrs && kernel32.FILE_ATTRIBUTE_DIRECTORY) != 0:
        return .directory
    else:
        if (attrs && kernel32.FILE_ATTRIBUTE_REPARSE_POINT) != 0:
            return .sym_link
        else:
            return .file

--| Open directory for iteration.
--| On Windows, we store the path for later use in FindFirstFileW.
fn openDirWindows(path str, iterable bool) -> !os.Dir:
    -- Verify directory exists
    path16 := utf8ToUtf16Le(path)!
    attrs := kernel32.GetFileAttributesW(path16.ptr)
    
    if attrs == kernel32.DWORD(4294967295):  -- INVALID_FILE_ATTRIBUTES
        return fromWin32Error()
    
    if (attrs && kernel32.FILE_ATTRIBUTE_DIRECTORY) == 0:
        return error.NotADirectory
    
    -- Return Dir with path stored for iterator
    -- Note: fd is void on Windows, use undef
    return os.Dir{
        fd = undef,
        closed = false,
        path = path,
    }

-- File info helper functions
--| Convert Windows file attributes to FileInfo.
fn fileInfoFromWin32Attr(data Ptr[kernel32.WIN32_FILE_ATTRIBUTE_DATA]) -> os.FileInfo:
    size := (as[int] data.nFileSizeHigh << 32) || as[int] data.nFileSizeLow
    is_dir := (as[int] data.dwFileAttributes && as[int] kernel32.FILE_ATTRIBUTE_DIRECTORY) != 0
    kind := if (is_dir) os.FileKind.directory else os.FileKind.file
    mtime_ns := fileTimeToNanos(&data.ftLastWriteTime)
    atime_ns := fileTimeToNanos(&data.ftLastAccessTime)
    ctime_ns := fileTimeToNanos(&data.ftCreationTime)
    return os.FileInfo{
        inode = 0,
        size = size,
        mode = 0,
        kind = kind,
        atime_sec = atime_ns / 1000000000,
        atime_nsec = atime_ns % 1000000000,
        mtime_sec = mtime_ns / 1000000000,
        mtime_nsec = mtime_ns % 1000000000,
        ctime_sec = ctime_ns / 1000000000,
        ctime_nsec = ctime_ns % 1000000000,
    }

--| Convert Windows FILETIME to nanoseconds since Unix epoch.
fn fileTimeToNanos(ft Ptr[kernel32.FILETIME]) -> int:
    intervals := (as[int] ft.dwHighDateTime << 32) || as[int] ft.dwLowDateTime
    unix_intervals := intervals - 116444736000000000
    return unix_intervals * 100

--| Convert Windows BY_HANDLE_FILE_INFORMATION to FileInfo.
fn fileInfoFromHandle(info Ptr[kernel32.BY_HANDLE_FILE_INFORMATION]) -> os.FileInfo:
    size := (as[int] info.nFileSizeHigh << 32) || as[int] info.nFileSizeLow
    is_dir := (as[int] info.dwFileAttributes && as[int] kernel32.FILE_ATTRIBUTE_DIRECTORY) != 0
    kind := if (is_dir) os.FileKind.directory else os.FileKind.file
    mtime_ns := fileTimeToNanos(&info.ftLastWriteTime)
    atime_ns := fileTimeToNanos(&info.ftLastAccessTime)
    ctime_ns := fileTimeToNanos(&info.ftCreationTime)
    return os.FileInfo{
        inode = 0,
        size = size,
        mode = 0,
        kind = kind,
        atime_sec = atime_ns / 1000000000,
        atime_nsec = atime_ns % 1000000000,
        mtime_sec = mtime_ns / 1000000000,
        mtime_nsec = mtime_ns % 1000000000,
        ctime_sec = ctime_ns / 1000000000,
        ctime_nsec = ctime_ns % 1000000000,
    }

-- Environment variable functions
fn getEnvWindows(key str) -> ?str:
    key_len := key.len()
    key_bytes := []byte((key_len + 1) * 2, 0)
    for 0..key_len |i|:
        key_bytes[i * 2] = key[i]
        key_bytes[i * 2 + 1] = 0
    
    size := kernel32.GetEnvironmentVariableW(as key_bytes.ptr, none, 0)
    if size == 0:
        return none
    
    val_bytes := []byte(size * 2, 0)
    result := kernel32.GetEnvironmentVariableW(as key_bytes.ptr, as val_bytes.ptr, size)
    if result == 0:
        return none
    
    var val_len int = 0
    while val_len < size and (val_bytes[val_len * 2] != 0 or val_bytes[val_len * 2 + 1] != 0):
        val_len += 1
    
    result_bytes := []byte(val_len, 0)
    for 0..val_len |i|:
        result_bytes[i] = val_bytes[i * 2]
    return str(result_bytes)

fn getEnvAllWindows() -> ^Map[str, str]:
    env_block := kernel32.GetEnvironmentStringsW()
    if env_block == none:
        return ^Map[str, str]{}
    
    result := ^Map[str, str]{}
    var offset int = 0
    
    while env_block[offset] != 0:
        var str_end int = offset
        while env_block[str_end] != 0:
            str_end += 1
        
        str_len := str_end - offset
        slice := []kernel32.WCHAR(str_len, 0)
        for 0..str_len |i|:
            slice[i] = env_block[offset + i]
        
        entry := utf16LeToUtf8(slice) !else:
            offset = str_end + 1
            continue
        
        var eq_pos int = 0
        while eq_pos < entry.len() and entry[eq_pos] != '=':
            eq_pos += 1
        
        if eq_pos < entry.len():
            key := entry[0..eq_pos]
            val := entry[(eq_pos + 1)..]
            result[key] = val
        
        offset = str_end + 1
    
    _ = kernel32.FreeEnvironmentStringsW(env_block)
    return result
