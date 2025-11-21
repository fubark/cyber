--| Invoked by the program's initializer to start binding libs to `extern` declarations.
#[bind]
#fn @initBindLib() -> void

--| Declares that the extern declarations in the current source file is to be binded at runtime
--| to a dynamic library. When `path == none`, the symbols will be searched from the host exe.
#[bind]
#fn bind_lib(path ?str)

#[bind]
#fn flag(s str) -> void

--| Returns a `str` from a null terminated C string.
#[bind]
fn from_strz(ptr Ptr[byte]) -> str

#[bind]
#fn include(spec str) -> void

--| Returns an allocated null terminated C string.
#[bind]
fn to_strz(s str) -> Ptr[byte]

-- TODO: specialize for architecture and system.
type size_t = r64
type ssize_t = i64
type c_char = byte
type c_short = i16
type c_ushort = r16
type c_int = i32
type c_uint = r32
type c_long = i64
type c_ulong = r64
type c_longlong = i64
type c_ulonglong = r64

#[bind] type variadic _

--| TODO: TCC functions.