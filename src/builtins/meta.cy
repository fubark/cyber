--| Access a receiver with a given field as a string.
#[bind] fn access(rec Code, field EvalStr) -> Any

--| Returns whether a build flag is set.
#[bind] fn build_flag(name str) -> bool

#[bind] fn build_mode() -> BuildMode

--| Returns a build option's value.
#[bind] fn build_option(name str) -> ?str

--| Returns the tag of a choice value.
#[bind] fn choice_tag(choice %T) -> T.Tag

--| The current build's target cpu arch.
#[bind] fn cpu() -> EvalStr

--| Returns the version of the Cyber compiler.
#[bind] fn full_version() -> EvalStr

#[bind] fn dump_frame()

--| The current arch's endianness.
#[bind] fn endian() -> Endian

#[bind] fn enum_case(enum_value %T) -> EnumCase

#[bind] fn enum_int_values(%T type) -> EvalBuffer[int]

#[bind] fn enum_name_eval(enum_value %T) -> EvalStr

fn enum_name(enum_value %T) -> str:
    -- if meta.is_inline_eval():

    -- TODO: Use a LUT.
    switch enum_value:
        #for enum_values(T) |Tag|:
            case Tag:
                return #{enum_case(Tag).name}

#[bind] fn enum_values(%T type) -> EvalBuffer[T]

--| Raise compile-time error.
#[bind] fn error(msg EvalStr) -> never

--| Equivalent to the compile-time expression `#{}`.
#[bind] fn eval(code Code) -> Any

--| For debugging. Returns the current event ID.
#[bind] fn event() -> int

#[bind]
fn get_closure_data(func Ptr[void]) -> ?Ptr[ClosureData]:
    kind := (as[Ptr[int]] func).*
    if kind == 2:
        return as[Ptr[ClosureData]](as[int] func + 8)
    else:
        return none

--| Returns whether a type has a `name` declaration.
#[bind] fn has_decl(parent Code, name EvalStr) -> bool

#[bind] fn init_choice(%T type, Tag T.Tag, payload Code) -> T

--| Equivalent to the default T{} initializer.
--| When `$initRecord` is overloaded, this can be used to emit the default initializer.
#[bind] fn init_type(%T type, init Code) -> T

#[bind] fn is_inline_eval() -> bool

--| Returns whether an option or a reference to an option is `none`.
#[bind] fn is_none(option Code) -> bool

#[bind] fn is_result_error(result Code) -> bool

--| Whether source is being compiled for the VM.
#[bind] fn is_vm_target() -> bool

--| Load a file relative to the project root (main source file).
--| TODO: This should restrict loading files above the project root.
#[bind] fn load(path str) -> EvalStr

--| Log to stderr at compile-time.
#[bind] fn log(x %T)

#[bind] fn mod_uri() -> EvalStr

type StructConfig:
    tuple bool = false

--| Creates a new `struct` type.
--| `StructField.offset` is ignored when computing field offsets.
#[bind] fn new_struct(init [%N]StructField, config StructConfig) -> type

--| Returns whether the current statement is reachable.
#[bind] fn reachable() -> bool

#[bind] fn stack_trace() -> str

--| The current build's target operating system.
#[bind] fn system() -> SystemKind

--| Whether the compiler was built with `Trace`.
#[bind] fn trace_enabled() -> bool

--| Returns the target arch pointer bit width.
#[bind] fn pointer_width() -> int

#[bind, unsafe]
fn access_choice_case(choice %T, %Tag T.Tag) -> CasePayload[T, Tag]

--| Unwraps an option type without runtime checks.
#[bind, unsafe]
fn access_option_payload(opt ?%T) -> T

#[bind, unsafe]
fn access_result_payload(res !%T) -> T

#[bind, unsafe]
fn access_result_error(res !%T) -> error

-- TODO: These should be enabled by the trace flag.
#[bind] fn trace_retains() -> int
#[bind] fn trace_releases() -> int

--| Raise compile-time error.
#[consteval]
fn unsupported() -> never:
    error('unsupported')

type BorrowInfo:
    child type

type BuildMode enum:
    case debug
    case release

#[bind] type CasePayload[ChoiceT Any, const Tag ChoiceT.Tag] = _

type ChoiceCase:
    name EvalStr
    type type

type ChoiceInfo:
    name  ?EvalStr
    cases EvalBuffer[ChoiceCase]

#[bind] type Code _

type CStructInfo:
    name   ?EvalStr
    fields EvalBuffer[StructField]

type CUnionCase:
    name EvalStr
    type type

type CUnionInfo:
    name  ?EvalStr
    cases EvalBuffer[CUnionCase]

type EnumCase:
    name EvalStr

type EnumInfo:
    name  ?EvalStr
    cases EvalBuffer[EnumCase]

type VectorInfo:
    len  int
    elem type

type FloatInfo:
    bits int

type FuncInfo:
    kind   FuncKind
    params EvalBuffer[FuncParam]
    ret    type

type FuncKind enum:
    case ptr
    case union
    case sym

type FuncParam:
    type type

type IntInfo:
    bits   int

type RawInfo:
    bits   int

type OptionInfo:
    child type

type PointerInfo:
    child type

type ResultInfo:
    child type

type StructField:
    name   EvalStr
    type   type
    offset int

    state_offset int

type StructInfo:
    name   ?EvalStr
    fields EvalBuffer[StructField]
    -- funcs  []funcsym_t

type BorrowTraitInfo:
    child type

type TraitInfo:
    name EvalStr
    -- funcs []funcsym_t

type RefTraitInfo:
    child type

type DynTraitInfo:
    child type

#[bind]
type type _

#[bind]
type PartialStructLayout _

#[bind]
fn (PartialStructLayout) is_field_active(state_offset int) -> bool

#[bind]
fn (PartialStructLayout) field_layout(state_offset int, state_len int) -> ?PartialStructLayout

#[bind]
fn type :: @init(s str) -> type

#[bind]
fn type :: field(T type, s str) -> StructField

#[bind]
fn type :: fn_ret(T type) -> type

--| Returns a unique ID for this type.
#[bind]
fn type :: id(T type) -> int

#[bind]
fn type :: implements(T type, Trait type) -> bool

--| Returns info about a type.
#[bind]
fn type :: info(T type) -> TypeInfo

--| Returns whether a type is an instance of a template.
#[bind]
fn type :: is_instance_of(T type, Template Code) -> bool

--| Returns whether a type is copyable.
--| A type can be copyable if it does not implement `NoCopy` and contains members that are all copyable.
#[bind]
fn type :: is_copyable(T type) -> bool

--| Returns whether the type is const eligible.
#[bind]
fn type :: is_const(T type) -> bool

--| Returns the type of an expression. 
#[bind]
fn type :: of(expr Code) -> type

fn type :: offset(T type, s str) -> int:
    return type.field(T, s).offset

#[bind]
fn type :: struct_state_len(T type) -> int

--| Returns whether the type is managed.
--| Managed types either have a builtin destructor or a custom `T.$deinit`.
#[bind]
fn type :: managed(T type) -> bool

#[bind]
fn type :: name(T type) -> EvalStr

--| Returns the runtime type name. This is only available in trace mode.
#[bind]
fn type :: name_rt(id int) -> str

--| Returns the number of bytes that the type occupies.
#[bind]
fn type :: size(T type) -> int

type TypeInfo enum:
    case int         IntInfo
    case raw         RawInfo
    case vector      VectorInfo
    case partial_vector VectorInfo
    case bool        void
    case choice      ChoiceInfo
    case enum        EnumInfo
    case error       void
    case float       FloatInfo
    case func        FuncInfo
    case result      ResultInfo
    case option      OptionInfo
    case ptr         PointerInfo
    case ref         PointerInfo
    case borrow      BorrowInfo
    case struct      StructInfo
    case cstruct     CStructInfo
    case cunion      CUnionInfo
    case trait       TraitInfo
    case borrow_trait BorrowTraitInfo
    case ref_trait   RefTraitInfo
    case dyn_trait   DynTraitInfo
    case type        void
    case void        void
    case never       void

type FnParamsTuple[Sig FuncSig] const:
    num_params := Sig.num_params()
    fields := [num_params]meta.StructField({name='', type=void, offset=0, state_offset=0})
    for 0..Sig.num_params() |i|:
        param := Sig.param_at(i)
        fields[i] = {
            name = 'p' + str(i),
            type = param.type,
            offset = 0,
            state_offset = 0,
        }
    return meta.new_struct(fields, {tuple=true})
