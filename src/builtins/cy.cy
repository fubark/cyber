var .Success      int = 0
var .Await        int = 0
var .ErrorCompile int = 2
var .ErrorPanic   int = 3

--| Evaluates source code in an isolated VM.
--| If the last statement is an expression, a primitive or a String can be returned.
func eval(src String) any:
    var vm = VM.new()  
    var res = vm.eval(src)
    if res.code != Success:
        if res.code == ErrorCompile:
            var report = vm.getErrorSummary()
            eprint(report)
        else res.code == ErrorPanic:
            var report = vm.getPanicSummary()
            eprint(report)
        else:
            eprint('Unknown error')
        throw error.EvalError

    return res.value.toHost()

--| Parses Cyber source string into a structured map object.
--| Currently, only metadata about static declarations is made available but this will be extended to include an AST.
@host func parse(src String) Map

--| Parses a CYON string into a value.
@host func parseCyon(src String) any

--| Starts an isolated REPL session.
--| The callback `read_line(prefix String) String` is responsible for obtaining the input.
@host func repl(read_line any) void

--| Encodes a value to CYON string.
@host func toCyon(val any) String

type Backend enum:
    case vm
    case jit
    case tcc
    case cc
    case llvm

@host type EvalConfig:
    single_run         bool
    file_modules       bool
    gen_all_debug_syms bool
    backend            Backend
    reload             bool
    spawn_exe          bool

@host type EvalResult:
    code  int
    value Value

@host type Value _:
    @host func toHost() any

@host type VM _:
    @host func eval(code String) EvalResult

    @host='VM.eval2'
    func eval(uri String, code String, config EvalConfig) EvalResult

    @host func getErrorSummary() String
    @host func getPanicSummary() String

--| Create an isolated VM.
@host func VM.new() VM