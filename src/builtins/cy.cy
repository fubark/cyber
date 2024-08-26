var .Success      int = 0
var .Await        int = 1
var .ErrorCompile int = 2
var .ErrorPanic   int = 3

var .TypeVoid     int = 0

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
func repl(read_line any) void:
    var ctx = REPL.new()
    ctx.printIntro()
    try:
        while ctx.read(read_line) -> code:
            ctx.evalPrint(code)
    catch e:
        if e != error.EndOfStream:
            throw e

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
    @host func dump(self) String
    @host func getTypeId(self) int
    @host func toHost(self) any

@host type VM _:
    @host func eval(self, code String) EvalResult

    @host='VM.eval2'
    func eval(self, uri String, code String, config EvalConfig) EvalResult

    @host func getErrorSummary(self) String
    @host func getPanicSummary(self) String

--| Create an isolated VM.
@host func VM.new() VM

type REPL:
    vm     VM
    indent int

    -- Build multi-line input.
    input_buffer String

    func printIntro(self):
        print "$(#build_full_version) REPL"
        print "Commands: .exit"

    func read(self, read_line dyn) ?String:
        while:
            var prefix = self.getPrefix()
            var str_or_future = read_line(prefix)
            var input = (await str_or_future) as String

            if input == '.exit':
                return none

            if input.endsWith(':'):
                self.input_buffer += input
                self.indent += 1
                continue

            if self.input_buffer.len() == 0:
                return input

            if input.len() == 0:
                self.indent -= 1
                if self.indent > 0:
                    continue
                else:
                    -- Build input and submit.
                    input = self.input_buffer
                    self.input_buffer = ''
                    return input
            else:
                self.input_buffer += "\n"
                self.input_buffer += ' '.repeat(self.indent * 4)
                self.input_buffer += input
                continue

    func evalPrint(self, code String) void:
        var res = self.vm.eval(code)
        if res.code != Success:
            if res.code == ErrorCompile:
                var report = self.vm.getErrorSummary()
                eprint(report)
            else res.code == ErrorPanic:
                var report = self.vm.getPanicSummary()
                eprint(report)
            else:
                eprint('Unknown error')
            return

        if res.value.getTypeId() != TypeVoid:
            print res.value.dump()

    func getPrefix(self) String:
        var head = if (self.indent == 0) '> ' else '| '
        var s = ' '.repeat(self.indent * 4)
        return s + head

func REPL.new() REPL:
    var ctx = REPL{
        vm = VM.new(),
        indent = 0,
        input_buffer = '',
    }

    -- TODO: Record inputs that successfully compiled. Can then be exported to file.

    -- Initial input includes `use $global`.
    -- Can also include additional source if needed.
    var init_src = 'use $global'
    var res = ctx.vm.eval(init_src)
    return ctx
