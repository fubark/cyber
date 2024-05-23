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
    var ctx = ReplContext.new()
    ctx.printIntro()
    try:
        while ctx.read(read_line, false) -> code:
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
    @host func dump() String
    @host func getTypeId() int
    @host func toHost() any

@host type VM _:
    @host func eval(code String) EvalResult

    @host='VM.eval2'
    func eval(uri String, code String, config EvalConfig) EvalResult

    @host func getErrorSummary() String
    @host func getPanicSummary() String

--| Create an isolated VM.
@host func VM.new() VM

type ReplContext:
    vm     VM
    indent int

    -- Build multi-line input.
    input_buffer String

    func printIntro():
        print "$(#build_full_version) REPL"
        print "Commands: .exit"

    func read(read_line any, async bool) ?String:
        while:
            var prefix = self.getPrefix()
            var input = ''
            if async:
                var f = read_line(prefix) as Future[any]
                input = (await f) as String
            else:
                input = read_line(prefix) as String

            if input == '.exit':
                return none

            if input.endsWith(':'):
                self.input_buffer += input
                indent += 1
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

    func evalPrint(code String) void:
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

    func getPrefix() String:
        var head = if (self.indent == 0) '> ' else '| '
        var s = ' '.repeat(self.indent * 4)
        return s + head

func ReplContext.new() ReplContext:
    var ctx = ReplContext{
        vm: VM.new(),
        indent: 0,
        input_buffer: '',
    }

    -- TODO: Record inputs that successfully compiled. Can then be exported to file.

    -- Initial input includes `use $global`.
    -- Can also include additional source if needed.
    var init_src = 'use $global'
    var res = ctx.vm.eval(init_src)
    return ctx
