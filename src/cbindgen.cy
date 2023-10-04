#!cyber
import os

var POST_HEADER = "
"

var args = os.parseArgs([
    { name: 'update', type: string, default: none }
    { name: 'libpath', type: string, default: 'lib.dll' }
])

var skipMap = {}
var existingLibPath = false
var markerPos = none
var existing = none
if args.update != none:
    -- Build skip map.
    existing = os.readFile(args.update)

    markerPos = existing.find('\n-- CBINDGEN MARKER')
    if markerPos == none:
        markerPos = existing.len()

    var res = parseCyber(existing)
    for res.decls each decl:
        if decl.pos < markerPos:
            match decl.type:
            case 'func':
                skipMap[decl.name] = true
            case 'funcInit':
                skipMap[decl.name] = true
            case 'object':
                skipMap[decl.name] = true
            case 'variable':
                if decl.name == 'libPath':
                    existingLibPath = true

var headerPath = args.rest[2]

var headerSrc = os.readFile(headerPath).utf8()
os.writeFile('temp.h', headerSrc.concat(POST_HEADER))

var res = os.execCmd(['/bin/bash', '-c', 'zig translate-c temp.h'])
var zigSrc = res.out
os.writeFile('bindings.zig', zigSrc)

-- Build output string.
var out = ''

-- State.
var structMap: {}    -- structName -> list of fields (ctype syms)
var structs: []
var aliases: {}      -- aliasName -> structName or binding symbol (eg: .voidPtr)
var arrays: {}       -- [n]typeName -> true

var state = .parseFirst
var vars = {}            -- varName -> bindingType
var funcs = []
var skipFields = false
var structFields = []

var lines = zigSrc.split('\n')
for lines each line:
    if state == .parseFirst:
        if line.startsWith('pub const '):
            line = line['pub const '.len()..]
            if line.startsWith('__'):
                -- Skip internal defs.
                continue
            if line.startsWith('@'):
                -- Skip zig keywords idents.
                continue

            var assignIdx = line.findRune(0u'=')

            var beforeAssign = line[..assignIdx-1]
            var idx = beforeAssign.findRune(0u':')
            if idx == none:
                var name = beforeAssign
                var init = line[assignIdx+2..]
                if init.startsWith('@'):
                    -- Skip `= @import`
                    continue
                if init.startsWith('struct_'):
                    -- Skip `Vector2 = struct_Vector2`
                    continue
                if init.startsWith('__'):
                    -- Skip `va_list = __builtin_va_list`
                    continue
                if init == '"";':
                    -- Skip empty defines `RAYLIB_H = "";`
                    continue

                if name.startsWith('struct_'):
                    var structName = name['struct_'.len()..]
                    if init == 'opaque \{\};':
                        out = out.concat('type {structName} pointer\n')
                        aliases[structName] = .voidPtr
                    else:
                        state = .parseField
                        if skipMap[structName]:
                            skipFields = true
                            out = out.concat('-- Skip type {structName} object:\n')
                        else:
                            out = out.concat('type {structName} object:\n')
                        structFields = []
                        structMap[structName] = structFields
                        structs.append(structName)
                    continue

                if init == 'c_uint;':
                    out = out.concat('type {name} int\n')
                    aliases[name] = .uint
                    continue
                else init.startsWith('?*const'):
                    -- Function pointer type alias.
                    out = out.concat('type {name} pointer\n')
                    aliases[name] = .voidPtr
                    continue
                else init.startsWith('"'):
                    -- String constant.
                    var right = init.trim(.right, ';')
                    out = out.concat('var {name}: {right}\n')
                    continue
                else:
                    -- Type alias.
                    var right = init.trim(.right, ';')
                    if structMap[right]:
                        -- Found existing type.
                        out = out.concat('type {name} {right}\n')
                        aliases[name] = right
                    else vars[right]:
                        -- Found existing var.
                        out = out.concat('var {name} {getCyName(vars[right])}: {right}\n')
                    else:
                        print 'TODO {line}'
            else:
                -- Has var specifier.
                var name = beforeAssign[..idx]
                var spec = beforeAssign[idx+2..]
                var init = line[assignIdx+2..line.len()-1]
                if spec == 'c_int':
                    out = out.concat('var {name} int: {init}\n')
                    vars[name] = .int
        else line.startsWith('pub extern fn '):
            line = line['pub extern fn '.len()..]

            -- Parse func signature.

            -- Find start.
            var idx = line.findRune(0u'(')
            var name = line[..idx]
            line = line[idx+1..]

            -- Find end.
            idx = line.findRune(0u')')
            var paramsStr = line[..idx]
            var ret = line[idx+2..].trim(.right, ';')

            var outFunc = 'func {name}('

            var fn = {}
            fn['name'] = name

            -- Parse params.
            var fnParamTypes = []
            var params = paramsStr.split(', ')
            for params each param:
                if param.len() == 0:
                    continue
                if param == '...':
                    -- varargs unsupported
                    outFunc = none
                    break

                idx = param.findRune(0u':')
                var paramName = param[..idx]
                if paramName.startsWith('@"'):
                    paramName = paramName[2..paramName.len()-1]
                var paramSpec = param[idx+2..]

                var paramType = getBinding(paramSpec)
                if paramType == none:
                    outFunc = none
                    break

                outFunc = outFunc.concat('{paramName} {getCyName(paramType)}, ')
                fnParamTypes.append(paramType)

            if outFunc != none:
                if fnParamTypes.len() > 0:
                    outFunc = outFunc[..outFunc.len()-2]

                outFunc = outFunc.concat(') ')

                var retType = getBinding(ret)
                if retType != none:
                    outFunc = outFunc.concat('{getCyName(retType)} = lib.{name}')
                    if skipMap[name]:
                        outFunc = '-- Skip '.concat(outFunc)

                    out = out.concat('{outFunc}\n')

                    fn['params'] = fnParamTypes
                    fn['ret'] = retType
                    funcs.append(fn)
        else:
            -- print 'TODO {line}'
            pass
    else state == .parseField:
        if line == '};':
            state = .parseFirst
            out = out.concat('\n')
            skipFields = false
            continue

        line = line.trim(.ends, ' ,')
        var idx = line.findRune(0u':')
        var name = line[..idx]
        var spec = line[idx+2..]

        var fieldType = getBinding(spec)
        if fieldType != none:
            if skipFields:
                out = out.concat('-- ')
            if fieldType == .voidPtr:
                out = out.concat('  {name} pointer -- {spec}\n')
            else typesym(fieldType) == .string and fieldType.startsWith('os.CArray'):
                out = out.concat('  {name} List -- {spec}\n')
            else:
                out = out.concat('  {name} {getCyName(fieldType)}\n')
            structFields.append(fieldType)

-- Generate bindLib.

out = out.concat("\nimport os\n")
out = out.concat("var lib: createLib()\n")
out = out.concat("func createLib():\n")
out = out.concat("  var decls = []\n")
for funcs each fn:
    out = out.concat("  decls.append(os.CFunc\{ sym: '{fn.name}', args: [{fn.params.joinString(', ')}], ret: {fn.ret} \})\n")
for structs each name:
    var fields = structMap[name]
    out = out.concat("  decls.append(os.CStruct\{ fields: [{fields.joinString(', ')}], type: {name} \})\n")

-- libPath = if existingLibPath then 'libPath' else "'{args.libpath}'"
var libPath = 'libPath'
out = out.concat("  return os.bindLib({libPath}, decls, \{ genMap: true \})")

-- Final output.
if args.update != none:
    out = existing[0..markerPos].concat('\n-- CBINDGEN MARKER\n').concat(out)

os.writeFile('bindings.cy', out)

func getCyName(nameOrSym):
    if typesym(nameOrSym) == .symbol:
        match nameOrSym:
        case .voidPtr:
            return 'pointer'
        case .bool:
            return 'boolean'
        case .int:
            return 'int'
        case .uint:
            return 'int'
        case .uchar:
            return 'int'
        case .long:
            return 'int'
        case .float:
            return 'float'
        case .double:
            return 'float'
        case .voidPtr:
            return 'pointer'
        case .void:
            return 'none'
        else:
            print 'TODO getCyName {nameOrSym}'
            return 'TODO'
    else:
        return nameOrSym

func getBinding(spec):
    match spec:
    case 'void':
        return .void
    case 'bool':
        return .bool
    case 'c_int':
        return .int
    case 'c_uint':
        return .uint
    case 'u8':
        return .uchar
    case 'f32':
        return .float
    case 'f64':
        return .double
    case 'c_long':
        return .long
    else:
        if spec.startsWith('?*'):
            return .voidPtr
        else spec.startsWith('[*c]'):
            return .voidPtr
        else spec.startsWith('['):
            var idx = spec.findRune(0u']')
            var n = spec[1..idx]
            var elem = getBinding(spec[idx+1..])
            if elem == none:
                print 'TODO getBinding {spec}'
                return none
            return 'os.CArray\{ n: {n}, elem: {elem}\}'
        else:
            if structMap[spec]:
                -- Valid type.
                return spec
            else aliases[spec]:
                -- Valid alias.
                return aliases[spec]
            else:
                print 'TODO getBinding {spec}'
                return none