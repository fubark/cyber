#!cyber
import os 'os'

POST_HEADER = "
"

args = os.parseArgs([
    { name: 'update', type: string, default: none }
    { name: 'libpath', type: string, default: 'lib.dll' }
])

skipMap = {}
existingLibPath = false
if args.update != none:
    -- Build skip map.
    existing = readFile(args.update)

    markerPos = existing.find('\n-- CBINDGEN MARKER')
    if markerPos == none:
        markerPos = existing.len()

    res = parseCyber(existing)
    for res.decls each decl:
        if decl.pos < markerPos:
            if decl.type == 'func':
                skipMap[decl.name] = true
            else decl.type == 'funcInit':
                skipMap[decl.name] = true
            else decl.type == 'object':
                skipMap[decl.name] = true
            else decl.type == 'variable':
                if decl.name == 'libPath':
                    existingLibPath = true

headerPath = args.rest[2]

headerSrc = readFile(headerPath).utf8()
writeFile('temp.h', headerSrc.concat(POST_HEADER))

res = execCmd(['/bin/bash', '-c', 'zig translate-c temp.h'])
zigSrc = res.out
writeFile('bindings.zig', zigSrc)

-- Build output string.
out = ''

-- State.
var structMap: {}    -- structName -> list of fields (ctype syms)
var structs: []
var aliases: {}      -- aliasName -> structName or binding symbol (eg: #voidPtr)
var arrays: {}       -- [n]typeName -> true

state = #parseFirst
vars = {}            -- varName -> bindingType
funcs = []
skipFields = false

lines = zigSrc.split('\n')
for lines each line:
    if state == #parseFirst:
        if line.startsWith('pub const '):
            line = line['pub const '.len()..]
            if line.startsWith('__'):
                -- Skip internal defs.
                continue
            if line.startsWith('@'):
                -- Skip zig keywords idents.
                continue

            assignIdx = line.findRune(0u'=')

            beforeAssign = line[..assignIdx-1]
            idx = beforeAssign.findRune(0u':')
            if idx == none:
                name = beforeAssign
                init = line[assignIdx+2..]
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
                    structName = name['struct_'.len()..]
                    if init == 'opaque \{\};':
                        out = out.concat('type {structName} pointer\n')
                        aliases[structName] = #voidPtr
                    else:
                        state = #parseField
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
                    out = out.concat('type {name} number\n')
                    aliases[name] = #uint
                    continue
                else init.startsWith('?*const'):
                    -- Function pointer type alias.
                    out = out.concat('type {name} pointer\n')
                    aliases[name] = #voidPtr
                    continue
                else init.startsWith('"'):
                    -- String constant.
                    right = init.trim(#right, ';')
                    out = out.concat('var {name}: {right}\n')
                    continue
                else:
                    -- Type alias.
                    right = init.trim(#right, ';')
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
                name = beforeAssign[..idx]
                spec = beforeAssign[idx+2..]
                init = line[assignIdx+2..line.len()-1]
                if spec == 'c_int':
                    out = out.concat('var {name} number: {init}\n')
                    vars[name] = #int
        else line.startsWith('pub extern fn '):
            line = line['pub extern fn '.len()..]

            -- Parse func signature.

            -- Find start.
            idx = line.findRune(0u'(')
            name = line[..idx]
            line = line[idx+1..]

            -- Find end.
            idx = line.findRune(0u')')
            paramsStr = line[..idx]
            ret = line[idx+2..].trim(#right, ';')

            outFunc = 'func {name}('

            fn = {}
            fn['name'] = name

            -- Parse params.
            fnParamTypes = []
            params = paramsStr.split(', ')
            for params each param:
                if param.len() == 0:
                    continue
                if param == '...':
                    -- varargs unsupported
                    outFunc = none
                    break

                idx = param.findRune(0u':')
                paramName = param[..idx]
                if paramName.startsWith('@"'):
                    paramName = paramName[2..paramName.len()-1]
                paramSpec = param[idx+2..]

                paramType = getBinding(paramSpec)
                if paramType == none:
                    outFunc = none
                    break

                outFunc = outFunc.concat('{paramName} {getCyName(paramType)}, ')
                fnParamTypes.append(paramType)

            if outFunc != none:
                if fnParamTypes.len() > 0:
                    outFunc = outFunc[..outFunc.len()-2]

                outFunc = outFunc.concat(') ')

                retType = getBinding(ret)
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
    else state == #parseField:
        if line == '};':
            state = #parseFirst
            out = out.concat('\n')
            skipFields = false
            continue

        line = line.trim(#ends, ' ,')
        idx = line.findRune(0u':')
        name = line[..idx]
        spec = line[idx+2..]

        fieldType = getBinding(spec)
        if fieldType != none:
            if skipFields:
                out = out.concat('-- ')
            if fieldType == #voidPtr:
                out = out.concat('  {name} pointer -- {spec}\n')
            else typesym(fieldType) == #string and fieldType.startsWith('os.CArray'):
                out = out.concat('  {name} List -- {spec}\n')
            else:
                out = out.concat('  {name} {getCyName(fieldType)}\n')
            structFields.append(fieldType)

-- Generate bindLib.

out = out.concat("\nimport os \'os\'\n")
out = out.concat("var lib: createLib()\n")
out = out.concat("func createLib():\n")
out = out.concat("  decls = []\n")
for funcs each fn:
    out = out.concat("  decls.append(os.CFunc\{ sym: '{fn.name}', args: [{fn.params.joinString(', ')}], ret: {fn.ret} \})\n")
for structs each name:
    fields = structMap[name]
    out = out.concat("  decls.append(os.CStruct\{ fields: [{fields.joinString(', ')}], type: {name} \})\n")

-- libPath = if existingLibPath then 'libPath' else "'{args.libpath}'"
libPath = 'libPath'
out = out.concat("  return os.bindLib({libPath}, decls, \{ genMap: true \})")

-- Final output.
if args.update != none:
    out = existing[0..markerPos].concat('\n-- CBINDGEN MARKER\n').concat(out)
writeFile('bindings.cy', out)

func getCyName(nameOrSym):
    if typesym(nameOrSym) == #symbol:
        sym = nameOrSym
        if sym == #voidPtr:
            return 'pointer'
        else sym == #bool:
            return 'boolean'
        else sym == #int:
            return 'number'
        else sym == #uint:
            return 'number'
        else sym == #uchar:
            return 'number'
        else sym == #long:
            return 'number'
        else sym == #float:
            return 'number'
        else sym == #double:
            return 'number'
        else sym == #voidPtr:
            return 'pointer'
        else sym == #void:
            return 'none'
        else:
            print 'TODO getCyName {nameOrSym}'
            return 'TODO'
    else:
        return nameOrSym

func getBinding(spec):
    if spec == 'void':
        return #void
    else spec == 'bool':
        return #bool
    else spec == 'c_int':
        return #int
    else spec == 'c_uint':
        return #uint
    else spec == 'u8':
        return #uchar
    else spec == 'f32':
        return #float
    else spec == 'f64':
        return #double
    else spec == 'c_long':
        return #long
    else spec.startsWith('?*'):
        return #voidPtr
    else spec.startsWith('[*c]'):
        return #voidPtr
    else spec.startsWith('['):
        idx = spec.findRune(0u']')
        n = spec[1..idx]
        elem = getBinding(spec[idx+1..])
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