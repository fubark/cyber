#!cyber

import os

type ModulePair object:
    path    string
    section string

var modules = [
    ModulePair{ path: '../src/builtins/builtins.cy', section: 'builtins' }
    ModulePair{ path: '../src/builtins/math.cy', section: 'math' }
    ModulePair{ path: '../src/std/os.cy', section: 'os' }
    ModulePair{ path: '../src/std/test.cy', section: 'test' }
]

var curDir = os.dirName(#ModUri)
-- var md = os.readFile('{curDir}/../modules.md')
var md = os.readFile('{curDir}/hugo/content/docs/toc/modules.md')

for modules each mod:
    var src = os.readFile('{curDir}/{mod.path}')
    var decls = parseCyber(src)['decls']
    var gen = '\n'
    for decls each decl:
        match decl.type:
        case 'funcInit':
            -- Two spaces for new line.
            var docLine = '> {decl.docs}\n\n' if decl.docs else '\n'
            var params = []
            for decl.header.params each param:
                var typeSpec = param.typeSpec if param.typeSpec != '' else 'any'
                params.append('{param.name} {typeSpec}')
            var paramsStr = params.joinString(', ')
            gen = gen + '> `func {decl.header.name}({paramsStr}) {decl.header.ret}`  \n{docLine}'
        case 'variable':
            var docLine = '> {decl.docs}\n\n' if decl.docs else '\n'
            var typeSpec = decl.typeSpec if decl.typeSpec != '' else 'any'
            gen = gen + '> `var {decl.name} {typeSpec}`  \n{docLine}'
        case 'object':
            gen = gen + '### `type {decl.name}`\n\n'
            for decl.children each child:
                if child.type == 'funcInit':
                    var docLine = '> {child.docs}\n\n' if child.docs else '\n'
                    var params = []
                    for child.header.params each param:
                        var typeSpec = param.typeSpec if param.typeSpec != '' else 'any'
                        params.append('{param.name} {typeSpec}')
                    var paramsStr = params.joinString(', ')
                    gen = gen + '> `func {child.header.name}({paramsStr}) {child.header.ret}`  \n{docLine}'

    -- Replace section in modules.md.
    var needle = '<!-- {mod.section}.start -->'
    var startIdx = md.find(needle) + needle.len()
    var endIdx = md.find('<!-- {mod.section}.end -->')
    md = md[0..startIdx] + gen + md[endIdx..]

os.writeFile('{curDir}/hugo/content/docs/toc/modules.md', md)
print 'Done.'