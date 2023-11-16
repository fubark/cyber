#!cyber

import os

type ModulePair object:
    var path    string
    var section string

var modules = [
    [ModulePair path: '../src/builtins/builtins.cy', section: 'builtins'],
    [ModulePair path: '../src/builtins/math.cy', section: 'math'],
    [ModulePair path: '../src/std/os.cy', section: 'os'],
    [ModulePair path: '../src/std/test.cy', section: 'test'],
]

var curDir = os.dirName(#ModUri)
-- var md = os.readFile('$(curDir)/../modules.md')
var md = os.readFile('$(curDir)/hugo/content/docs/toc/modules.md')

for modules -> mod:
    var src = os.readFile('$(curDir)/$(mod.path)')
    var decls = parseCyber(src)['decls']
    var gen = '\n'
    for decls -> decl:
        switch decl.type:
        case 'funcInit':
            var docLine = decl.docs ? decl.docs else ''
            var params = []
            for decl.header.params -> param:
                var typeSpec = (param.typeSpec != '') ? param.typeSpec else 'any'
                params.append('$(param.name) $(typeSpec)')
            var paramsStr = params.joinString(', ')
            gen = gen + '{{<cy "func $(decl.header.name)($(paramsStr)) $(decl.header.ret)">}}$(docLine){{</cy>}}\n'
        case 'variable':
            var docLine = decl.docs ? decl.docs else ''
            var typeSpec = (decl.typeSpec != '') ? decl.typeSpec else 'any'
            var idx = decl.name.findRune(0u'.')
            gen = gen + '{{<cy "var $(decl.name[idx+1..]) $(typeSpec)">}}$(docLine){{</cy>}}\n'
        case 'object':
            gen = gen + '### `type $(decl.name)`\n\n'
            for decl.children -> child:
                if child.type == 'funcInit':
                    var docLine = child.docs ? child.docs else ''
                    var params = []
                    for child.header.params -> param:
                        var typeSpec = (param.typeSpec != '') ? param.typeSpec else 'any'
                        params.append('$(param.name) $(typeSpec)')
                    var paramsStr = params.joinString(', ')
                    gen = gen + '{{<cy "func $(child.header.name)($(paramsStr)) $(child.header.ret)">}}$(docLine){{</cy>}}\n'

    -- Replace section in modules.md.
    var needle = '<!-- $(mod.section).start -->'
    var startIdx = md.find(needle) + needle.len()
    var endIdx = md.find('<!-- $(mod.section).end -->')
    md = md[0..startIdx] + gen + md[endIdx..]

os.writeFile('$(curDir)/hugo/content/docs/toc/modules.md', md)
print 'Done.'