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
print md.len()

for modules each mod:
  var src = os.readFile('{curDir}/{mod.path}')
  var decls = parseCyber(src)['decls']
  var gen = '\n'
  for decls each decl:
    match decl.type:
      'funcInit':
        -- Two spaces for new line.
        var docLine = if decl.docs then '> {decl.docs}\n\n' else '\n'
        var params = []
        for decl.header.params each param:
          var typeSpec = if param.typeSpec != '' then param.typeSpec else 'any'
          params.append('{param.name} {typeSpec}')
        var paramsStr = params.joinString(', ')
        gen = gen + '> `func {decl.header.name}({paramsStr}) {decl.header.ret}`  \n{docLine}'
      'variable':
        var docLine = if decl.docs then '> {decl.docs}\n\n' else '\n'
        var typeSpec = if decl.typeSpec != '' then decl.typeSpec else 'any'
        gen = gen + '> `var {decl.name} {typeSpec}`  \n{docLine}'
      'object':
        gen = gen + '### `type {decl.name}`\n\n'
        for decl.children each child:
          if child.type == 'funcInit':
            var docLine = if child.docs then '> {child.docs}\n\n' else '\n'
            var params = []
            for child.header.params each param:
              var typeSpec = if param.typeSpec != '' then param.typeSpec else 'any'
              params.append('{param.name} {typeSpec}')
            var paramsStr = params.joinString(', ')
            gen = gen + '> `func {child.header.name}({paramsStr}) {child.header.ret}`  \n{docLine}'

  -- Replace section in modules.md.
  var needle = '<!-- {mod.section}.start -->'
  var startIdx = md.find(needle) + needle.len()
  var endIdx = md.find('<!-- {mod.section}.end -->')
  md = md[0..startIdx] + gen + md[endIdx..]

print md.len()
os.writeFile('{curDir}/hugo/content/docs/toc/modules.md', md)
print 'Done.'