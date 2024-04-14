#!cyber

-- 1. First generates docs-modules.md from docs.md by patching module sections with
--    API docs extracted from .cy files.
-- 2. Generates single docs.html from docs-modules.md.
--    CSS/JS are also embedded into the docs.html.
-- * docs.md does not handle custom header ids since Github md doesn't support it.
-- * The version string can be overrided with `-version`

use cy
use os
use md '../src/tools/md4c.cy'

let args = os.parseArgs([
    { name: 'version', type: String, default: 'dev' },
    { name: 'import-style', type: bool, default: false },
])

genDocsModules()

var curDir = os.dirName(#modUri).?
var src = os.readFile("$(curDir)/docs-modules.md")
var csrc = os.cstr(src)
var csrcLen = Array(src).len()

var parser = os.malloc(64)
var enterBlock_c = md.ffi.bindCallback(enterBlock, [.int, .voidPtr, .voidPtr], .int)
var leaveBlock_c = md.ffi.bindCallback(leaveBlock, [.int, .voidPtr, .voidPtr], .int)
var enterSpan_c = md.ffi.bindCallback(enterSpan, [.int, .voidPtr, .voidPtr], .int)
var leaveSpan_c = md.ffi.bindCallback(leaveSpan, [.int, .voidPtr, .voidPtr], .int)
var text_c = md.ffi.bindCallback(text, [.int, .voidPtr, .int, .voidPtr], .int)
var nullptr = pointer(0)
parser.set(0, .int, 0)
parser.set(4, .int, md.FLAG_TABLES)
parser.set(8, .voidPtr, enterBlock_c)
parser.set(16, .voidPtr, leaveBlock_c)
parser.set(24, .voidPtr, enterSpan_c)
parser.set(32, .voidPtr, leaveSpan_c)
parser.set(40, .voidPtr, text_c)
parser.set(48, .voidPtr, nullptr)
parser.set(56, .voidPtr, nullptr)

var res = md.md_parse(csrc, csrcLen, parser, pointer(0))
if res != 0:
    print "parse error: $(res)"
    os.exit(1)

var tocLinksHtml = []
for tocLinks -> link:
    tocLinksHtml.append("""<li><a href="$(link.href)">$(link.text)</a></li>""")

var simpleCSS = os.readFile("$(curDir)/simple.css")
var hljsCSS = os.readFile("$(curDir)/github-dark.min.css")
var hljsJS = os.readFile("$(curDir)/highlight.min.js")

var stylePart = '<link rel="stylesheet" href="./style.css">'
if !args['import-style']:
    var styleCSS = os.readFile("$(curDir)/style.css")
    stylePart = "<style>$(styleCSS)</style>"

var toc_links = tocLinksHtml.join("\n")
var html = """<html lang="en">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Cyber Docs</title>
    <style>$(simpleCSS)</style>
    <style>$(hljsCSS)</style>
    $(stylePart)
</head>
<body id="table-of-contents">
<header>
    <h1 class="title">Cyber Docs</h1>
    <div class="sub-title">$(args.version)</div>
    <ul>
        <li><a href="https://cyberscript.dev" target="_blank" rel="noopener">Homepage</a></li>
        <li><a href="https://cyberscript.dev/play.html" target="_blank" rel="noopener">Playground</a></li>
        <li><a href="https://github.com/fubark/cyber/edit/master/docs/docs.md" target="_blank" rel="noopener">Edit Docs</a></li>
    </ul>
    <div class="sub-title">Table of Contents</div>
    <ul>
        $(toc_links)
    </ul>
</header>
<main>
    $(out)
</main>
<script>$(hljsJS)</script>
<script>
hljs.registerLanguage('cy', function() {
    return {
    keywords: {
        keyword: [
            'template', 'func', 'module', 'for', 'coinit', 'coresume', 'coyield', 'use',
            'return', 'if', 'else', 'as', 'while', 'var', 'let', 'dynobject', 'object', 'struct', 'with', 'caught',
            'break', 'continue', 'switch', 'pass', 'or', 'and', 'not', 'is', 'error', 'throws',
            'true', 'false', 'none', 'throw', 'try', 'catch', 'recover', 'enum', 'type', 'case'
        ],
        type: [
            'float', 'String', 'Array', 'bool', 'any', 'int', 'List', 'Map', 'symbol', 'pointer', 'dynamic'
        ],
    },
    contains: [
        {
            scope: 'string',
            begin: "\\"", end: "\\""
        },
        {
            scope: 'string',
            begin: "'", end: "'"
        },
        /*
        {
            scope: 'symbol',
            begin: '.', end: /\\w(?=[^\\w])/
        },*/
        hljs.COMMENT(
        '\\-\\-', // begin
        '\\n', // end
        {
            contains: [
            ]
        }
        ),
        hljs.C_NUMBER_MODE,
    ]
    };
})
hljs.highlightAll();
</script>
</body>
</html>"""
-- print out
print 'Done.'
os.writeFile("$(curDir)/docs.html", html)

var .out = ''
var .htmlContent = ''
var .textContent = ''
var .state = State.main
var .parsingToc = false
var .tocLinks = []
var .bufContent = false
var .lastTopicId = ''
var .lastHLevel = 1

-- Maps id names to the next unique count from 1.
-- Mimics Githubs duplicate header id generation.
var .idCounts = Map{}

type State enum:
    case main
    case header
    case html

type Link:
    href  String
    title String
    text  String

func resetState():
    state = State.main
    bufContent = false

func enterBlock(block_t md.BLOCKTYPE, detail_p pointer, userdata pointer) int:
    if parsingToc:
        switch block_t
        case md.BLOCK_HTML:
            textContent = ''
            bufContent = true
            return 0
        case md.BLOCK_UL: return 0
        case md.BLOCK_LI: return 0
        else:
            print "unsupported enter block $(block_t)"
            return 1
        return 0

    switch block_t
    case md.BLOCK_DOC: 
        return 0
    case md.BLOCK_QUOTE:
        out += '<blockquote>'
        return 0 
    case md.BLOCK_TABLE:
        out += '<table>'
        return 0 
    case md.BLOCK_THEAD:
        out += '<thead>'
        return 0 
    case md.BLOCK_TBODY:
        out += '<tbody>'
        return 0 
    case md.BLOCK_TR:
        out += '<tr>'
        return 0 
    case md.BLOCK_TH:
        out += '<th>'
        return 0 
    case md.BLOCK_TD:
        out += '<td>'
        return 0 
    case md.BLOCK_H:
        textContent = ''
        htmlContent = ''
        state = State.header
        bufContent = true
        return 0
    case md.BLOCK_P:
        out += '<p>'
        return 0
    case md.BLOCK_CODE:
        let detail = md.lib.ptrToBLOCK_CODE_DETAIL_S(detail_p)
        var lang = getAttrText(detail.lang)
        out += """<pre><code class="language-$(lang)">"""
        return 0
    case md.BLOCK_UL:
        out += "<ul>\n"
        return 0
    case md.BLOCK_OL:
        out += "<ol>\n"
        return 0
    case md.BLOCK_LI:
        out += '<li>'
        return 0
    case md.BLOCK_HTML:
        return 0
    else:
        print "unsupported enter block $(block_t)"
        return 1

func leaveBlock(block_t md.BLOCKTYPE, detail_p pointer, userdata pointer) int:
    if parsingToc:
        if block_t == md.BLOCK_HTML:
            if textContent.startsWith('<!--') and !isNone(textContent.find('TOC-END')):
                parsingToc = false
            resetState()
        return 0

    switch block_t
    case md.BLOCK_DOC:
        return 0
    case md.BLOCK_QUOTE:
        out += '</blockquote>'
        return 0 
    case md.BLOCK_TABLE:
        out += '</table>'
        return 0 
    case md.BLOCK_THEAD:
        out += '</thead>'
        return 0 
    case md.BLOCK_TBODY:
        out += '</tbody>'
        return 0 
    case md.BLOCK_TR:
        out += '</tr>'
        return 0 
    case md.BLOCK_TH:
        out += '</th>'
        return 0 
    case md.BLOCK_TD:
        out += '</td>'
        return 0 
    case md.BLOCK_H:
        let detail = md.lib.ptrToBLOCK_H_DETAIL_S(detail_p)
        textContent = textContent.trim(.ends, " \n")

        let id = textContent.replace(' ', '-')
        id = id.replace('.', '')
        id = id.replace('/', '')
        id = id.replace('$', '')
        id = id.lower()
        if idCounts.get(id) -> count:
            var newId = "$(id)-$(count)"
            idCounts[id] += 1
            id = newId
        else:
            idCounts[id] = 1

        if id == 'table-of-contents':
            parsingToc = true
        else:
            if lastHLevel > 1:
                out += """<a href="#$(lastTopicId)">^topic</a>\n"""
            if detail.level == 1:
                lastTopicId = id
            lastHLevel = detail.level
            out += """<h$(detail.level) id="$(id)">$(htmlContent) <a href="#$(id)">#</a></h$(detail.level)>\n"""

        resetState()
        return 0
    case md.BLOCK_P:
        out += "</p>\n"
        return 0
    case md.BLOCK_CODE:
        out += "</code></pre>\n"
        return 0
    case md.BLOCK_UL:
        out += "</ul>\n"
        return 0
    case md.BLOCK_OL:
        out += "</ol>\n"
        return 0
    case md.BLOCK_LI:
        out += "</li>\n"
        return 0
    case md.BLOCK_HTML:
        return 0
    else:
        print "unsupported leave block $(block_t)"
        return 1

func enterSpan(span_t md.SPANTYPE, detail_p pointer, userdata pointer) int:
    switch span_t
    case md.SPAN_EM:
        out += '<em>'
        return 0
    case md.SPAN_STRONG:
        out += '<strong>'
        return 0
    case md.SPAN_CODE:
        if bufContent:
            htmlContent += '<code>'
        else:
            out += '<code>'
        return 0
    case md.SPAN_A:
        let detail = md.lib.ptrToSPAN_A_DETAIL_S(detail_p)

        var href = getAttrText(detail.href)
        var title = getAttrText(detail.title)

        if parsingToc:
            tocLinks.append(Link{href: href, title: title, text: ''})
            textContent = ''
            bufContent = true
            return 0

        out += """<a href="$(href)" title="$(title)">"""
        return 0
    else:
        print "unsupported enter span $(span_t)"
        return 1

func leaveSpan(span_t md.SPANTYPE, detail pointer, userdata pointer) int:
    switch span_t
    case md.SPAN_EM:
        out += '</em>'
        return 0
    case md.SPAN_STRONG:
        out += '</strong>'
        return 0
    case md.SPAN_CODE:
        if bufContent:
            htmlContent += '</code>'
        else:
            out += '</code>'
        return 0
    case md.SPAN_A:
        if parsingToc:
            bufContent = false
            tocLinks[tocLinks.len()-1].text = textContent
            return 0

        out += '</a>'
        return 0
    else:
        print "unsupported leave span $(span_t)"
        return 1

func text(text_t md.SPANTYPE, ptr pointer, len int, userdata pointer) int:
    var str = ptr.toArray(0, len).decode()
    if bufContent:
        textContent += str
        htmlContent += str
    else:
        out += str
    return 0

func getAttrText(attr dynamic) String:
    if attr.size == 0:
        return ''
    return (attr.text as pointer).toArray(0, attr.size).decode()

type ModulePair:
    path    String
    section String

func genDocsModules():
    var modules = [
        ModulePair{path: '../src/builtins/builtins_vm.cy', section: 'core'},
        ModulePair{path: '../src/builtins/cy.cy', section: 'cy'},
        ModulePair{path: '../src/builtins/math.cy', section: 'math'},
        ModulePair{path: '../src/std/cli.cy', section: 'cli'},
        ModulePair{path: '../src/std/os.cy', section: 'os'},
        ModulePair{path: '../src/std/test.cy', section: 'test'},
    ]

    var curDir = os.dirName(#modUri).?
    -- var md = os.readFile("$(curDir)/../modules.md")
    var md = os.readFile("$(curDir)/docs.md")

    for modules -> mod:
        var src = os.readFile("$(curDir)/$(mod.path)")
        var decls = cy.parse(src)['decls']
        var gen = "\n"
        for decls -> decl:
            switch decl['type']
            case 'funcInit', 'func':
                var docLine = decl.get('docs') ?else ''
                var params = []
                for decl['header']['params'] -> param:
                    var typeSpec = if (param['typeSpec'] != '') param['typeSpec'] else 'any'
                    params.append("$(param['name']) $(typeSpec)")
                var paramsStr = params.join(', ')
                gen = gen + "> `func $(decl['header']['name'])($(paramsStr)) $(decl['header']['ret'])`\n>\n>$(docLine)\n\n"
            case 'variable':
                var docLine = decl.get('docs') ?else ''
                var typeSpec = if (decl['typeSpec'] != '') decl['typeSpec'] else 'any'
                gen = gen + "> `var $(decl['name']) $(typeSpec)`\n>\n>$(docLine)\n\n"
            case 'distinct_t':
                gen = gen + "### `type $(decl['name'])`\n\n"
            case 'object':
                gen = gen + "### `type $(decl['name'])`\n\n"
            case 'struct_t':
                gen = gen + "### `type $(decl['name']) struct`\n\n"
            case 'implicit_method':
                var docLine = decl.get('docs') ?else ''
                var params = []
                for decl['header']['params'] -> param:
                    var typeSpec = if (param['typeSpec'] != '') param['typeSpec'] else 'any'
                    params.append("$(param['name']) $(typeSpec)")
                var paramsStr = params.join(', ')
                gen = gen + "> `func $(decl['header']['name'])($(paramsStr)) $(decl['header']['ret'])`\n>\n>$(docLine)\n\n"

        -- Replace section in modules.md.
        var needle = "<!-- $(mod.section).start -->"
        var startIdx = md.find(needle).? + needle.len()
        var endIdx = md.find("<!-- $(mod.section).end -->").?
        md = md[0..startIdx] + gen + md[endIdx..]

    os.writeFile("$(curDir)/docs-modules.md", md)
