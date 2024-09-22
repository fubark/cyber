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

var args = os.parseArgs(.{
    { name='version', type=string, default='DEV' },
    { name='import-style', type=bool, default=false },
})

genDocsModules()

var curDir = os.dirName(#modUri).?
var src = os.readFile("${curDir}/docs-modules.md")
var csrc = os.cstr(src)
var csrcLen = src.len()

var parser = os.malloc(64)
var enterBlock_c = md.ffi.bindCallback(enterBlock, List[dyn]{symbol.int, symbol.voidPtr, symbol.voidPtr}, symbol.int)
var leaveBlock_c = md.ffi.bindCallback(leaveBlock, List[dyn]{symbol.int, symbol.voidPtr, symbol.voidPtr}, symbol.int)
var enterSpan_c = md.ffi.bindCallback(enterSpan, List[dyn]{symbol.int, symbol.voidPtr, symbol.voidPtr}, symbol.int)
var leaveSpan_c = md.ffi.bindCallback(leaveSpan, List[dyn]{symbol.int, symbol.voidPtr, symbol.voidPtr}, symbol.int)
var text_c = md.ffi.bindCallback(text, List[dyn]{symbol.int, symbol.voidPtr, symbol.int, symbol.voidPtr}, symbol.int)
var nullptr = pointer.fromAddr(void, 0)
parser.set(0, .int, 0)
parser.set(4, .int, md.FLAG_TABLES)
parser.set(8, .voidPtr, enterBlock_c)
parser.set(16, .voidPtr, leaveBlock_c)
parser.set(24, .voidPtr, enterSpan_c)
parser.set(32, .voidPtr, leaveSpan_c)
parser.set(40, .voidPtr, text_c)
parser.set(48, .voidPtr, nullptr)
parser.set(56, .voidPtr, nullptr)

var res = md.md_parse(csrc, csrcLen, parser, pointer.fromAddr(void, 0))
if res != 0:
    print "parse error: ${res}"
    os.exit(1)

var tocLinksHtml = List[string]{}
for tocLinks -> link:
    tocLinksHtml.append("<li><a href=\"${link.href}\">${link.text}</a></li>")

var simpleCSS = os.readFile("${curDir}/simple.css")
var hljsCSS = os.readFile("${curDir}/github-dark.min.css")
var hljsJS = os.readFile("${curDir}/highlight.min.js")

var stylePart = '<link rel="stylesheet" href="./style.css">'
if !args['import-style']:
    var styleCSS = os.readFile("${curDir}/style.css")
    stylePart = "<style>${styleCSS}</style>"

var toc_links = tocLinksHtml.join("\n")
var html = """<html lang="en">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Cyber Docs</title>
    <style>${simpleCSS}</style>
    <style>${hljsCSS}</style>
    ${stylePart}
</head>
<body id="table-of-contents">
<header>
    <h1 class="title">Cyber Docs</h1>
    <div class="sub-title">${args.version}</div>
    <ul>
        <li><a href="https://cyberscript.dev" target="_blank" rel="noopener">Homepage</a></li>
        <li><a href="https://cyberscript.dev/play.html" target="_blank" rel="noopener">Playground</a></li>
        <li><a href="https://github.com/fubark/cyber/edit/master/docs/docs.md" target="_blank" rel="noopener">Edit Docs</a></li>
    </ul>
    <div class="sub-title">Table of Contents</div>
    <ul>
        ${toc_links}
    </ul>
</header>
<main>
    ${out}
</main>
<script>${hljsJS}</script>
<script>
hljs.registerLanguage('cy', function() {
    return {
    keywords: {
        keyword: [
            'func', 'mod', 'for', 'coinit', 'coresume', 'coyield', 'use', 'await', 'context', 'def',
            'return', 'if', 'else', 'as', 'while', 'var', 'let', 'dynobject', 'object', 'struct', 'cstruct', 'with', 'caught',
            'break', 'continue', 'switch', 'pass', 'or', 'and', 'not', 'is', 'error', 'throws', 'move',
            'true', 'false', 'none', 'throw', 'try', 'catch', 'recover', 'enum', 'type', 'case', 'trait'
        ],
        type: [
            'float', 'string', 'Array', 'bool', 'any', 'int', 'List', 'Map', 'symbol', 'dyn'
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
os.writeFile("${curDir}/docs.html", html)

var .out = ''
var .htmlContent = ''
var .textContent = ''
var .state = State.main
var .parsingToc = false
var .tocLinks = List[Link]{}
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
    href  string
    title string
    text  string

func resetState():
    state = State.main
    bufContent = false

func enterBlock(block_t md.BLOCKTYPE, detail_p *void, userdata *void) int:
    if parsingToc:
        switch block_t
        case md.BLOCK_HTML:
            textContent = ''
            bufContent = true
            return 0
        case md.BLOCK_UL: return 0
        case md.BLOCK_LI: return 0
        else:
            print "unsupported enter block ${block_t}"
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
        var detail = md.ptrToBLOCK_CODE_DETAIL_S(detail_p)
        var lang = getAttrText(detail.lang)
        out += "<pre><code class=\"language-${lang}\">"
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
        print "unsupported enter block ${block_t}"
        return 1

func leaveBlock(block_t md.BLOCKTYPE, detail_p *void, userdata *void) int:
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
        var detail = md.ptrToBLOCK_H_DETAIL_S(detail_p)
        textContent = textContent.trim(.ends, " \n")

        var id = textContent.replace(' ', '-')
        id = id.replace('.', '')
        id = id.replace('/', '')
        id = id.replace('$', '')
        id = id.lower()
        if idCounts.get(id) -> count:
            var newId = "${id}-${count}"
            idCounts[id] += 1
            id = newId
        else:
            idCounts[id] = 1

        if id == 'table-of-contents':
            parsingToc = true
        else:
            if lastHLevel > 1:
                out += "<a href=\"#${lastTopicId}\">^topic</a>\n"
            if detail.level == 1:
                lastTopicId = id
            lastHLevel = detail.level
            out += """<h${detail.level} id="${id}">${htmlContent} <a href="#${id}">#</a></h${detail.level}>\n"""

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
        print "unsupported leave block ${block_t}"
        return 1

func enterSpan(span_t md.SPANTYPE, detail_p *void, userdata *void) int:
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
        var detail = md.ptrToSPAN_A_DETAIL_S(detail_p)

        var href = getAttrText(detail.href)
        var title = getAttrText(detail.title)

        if parsingToc:
            tocLinks.append(Link{href=href, title=title, text=''})
            textContent = ''
            bufContent = true
            return 0

        out += """<a href="${href}" title="${title}">"""
        return 0
    else:
        print "unsupported enter span ${span_t}"
        return 1

func leaveSpan(span_t md.SPANTYPE, detail *void, userdata *void) int:
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
        print "unsupported leave span ${span_t}"
        return 1

func text(text_t md.SPANTYPE, ptr *void, len int, userdata *void) int:
    var str = ptr.getString(0, len)
    if bufContent:
        textContent += str
        htmlContent += str
    else:
        out += str
    return 0

func getAttrText(attr dyn) string:
    if attr.size == 0:
        return ''
    return (attr.text as *void).getString(0, attr.size)

type ModulePair:
    path    string
    section string

func genFuncDecl(decl Map) string:
    if (decl['hidden']): return ''
    
    var docLine = decl.get('docs') ?else ''
    var params = List[string]{}
    for (decl['params'] as List[Map]) -> param:
        if param['typeSpec'] != '':
            params.append("${param['name']} ${param['typeSpec']}")
        else:
            params.append("${param['name']}")
    var paramsStr = params.join(', ')
    return "> `func ${decl['name']}(${paramsStr}) ${decl['ret']}`\n>\n>${docLine}\n\n"

func genDocsModules():
    var modules = {
        ModulePair{path='../src/builtins/builtins_vm.cy', section='core'},
        ModulePair{path='../src/builtins/cy.cy', section='cy'},
        ModulePair{path='../src/builtins/math.cy', section='math'},
        ModulePair{path='../src/std/cli.cy', section='cli'},
        ModulePair{path='../src/std/os.cy', section='os'},
        ModulePair{path='../src/std/test.cy', section='test'},
    }

    var curDir = os.dirName(#modUri).?
    -- var md = os.readFile("${curDir}/../modules.md")
    var md = os.readFile("${curDir}/docs.md")

    for modules -> m:
        var src = os.readFile("${curDir}/${m.path}")
        var decls = cy.parse(src).decls
        var gen = "\n"
        for decls -> decl:
            gen += genDecl(decl)

        -- Replace section in modules.md.
        var needle = "<!-- ${m.section}.start -->"
        var startIdx = md.find(needle).? + needle.len()
        var endIdx = md.find("<!-- ${m.section}.end -->").?
        md = md[0..startIdx] + gen + md[endIdx..]

    os.writeFile("${curDir}/docs-modules.md", md)

func genDecl(decl Map) string:
    var gen = ''
    switch decl['type']
    case 'funcDecl':
        gen += genFuncDecl(decl)
    case 'staticDecl':
        var docLine = decl.get('docs') ?else ''
        var typeSpec = if (decl['typeSpec'] != '') decl['typeSpec'] else 'any'
        gen += "> `var ${decl['name']} ${typeSpec}`\n>\n>${docLine}\n\n"
    case 'distinct_decl':
        gen += "### `type ${decl['name']}`\n\n"
        for (decl['funcs'] as List[Map]) -> fdecl:
            gen += genFuncDecl(fdecl)
    case 'struct_decl':
        gen += "### `type ${decl['name']}`\n\n"
        for (decl['funcs'] as List[Map]) -> fdecl:
            gen += genFuncDecl(fdecl)
    case 'custom_decl':
        gen += "### `type ${decl['name']}`\n\n"
        for (decl['funcs'] as List[Map]) -> fdecl:
            gen += genFuncDecl(fdecl)
    case 'trait_decl':
        gen += "### `type ${decl['name']} trait`\n\n"
        for (decl['funcs'] as List[Map]) -> fdecl:
            gen += genFuncDecl(fdecl)
    case 'template':
        gen += genDecl(decl['child'])
    case 'import_stmt':
        pass
    case 'enumDecl':
        pass
    else:
        panic "Unsupported ${decl['type']}"
    return gen
