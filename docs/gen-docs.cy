#!cyber

-- Usage: ./gen-docs.cy
-- Import styles instead of inlining: ./gen-docs.cy -import-style
-- Override version string: `-version`

-- 1. First generates `api.html` from `api_template.html`.
-- 2. Generates single `docs.html` from `docs.md`.
--    CSS/JS are also embedded into the docs.html.
-- NOTE: docs.md does not handle custom header ids since Github md doesn't support it.

use cy
use os
use meta
use math
use md '../src/tools/md4c.cy'

args := os.parseArgs({'version', 'import-style', 'skip-api'})
version := args.get('version') ?else 'DEV'
skip_api := args.contains('skip-api')
import_style := args.get('import-style') != none

curDir := os.dirname(meta.mod_uri()).?
hljs_css := str(os.read_file('%{curDir}/github-dark.min.css')!)
simple_css = str(os.read_file('%{curDir}/simple.css')!)
user_css = '<link rel="stylesheet" href="./style.css">'
if !import_style:
    css := str(os.read_file('%{curDir}/style.css')!)
    user_css = '<style>%{css}</style>'

if !skip_api:
    gen_api_docs()!

src := str(os.read_file('%{curDir}/docs.md')!)
csrc := str.initz(src)

parser := md.PARSER{
    enter_block = enterBlock,
    leave_block = leaveBlock,
    enter_span = enterSpan,
    leave_span = leaveSpan,
    text = text,
    abi_version = 0,
    flags = md.FLAG_TABLES,
}
res := md.md_parse(csrc.ptr, as csrc.len(), &parser, none)
if res != 0:
    eprint('parse error: %{res}')
    os.exit(1)

tocLinksHtml := []str{}
for tocLinks |link|:
    tocLinksHtml = tocLinksHtml << '<li><a href="%{link.href}">%{link.text}</a></li>'

hljsJS := str(os.read_file('%{curDir}/highlight.min.js')!)

toc_links := tocLinksHtml.join('\n')
html := """<html lang="en">
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Cyber Docs</title>
    <style>%{simple_css}</style>
    <style>%{hljs_css}</style>
    %{user_css}
</head>
<body id="table-of-contents">
<header>
    <h1 class="title">Cyber Docs</h1>
    <div class="sub-title">%{version}</div>
    <ul>
        <li><a href="https://cyberscript.dev" target="_blank" rel="noopener">Homepage</a></li>
        <li><a href="api.html">API Docs</a></li>
        <li><a href="https://cyberscript.dev/play.html" target="_blank" rel="noopener">Playground</a></li>
        <li><a href="https://github.com/fubark/cyber/edit/master/docs/docs.md" target="_blank" rel="noopener">Edit Page</a></li>
    </ul>
    <div class="sub-title">Table of Contents</div>
    <ul>
        %{toc_links}
    </ul>
</header>
<main>
    %{out}
</main>
<script>%{hljsJS}</script>
<script>
hljs.registerLanguage('cy', function() {
    return {
    keywords: {
        keyword: [
            'fn', 'mod', 'for', 'coinit', 'coresume', 'coyield', 'use', 'await', 'global', 'const', 'scope', 'sink',
            'return', 'if', 'else', 'as', 'while', 'var', 'let', 'struct', 'cstruct', 'with',
            'break', 'continue', 'switch', 'pass', 'or', 'and', 'not', 'is', 'error', 'throws', 'move',
            'true', 'false', 'none', 'throw', 'try', 'catch', 'recover', 'enum', 'type', 'case', 'trait'
        ],
        type: [
            'r8', 'byte', 'r16', 'r32', 'r64', 'i8', 'i16', 'i32', 'i64', 'f32', 'f64', 'float', 'str', 'Array', 'bool', 'int', 'Map', 'symbol'
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
        {
            scope: 'string',
            begin: "```", end: "```"
        },
        {
            scope: 'string',
            begin: "`", end: "`"
        },
        {
            scope: 'keyword',
            begin: '\\\\|',
            end: '\\\\|',
            contains: [
              {
                scope: 'subst', 
                match: /[^|]+/,
              }
            ]
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
print('Done.')
os.write_file('%{curDir}/docs.html', html)!

global out str = ''
global htmlContent str = ''
global textContent str = ''
global state State = .main
global parsing_toc bool = false
global tocLinks []Link = {}
global bufContent bool = false
global lastTopicId str = ''
global lastHLevel int = 1
global simple_css str = ''
global user_css str = ''
global hljs_css str = ''

-- Maps id names to the next unique count from 1.
-- Mimics Githubs duplicate header id generation.
global idCounts Map[str, int] = {}

type State enum:
    case main
    case header
    case html

type Link:
    href  str
    title str
    text  str

fn resetState():
    state = State.main
    bufContent = false

#[extern]
fn enterBlock(block_t md.BLOCKTYPE, detail_p Ptr[void], userdata Ptr[void]) -> i32:
    if parsing_toc:
        switch block_t:
            case md.BLOCK_HTML:
                textContent = ''
                bufContent = true
                return 0
            case md.BLOCK_UL: return 0
            case md.BLOCK_LI: return 0
            else:
                print('unsupported enter block %{block_t}')
                return 1
        return 0

    switch block_t:
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
            detail := as[Ptr[md.BLOCK_CODE_DETAIL]] detail_p
            lang := get_attr_text(detail.lang)
            out += '<pre><code class="language-%{lang}">'
            return 0
        case md.BLOCK_UL:
            out += '<ul>\n'
            return 0
        case md.BLOCK_OL:
            out += '<ol>\n'
            return 0
        case md.BLOCK_LI:
            out += '<li>'
            return 0
        case md.BLOCK_HTML:
            return 0
        else:
            print('unsupported enter block %{block_t}')
            return 1

#[extern]
fn leaveBlock(block_t md.BLOCKTYPE, detail_p Ptr[void], userdata Ptr[void]) -> i32:
    if parsing_toc:
        if block_t == md.BLOCK_HTML:
            if textContent.starts_with('<!--') and textContent.contains('TOC-END'):
                parsing_toc = false
            resetState()
        return 0

    switch block_t:
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
            detail := as[Ptr[md.BLOCK_H_DETAIL]] detail_p
            textContent = textContent.trim(' \n')

            id := textContent.replace(' ', '-')
            id = id.replace('.', '')
            id = id.replace('/', '')
            id = id.replace('$', '')
            id = id.replace('[', '')
            id = id.replace(']', '')
            id = id.replace('#', '')
            id = id.lower()
            if idCounts.get(id) |count|:
                newId := '%{id}-%{count}'
                idCounts[id] = 1 + idCounts[id]
                id = newId
            else:
                idCounts[id] = 1

            if id == 'table-of-contents':
                parsing_toc = true
            else:
                if lastHLevel > 1:
                    out += '<a href="#%{lastTopicId}">^topic</a>\n'
                if detail.level == 1:
                    lastTopicId = id
                lastHLevel = detail.level
                out += '<h%{detail.level.fmt(.dec)} id="%{id}">%{htmlContent} <a href="#%{id}">#</a></h%{detail.level.fmt(.dec)}>\n'

            resetState()
            return 0
        case md.BLOCK_P:
            out += '</p>\n'
            return 0
        case md.BLOCK_CODE:
            out += '</code></pre>\n'
            return 0
        case md.BLOCK_UL:
            out += '</ul>\n'
            return 0
        case md.BLOCK_OL:
            out += '</ol>\n'
            return 0
        case md.BLOCK_LI:
            out += '</li>\n'
            return 0
        case md.BLOCK_HTML:
            return 0
        else:
            print('unsupported leave block %{block_t}')
            return 1

#[extern]
fn enterSpan(span_t md.SPANTYPE, detail_p Ptr[void], userdata Ptr[void]) -> i32:
    switch span_t:
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
            detail := as[Ptr[md.SPAN_A_DETAIL]] detail_p

            href := get_attr_text(detail.href)
            title := get_attr_text(detail.title)

            if parsing_toc:
                tocLinks <<= {href=href, title=title, text=''}
                textContent = ''
                bufContent = true
                return 0

            out += '<a href="%{href}" title="%{title}">'
            return 0
        else:
            print('unsupported enter span %{span_t}')
            return 1

#[extern]
fn leaveSpan(span_t md.SPANTYPE, detail Ptr[void], userdata Ptr[void]) -> i32:
    switch span_t:
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
            if parsing_toc:
                bufContent = false
                tocLinks[tocLinks.len()-1].text = textContent
                return 0

            out += '</a>'
            return 0
        else:
            print('unsupported leave span %{span_t}')
            return 1

#[extern]
fn text(text_t md.SPANTYPE, ptr Ptr[byte], len r32, userdata Ptr[void]) -> i32:
    s := str(ptr[0..len])
    if bufContent:
        textContent += s
        htmlContent += s
    else:
        out += s
    return 0

fn get_attr_text(attr md.ATTRIBUTE) -> str:
    if attr.size == 0:
        return ''
    return str(attr.text[0..attr.size])

type ModulePair:
    path    str
    section str

fn func_params_text(parse_res &cy.ParseResult, param_nodes []^cy.Node) -> str:
    params := []str{}
    for param_nodes |param_n|:
        param := param_n.!func_param
        param_t := ''
        if param.type |spec|:
            param_t = ' <span class="type">%{parse_res.node_text(spec)}</span>'

        scope_s := if (param.scope_param) '<span class="keyword">scope</span> ' else ''
        name := '%{scope_s}%{parse_res.node_text(param.name_type)}'
        params <<= '%{name}%{param_t}'
    return params.join(', ')

fn gen_func_decl(parse_res &cy.ParseResult, id str, decl cy.FuncDecl, tparams_s str, docs str) -> str:
    if decl.hidden: return ''

    name := decl.name.name()
    ret_s := ''
    if decl.ret |ret|:
        scope_s := if (decl.scope_ret) '<span class="keyword">scope</span> ' else ''
        ret_s = '-> %{scope_s}<span class="type">' + parse_res.node_text(decl.ret.?) + '</span>'

    if decl.sig_t == .method:
        params_s := func_params_text(parse_res, decl.params[1..])

        self := decl.params[0].!func_param
        self_scope_s := if (self.scope_param) '<span class="keyword">scope</span> ' else ''
        self_t := '%{self_scope_s}%{parse_res.node_text(self.type.?)}'
        return '<blockquote><code><span class="keyword">fn</span> (<span class="type">%{self_t}</span>) <span class="decl">%{name}</span>%{tparams_s}(%{params_s}) %{ret_s}</code><br />%{docs}</blockquote>'
    else:
        parent_s := ''
        if decl.parent |parent|:
            parent_s = '<span class="type">%{parse_res.node_text(parent)}</span> :: '
        params_s := func_params_text(parse_res, decl.params)
        return '<blockquote id="%{id}"><code><span class="keyword">fn</span> %{parent_s}<span class="decl">%{name}</span>%{tparams_s}(%{params_s}) %{ret_s}</code><br />%{docs}</blockquote>'

fn gen_mods_nav(group_name str, mods []ModulePair) -> str:
    res := '<div class="sub-title">%{group_name}</div>'
    res += '<ul>'
    for mods |m|:
        res += '<li><a href="#%{m.section}">mod %{m.section}</a></li>'
    res += '</ul>'
    return res

global decl_ids Map[^cy.Node, int] = {}
global next_decl_id int = 1

fn gen_index_entry(entry IndexEntry) -> str:
    base_stmt := entry.stmt
    if meta.choice_tag(cy.Node, base_stmt) == .template:
        template := base_stmt.!template
        base_stmt = template.child_decl

    id := 'decl%{decl_ids[base_stmt]}'
    kind := ''

    switch base_stmt.*:
        case .func_decl |decl|:
            kind = 'fn'

        case .const_decl |decl|:
            kind = 'const'

        case .global_decl |decl|:
            kind = 'global'

        case .type_alias_decl:
            return '<div><code><a href="#%{id}"><span class="keyword">type</span> %{entry.name} =</a></code></div>'

        case .enum_decl
        case .trait_decl
        case .custom_type_decl
        case .struct_decl
        case .cunion_decl
        case .type_const_decl:
            kind = 'type'

        else:
            panic('unexpected')

    child_syms := ''
    if entry.children.len() > 0:
        child_syms = ' (%{entry.children.len()})'

    return '<div><code><a href="#%{id}"><span class="keyword">%{kind}</span> %{entry.name}</a>%{child_syms}</code></div>'

type IndexEntry:
    stmt ^cy.Node
    name str

    -- For type declaration children.
    children []^cy.Node = {}

fn symbol_order(stmt ^cy.Node) -> int:
    base_stmt := stmt
    if meta.choice_tag(cy.Node, stmt) == .template:
        template := stmt.!template
        base_stmt = template.child_decl

    switch base_stmt.*:
        case .func_decl: return 2
        case .const_decl: return 0
        case .global_decl: return 1
        case .type_alias_decl
        case .trait_decl
        case .custom_type_decl
        case .struct_decl
        case .enum_decl
        case .type_const_decl
        case .cunion_decl: return 3
        else:
            panic('unexpected')

fn get_base_type(node ^cy.Node) -> ^cy.Node:
    switch node.*:
        case .ident:
            return node
        case .generic_expand |n|:
            return get_base_type(n.left)
        case .borrow |n|:
            return get_base_type(n.child)
        case .ex_borrow |n|:
            return get_base_type(n.child)
        else:
            panic('unsupported %{meta.choice_tag(cy.Node, node)}')

fn gen_mods_content(cur_dir str, mods []ModulePair) -> !str:
    res := ''
    for mods |m|:
        eprint('gen docs for: %{m.path}')
        src := str(os.read_file('%{cur_dir}/%{m.path}')!)

        parse_res := cy.parse(src)
        stmts := parse_res.root.?.!root.stmts
        gen := '<h1 id="%{m.section}"><code><span class="keyword">mod</span> <span class="decl">%{m.section}</span></code></h1>'

        index_entries := []IndexEntry{}
        type_to_entry := Map[str, int]{}

        for stmts |stmt|:
            base_stmt := stmt
            if meta.choice_tag(cy.Node, stmt) == .template:
                template := stmt.!template
                base_stmt = template.child_decl

            decl_ids[base_stmt] = next_decl_id
            next_decl_id += 1

            switch base_stmt.*:
                case .func_decl |decl|:
                    if decl.hidden:
                        continue
                    if decl.sig_t == .method:
                        rec_t := decl.params[0].!func_param.type.?
                        rec_t = get_base_type(rec_t)
                        entry_id := type_to_entry.get(rec_t.name()) ?else:
                            panic('Missing type entry. %{rec_t.name()}')
                        index_entries[entry_id].children <<= base_stmt
                        continue

                    if decl.parent |parent|:
                        parent = get_base_type(parent)
                        entry_id := type_to_entry.get(parent.name()) ?else:
                            panic('Missing type entry. %{parent.name()}')
                        index_entries[entry_id].children <<= base_stmt
                        continue

                    index_entries <<= {stmt=stmt, name=decl.name.name()}

                case .const_decl |decl|:
                    if decl.parent |parent|:
                        parent = get_base_type(parent)
                        entry_id := type_to_entry.get(parent.name()) ?else:
                            panic('Missing type entry. %{parent.name()}')
                        index_entries[entry_id].children <<= base_stmt
                        continue
                    index_entries <<= {stmt=stmt, name=decl.name.name()}

                case .global_decl |decl|:
                    if decl.parent |parent|:
                        parent = get_base_type(parent)
                        entry_id := type_to_entry.get(parent.name()) ?else:
                            panic('Missing type entry. %{parent.name()}')
                        index_entries[entry_id].children <<= base_stmt
                        continue
                    index_entries <<= {stmt=stmt, name=decl.name.name()}

                case .type_alias_decl |decl|:
                    if decl.parent |parent|:
                        parent = get_base_type(parent)
                        entry_id := type_to_entry.get(parent.name()) ?else:
                            panic('Missing type entry. %{parent.name()}')
                        index_entries[entry_id].children <<= base_stmt
                        continue
                    index_entries <<= {stmt=stmt, name=decl.name.name()}

                case .cunion_decl |decl|:
                    entry_id := index_entries.len()
                    index_entries <<= {stmt=stmt, name=decl.name.name()}
                    type_to_entry[decl.name.name()] = entry_id

                case .type_const_decl |decl|:
                    entry_id := index_entries.len()
                    index_entries <<= {stmt=stmt, name=decl.name.name()}
                    type_to_entry[decl.name.name()] = entry_id

                case .enum_decl |decl|:
                    entry_id := index_entries.len()
                    index_entries <<= {stmt=stmt, name=decl.name.name()}
                    type_to_entry[decl.name.name()] = entry_id

                case .trait_decl |decl|:
                    entry_id := index_entries.len()
                    index_entries <<= {stmt=stmt, name=decl.name.name()}
                    type_to_entry[decl.name.name()] = entry_id

                case .custom_type_decl |decl|:
                    entry_id := index_entries.len()
                    index_entries <<= {stmt=stmt, name=decl.name.name()}
                    type_to_entry[decl.name.name()] = entry_id

                case .struct_decl |decl|:
                    entry_id := index_entries.len()
                    index_entries <<= {stmt=stmt, name=decl.name.name()}
                    type_to_entry[decl.name.name()] = entry_id

                else:
                    continue

        -- Sort based on symbol kind then natural order.
        index_entries.sort(|a, b|):
            a_sym := symbol_order(a.stmt)
            b_sym := symbol_order(b.stmt)
            if a_sym < b_sym:
                return true
            if a_sym > b_sym:
                return false
            return a.name.lower().compare(b.name.lower()) == .lt

        column_size := index_entries.len()/4
        if index_entries.len()%4 != 0:
            column_size += 1

        gen += '<table class="index-table"><tbody><tr>'

        -- First double column.
        gen += '<td>'
        gen += '<div class="double">'
        gen += '<div>'
        for index_entries[0..column_size] |stmt|:
            gen += gen_index_entry(stmt)
        gen += '</div>'

        i := column_size
        end := math.min(i + column_size, index_entries.len())
        gen += '<div>'
        for index_entries[i..end] |stmt|:
            gen += gen_index_entry(stmt)
        gen += '</div>'
        gen += '</div>'
        gen += '</td>'

        -- Second double column.
        gen += '<td>'
        gen += '<div class="double">'
        i = end
        end = math.min(i + column_size, index_entries.len())
        gen += '<div>'
        for index_entries[i..end] |stmt|:
            gen += gen_index_entry(stmt)
        gen += '</div>'

        end = math.min(i + column_size, index_entries.len())
        gen += '<div>'
        gen += '<div>'
        i = end
        end = math.min(i + column_size, index_entries.len())
        for index_entries[i..end] |stmt|:
            gen += gen_index_entry(stmt)
        gen += '</div>'
        gen += '</div>'
        gen += '</td>'
        gen += '</tr></tbody></table>'

        -- Generate each declaration entry.
        for index_entries |entry|:
            gen += gen_decl(entry.stmt, &parse_res)

            -- Order: type level decls, then methods.
            entry.children.sort(|a, b|):
                a_method := meta.choice_tag(cy.Node, a) == .func_decl and a.!func_decl.sig_t == .method
                b_method := meta.choice_tag(cy.Node, b) == .func_decl and b.!func_decl.sig_t == .method
                if !a_method and b_method:
                    return true
                if a_method and !b_method:
                    return false
                return a.decl_name().lower().compare(b.decl_name().lower()) == .lt

            for entry.children |child|:
                gen += gen_decl(child, &parse_res)

        res += gen
    return res 

fn gen_api_docs() -> !void:
    core_mods := []ModulePair{
        {path='../src/builtins/core.cy', section='core'},
        {path='../src/builtins/c.cy', section='c'},
        {path='../src/builtins/cy.cy', section='cy'},
        {path='../src/builtins/meta.cy', section='meta'},
    }

    std_mods := []ModulePair{
        {path='../src/std/cli.cy', section='cli'},
        {path='../src/std/libc.cy', section='libc'},
        {path='../src/std/io.cy', section='io'},
        {path='../src/std/math.cy', section='math'},
        {path='../src/std/os.cy', section='os'},
        {path='../src/std/test.cy', section='test'},
    }

    cur_dir := os.dirname(meta.mod_uri()).?
    template := str(os.read_file('%{cur_dir}/api_template.html')!)

    nav := ''
    content := ''

    nav += gen_mods_nav('core modules', core_mods)
    content += gen_mods_content(cur_dir, core_mods)!

    nav += gen_mods_nav('std modules', std_mods)
    content += gen_mods_content(cur_dir, std_mods)!

    head := '<style>%{simple_css}</style>'
    head += '<style>%{hljs_css}</style>'
    head += user_css

    html := template.replace('<!--NAV-->', nav)
    html = html.replace('<!--HEAD-->', head)
    html = html.replace('<!--CONTENT-->', content)

    os.write_file('%{cur_dir}/api.html', html)!

fn gen_impls(res &cy.ParseResult, impls []^cy.Node) -> str:
    if impls.len() == 0:
        return ''

    s := '<br /><code>&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">with</span> '
    s += '<span class="decl">%{res.node_text(impls[0].!impl_decl.trait)}</span>'
    for impls[1..] |impl|:
        s += ', <span class="decl">%{res.node_text(impl.!impl_decl.trait)}</span>'
    s += '</code>'

    return s

fn gen_decl(_stmt ^cy.Node, parse_res &cy.ParseResult) -> str:
    gen := ''

    stmt := _stmt

    tparams_s := ''
    if meta.choice_tag(cy.Node, stmt) == .template:
        template := stmt.!template
        tparams_s = '[%{func_params_text(parse_res, template.params)}]'
        stmt = template.child_decl

    id := 'decl%{decl_ids[stmt]}'

    switch stmt.*:
        case .func_decl |decl|:
            docs := parse_res.stmt_docs(stmt) ?else ''

            gen += gen_func_decl(parse_res, id, decl, tparams_s, docs)

        case .const_decl |decl|:
            parent_s := ''
            if decl.parent |parent|:
                parent_s = '<span class="type">%{parse_res.node_text(parent)}</span> :: '

            type_s := ''
            if decl.type |_type|:
                type_s = ' <span class="type">%{parse_res.node_text(_type)}</span>'

            init_s := ''
            if decl.right |right|:
                init_s = ' = %{parse_res.node_text(right)}'

            docs := parse_res.stmt_docs(stmt) ?else ''
            gen += '<blockquote id="%{id}"><code><span class="keyword">const</span> %{parent_s}<span class="decl">%{decl.name.name()}</span>%{type_s}%{init_s}<br/>%{docs}</code></blockquote>'

        case .global_decl |decl|:
            parent_s := ''
            if decl.parent |parent|:
                parent_s = '<span class="type">%{parse_res.node_text(parent)}</span> :: '

            type_s := ' <span class="type">%{parse_res.node_text(decl.type)}</span>'

            init_s := ''
            if decl.right |right|:
                init_s = ' = %{parse_res.node_text(right)}'

            docs := parse_res.stmt_docs(stmt) ?else ''
            gen += '<blockquote id="%{id}"><code><span class="keyword">global</span> %{parent_s}<span class="decl">%{decl.name.name()}</span>%{type_s}%{init_s}<br/>%{docs}</code></blockquote>'

        case .struct_decl |decl|:
            docs := parse_res.stmt_docs(stmt) ?else ''
            implements := gen_impls(parse_res, decl.impls)
            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> <span class="decl">%{decl.name.name()}</span>%{tparams_s} <span class="keyword">struct</span></code>%{implements}<br/>%{docs}</blockquote>'

        case .cunion_decl |decl|:
            docs := parse_res.stmt_docs(stmt) ?else ''
            id := decl.name.name()
            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> <span class="decl">%{decl.name.name()}</span>%{tparams_s} <span class="keyword">cunion</span></code><br/>%{docs}</blockquote>'

        case .custom_type_decl |decl|:
            docs := parse_res.stmt_docs(stmt) ?else ''
            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> <span class="decl">%{decl.name.name()}</span>%{tparams_s} _</code><br/>%{docs}</blockquote>'

        case .type_alias_decl |decl|:
            parent_s := ''
            if decl.parent |parent|:
                parent_s = '<span class="type">%{parse_res.node_text(parent)}</span> :: '
            target := '<span class="type">%{parse_res.node_text(decl.target)}</span>'

            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> %{parent_s}<span class="decl">%{decl.name.name()}</span>%{tparams_s} = %{target}</code></blockquote>'

        case .type_const_decl |decl|:
            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> <span class="decl">%{decl.name.name()}</span>%{tparams_s} <span class="keyword">const</span></code></blockquote>'

        case .enum_decl |decl|:
            docs := parse_res.stmt_docs(stmt) ?else ''
            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> <span class="decl">%{decl.name.name()}</span>%{tparams_s} <span class="keyword">enum:</span></code><br/>%{docs}'
            for decl.members |member_n|:
                member := member_n.!enum_member
                payload_s := ''
                if member.payload |payload|:
                    payload_s = ' <span class="type">%{parse_res.node_text(payload)}</span>'

                gen += '<code>&nbsp;&nbsp;<span class="keyword">case</span> %{member.name.name()}%{payload_s}</code><br />'
            gen += '</blockquote>'

        case .trait_decl |decl|:
            docs := parse_res.stmt_docs(stmt) ?else ''
            gen += '<blockquote id="%{id}" class="type"><code><span class="keyword">type</span> <span class="decl">%{decl.name.name()}</span>%{tparams_s} <span class="keyword">trait</span></code><br/>%{docs}</blockquote>'
            for decl.funcs |func|:
                docs := parse_res.stmt_docs(func) ?else ''
                gen += gen_func_decl(parse_res, '', func.!func_decl, '', docs)

        case .ct_stmt
        case .import_stmt:
            pass

        else:
            panic('Unsupported %{stmt.*}')
    return gen
