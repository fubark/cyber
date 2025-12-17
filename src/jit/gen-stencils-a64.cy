use llvm '../tools/llvm.cy'
use os
use meta
use c

--|
--| Takes stencils.o and generates a64_stencils.zig
--|

out := ''
curDir := os.dirname(meta.mod_uri())

var llBuf Ptr[llvm.OpaqueMemoryBuffer_S] = none
var outMsg Ptr[byte] = none
if llvm.CreateMemoryBufferWithContentsOfFile(str.initz('%{curDir}/stencils.o').ptr, &llBuf, &outMsg) != 0:
    panic('unexpected')

cbuf := llvm.GetBufferStart(llBuf)
size := llvm.GetBufferSize(llBuf)
buf := str(cbuf[0..size])

llBin := llvm.CreateBinary(llBuf, none, &outMsg)

binType := llvm.BinaryGetType(llBin)
if binType != llvm.BinaryTypeMachO64L:
    panic('Unexpected object format')

-- Find text section.
codeBuf := ''
llSectIter := llvm.ObjectFileCopySectionIterator(llBin)
while llvm.ObjectFileIsSectionIteratorAtEnd(llBin, llSectIter) == 0:
    cname := llvm.GetSectionName(llSectIter)
    if cname == none:
        llvm.MoveToNextSection(llSectIter)
        continue

    name := c.from_strz(cname)

    if name == '__text':
        ccodeBuf := llvm.GetSectionContents(llSectIter)
        size := llvm.GetSectionSize(llSectIter)
        codeBuf = str(ccodeBuf[0..size])
        break
    llvm.MoveToNextSection(llSectIter)

if codeBuf.len() == 0:
    panic('MissingTextSection')

llSymIter := llvm.ObjectFileCopySymbolIterator(llBin)

type Sym:
    name str
    addr int

-- First pass accumulates the unordered symbols.
syms := []Sym{}
symMap := Map[str, Sym]{}
while llvm.ObjectFileIsSymbolIteratorAtEnd(llBin, llSymIter) == 0:
    if llvm.GetSectionContainsSymbol(llSectIter, llSymIter) == 0:
        -- Not in text section, skip.
        llvm.MoveToNextSymbol(llSymIter)
        continue

    cname := llvm.GetSymbolName(llSymIter)
    name := c.from_strz(cname)
    addr := llvm.GetSymbolAddress(llSymIter)
    -- Size is missing, calculate by sorting symbols and using their address.
    sym := Sym{name=name, addr=addr}
    syms += sym
    symMap[name] = sym

    llvm.MoveToNextSymbol(llSymIter)

-- Sort syms by addr so len can be computed.
syms.sort(|a, b| a.addr < b.addr)

for syms |i, sym|:
    len := 0
    if i == syms.len()-1:
        -- Last sym len needs text sect's len.
        len = codeBuf.len() - sym.addr
    else:
        len = syms[i+1].addr - sym.addr

    -- Skip ltmp0.
    if len == 0: continue 

    bin := codeBuf[sym.addr..sym.addr+len] 

    -- Remove ending continuation branch.

    if r32.decode(bin.span().vec(bin.len()-4, 4), .little) == 0x14000000:
        bin = bin[0..bin.len()-4]

    print('%{sym.name} %{sym.addr} %{bin.fmt_bytes(.hex)}')

    bytes := []str{}
    for bin |b|:
        bytes += '0x%{b.fmt(.hex, {pad='0', width=2})}'

    out += 'pub const %{sym.name[1..]} = [_]u8{ %{bytes.join(', ')} };\n'

-- llSectIter is already at text section.
-- Visit relocation entries and record them as Zig constants.
llRelocIter := llvm.GetRelocations(llSectIter)
while llvm.IsRelocationIteratorAtEnd(llSectIter, llRelocIter) == 0:
    symRef := llvm.GetRelocationSymbol(llRelocIter)
    csymName := llvm.GetSymbolName(symRef)
    symName := c.from_strz(csymName)
    if symName.starts_with('_cont'):
        -- Skip continuations.
        llvm.MoveToNextRelocation(llRelocIter)
        continue

    offset := llvm.GetRelocationOffset(llRelocIter)
    -- relocType := llvm.GetRelocationType(llRelocIter)
    cname := llvm.GetRelocationTypeName(llRelocIter)
    name := c.from_strz(cname)
    cvalue := llvm.GetRelocationValueString(llRelocIter)
    value := c.from_strz(cname)

    -- Find relevant func sym.
    var found ?Sym = none
    for syms |i, sym|:
        if offset >= sym.addr:
            if i < syms.len()-1 and offset >= syms[i+1].addr:
                continue
            found = sym
            break

    if found == none:
        panic('MissingSym')

    out += 'pub const %{found.?.name[1..]}_%{symName[1..]} = %{offset-found.?.addr};\n'

    llvm.MoveToNextRelocation(llRelocIter)

os.write_file('%{curDir}/a64_stencils.zig', out)!