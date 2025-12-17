use llvm '../tools/llvm.cy'
use os
use meta
use c

--|
--| Takes stencils.o and generates x64_stencils.zig
--|

out := ''
curDir := os.dirname(meta.mod_uri())

var llBuf Ptr[llvm.OpaqueMemoryBuffer_S] = none
var outMsg Ptr[byte] = none
if llvm.CreateMemoryBufferWithContentsOfFile(str.initz('%{curDir}/stencils.o').ptr, &llBuf, &outMsg) != 0:
    panic('unexepcted')

cbuf := llvm.GetBufferStart(llBuf)
size := llvm.GetBufferSize(llBuf)
buf := str(cbuf[0..size])

llBin := llvm.CreateBinary(llBuf, none, &outMsg)

binType := llvm.BinaryGetType(llBin)
if binType != llvm.BinaryTypeELF64L:
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

    if name == '.text':
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
    code str

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
    if name == '.text':
        llvm.MoveToNextSymbol(llSymIter)
        continue

    addr := llvm.GetSymbolAddress(llSymIter)
    size := llvm.GetSymbolSize(llSymIter)
    code := codeBuf[addr..addr+size]
    sym := Sym{name=name, addr=addr, code=code}
    syms += sym
    symMap[name] = sym

    llvm.MoveToNextSymbol(llSymIter)

-- Seek to relocation section.
llSectIter = llvm.ObjectFileCopySectionIterator(llBin)
while llvm.ObjectFileIsSectionIteratorAtEnd(llBin, llSectIter) == 0:
    cname := llvm.GetSectionName(llSectIter)
    if cname == none:
        llvm.MoveToNextSection(llSectIter)
        continue

    name := c.from_strz(cname)
    if name == '.rela.text':
        break

    llvm.MoveToNextSection(llSectIter)

-- Visit relocation entries and record them as Zig constants.
llRelocIter := llvm.GetRelocations(llSectIter)
while llvm.IsRelocationIteratorAtEnd(llSectIter, llRelocIter) == 0:
    symRef := llvm.GetRelocationSymbol(llRelocIter)
    csymName := llvm.GetSymbolName(symRef)
    symName := c.from_strz(csymName)

    offset := llvm.GetRelocationOffset(llRelocIter)
    relocType := llvm.GetRelocationType(llRelocIter)
    cname := llvm.GetRelocationTypeName(llRelocIter)
    name := c.from_strz(cname)
    cvalue := llvm.GetRelocationValueString(llRelocIter)
    value := c.from_strz(cname)

    instOffset := 0
    R_X86_64_PLT32 := 4
    if relocType == R_X86_64_PLT32:
        instOffset = 1

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

    roffset := (offset - instOffset) - found.?.addr
    if symName.starts_with('cont'):
        -- Remove code after continuation.
        found.?.code = found.?.code[0..roffset]

        -- Skip continuations.
        llvm.MoveToNextRelocation(llRelocIter)
        continue

    out += 'pub const %{found.?.name}_%{symName} = %{roffset};\n'

    llvm.MoveToNextRelocation(llRelocIter)

-- After continuations are removed, gen sym's code.
for syms |i, sym|:
    print('%{sym.name} %{sym.addr} %{sym.code.fmt_bytes(.hex)}')

    bytes := []str{}
    for sym.code |b|:
        bytes += '0x%{b.fmt(.hex, {pad='0', width=2})}'

    out += 'pub const %{sym.name} = [_]u8{ %{bytes.join(', ')} };\n'

os.write_file('%{curDir}/x64_stencils.zig', out)!