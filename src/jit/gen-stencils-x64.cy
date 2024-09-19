use llvm '../tools/llvm.cy'
use os

--|
--| Takes stencils.o and generates x64_stencils.zig
--|

var out = ''
var curDir = os.dirName(#modUri)

dyn outLLBuf = llvm.ffi.new(.voidPtr)
var outMsg = llvm.ffi.new(.charPtr)
if llvm.CreateMemoryBufferWithContentsOfFile(os.cstr("$(curDir)/stencils.o"), outLLBuf, outMsg) != 0:
    throw error.Unexpected

var llBuf = outLLBuf.get(0, .voidPtr)
var cbuf = llvm.GetBufferStart(llBuf)
var size = llvm.GetBufferSize(llBuf)
var buf = cbuf.getString(0, size)

var llBin = llvm.CreateBinary(llBuf, pointer(void, 0), outMsg)

var binType = llvm.BinaryGetType(llBin)
if binType != llvm.BinaryTypeELF64L:
    throw error.UnexpectedObjectFormat

-- Find text section.
dyn codeBuf = false
var llSectIter = llvm.ObjectFileCopySectionIterator(llBin)
while llvm.ObjectFileIsSectionIteratorAtEnd(llBin, llSectIter) == 0:
    var cname = llvm.GetSectionName(llSectIter)
    if cname.addr() == 0:
        llvm.MoveToNextSection(llSectIter)
        continue

    var name = cname.fromCstr(0)

    if name == '.text':
        var ccodeBuf = llvm.GetSectionContents(llSectIter)
        var size = llvm.GetSectionSize(llSectIter)
        codeBuf = ccodeBuf.getString(0, size)
        break
    llvm.MoveToNextSection(llSectIter)

if codeBuf == false:
    throw error.MissingTextSection

var llSymIter = llvm.ObjectFileCopySymbolIterator(llBin)

type Sym:
    name String
    addr int
    code String

-- First pass accumulates the unordered symbols.
var syms = List[Sym]{}
var symMap = {}
while llvm.ObjectFileIsSymbolIteratorAtEnd(llBin, llSymIter) == 0:
    if llvm.GetSectionContainsSymbol(llSectIter, llSymIter) == 0:
        -- Not in text section, skip.
        llvm.MoveToNextSymbol(llSymIter)
        continue

    var cname = llvm.GetSymbolName(llSymIter)
    var name = cname.fromCstr(0)
    if name == '.text':
        llvm.MoveToNextSymbol(llSymIter)
        continue

    var addr = llvm.GetSymbolAddress(llSymIter)
    var size = llvm.GetSymbolSize(llSymIter)
    var code = codeBuf[addr..addr+size]
    var sym = Sym{name=name, addr=addr, code=code}
    syms.append(sym)
    symMap[name] = sym

    llvm.MoveToNextSymbol(llSymIter)

-- Seek to relocation section.
llSectIter = llvm.ObjectFileCopySectionIterator(llBin)
while llvm.ObjectFileIsSectionIteratorAtEnd(llBin, llSectIter) == 0:
    var cname = llvm.GetSectionName(llSectIter)
    if cname.addr() == 0:
        llvm.MoveToNextSection(llSectIter)
        continue

    var name = cname.fromCstr(0)
    if name == '.rela.text':
        break

    llvm.MoveToNextSection(llSectIter)

-- Visit relocation entries and record them as Zig constants.
var llRelocIter = llvm.GetRelocations(llSectIter)
while llvm.IsRelocationIteratorAtEnd(llSectIter, llRelocIter) == 0:
    var symRef = llvm.GetRelocationSymbol(llRelocIter)
    var csymName = llvm.GetSymbolName(symRef)
    var symName = csymName.fromCstr(0)

    var offset = llvm.GetRelocationOffset(llRelocIter)
    var relocType = llvm.GetRelocationType(llRelocIter)
    var cname = llvm.GetRelocationTypeName(llRelocIter)
    var name = cname.fromCstr(0)
    var cvalue = llvm.GetRelocationValueString(llRelocIter)
    var value = cname.fromCstr(0)

    var instOffset = 0
    var R_X86_64_PLT32 = 4
    if relocType == R_X86_64_PLT32:
        instOffset = 1

    -- Find relevant func sym.
    dyn found = false
    for syms -> sym, i:
        if offset >= sym.addr:
            if i < syms.len()-1 and offset >= syms[i+1].addr:
                continue
            found = sym
            break

    if found == false:
        throw error.MissingSym

    var roffset = (offset - instOffset) - found.addr
    if symName.startsWith('cont'):
        -- Remove code after continuation.
        found.code = found.code[0..roffset]

        -- Skip continuations.
        llvm.MoveToNextRelocation(llRelocIter)
        continue

    out += "pub const $(found.name)_$(symName) = $(roffset);\n"

    llvm.MoveToNextRelocation(llRelocIter)

-- After continuations are removed, gen sym's code.
for syms -> sym, i:
    print "$(sym.name) $(sym.addr) $(sym.code.fmt(.x))"

    var bytes = List[dyn]{}
    for sym.code -> byte:
        bytes.append("0x$(byte.fmt(.x, {pad=`0`, width=2}))")

    out += "pub const $(sym.name) = [_]u8{ $(bytes.join(', ')) };\n"

os.writeFile("$(curDir)/x64_stencils.zig", out)