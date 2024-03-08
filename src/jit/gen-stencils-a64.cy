import llvm '../tools/llvm.cy'
import os

--|
--| Takes stencils.o and generates a64_stencils.zig
--|

var out = ''
var curDir = os.dirName(#modUri)

my outLLBuf = llvm.ffi.new(.voidPtr)
var outMsg = llvm.ffi.new(.charPtr)
if llvm.CreateMemoryBufferWithContentsOfFile(os.cstr("$(curDir)/stencils.o"), outLLBuf, outMsg) != 0:
    throw error.Unexpected

var llBuf = outLLBuf.get(0, .voidPtr)
var cbuf = llvm.GetBufferStart(llBuf)
var size = llvm.GetBufferSize(llBuf)
var buf = cbuf.toArray(0, size)

var llBin = llvm.CreateBinary(llBuf, pointer(0), outMsg)

var binType = llvm.BinaryGetType(llBin)
if binType != llvm.BinaryTypeMachO64L:
    throw error.UnexpectedObjectFormat

-- Find text section.
my codeBuf = false
var llSectIter = llvm.ObjectFileCopySectionIterator(llBin)
while llvm.ObjectFileIsSectionIteratorAtEnd(llBin, llSectIter) == 0:
    var cname = llvm.GetSectionName(llSectIter)
    if cname.addr() == 0:
        llvm.MoveToNextSection(llSectIter)
        continue

    var name = cname.fromCstr(0).decode()

    if name == '__text':
        var ccodeBuf = llvm.GetSectionContents(llSectIter)
        var size = llvm.GetSectionSize(llSectIter)
        codeBuf = ccodeBuf.toArray(0, size)
        break
    llvm.MoveToNextSection(llSectIter)

if codeBuf == false:
    throw error.MissingTextSection

var llSymIter = llvm.ObjectFileCopySymbolIterator(llBin)

type Sym:
    name String
    addr int

-- First pass accumulates the unordered symbols.
my syms = []
var symMap = [:]
while llvm.ObjectFileIsSymbolIteratorAtEnd(llBin, llSymIter) == 0:
    if llvm.GetSectionContainsSymbol(llSectIter, llSymIter) == 0:
        -- Not in text section, skip.
        llvm.MoveToNextSymbol(llSymIter)
        continue

    var cname = llvm.GetSymbolName(llSymIter)
    var name = cname.fromCstr(0).decode()
    var addr = llvm.GetSymbolAddress(llSymIter)
    -- Size is missing, calculate by sorting symbols and using their address.
    var sym = [Sym name: name, addr: addr]
    syms.append(sym)
    symMap[name] = sym

    llvm.MoveToNextSymbol(llSymIter)

-- Sort syms by addr so len can be computed.
syms.sort((a, b) => a.addr < b.addr)

for syms -> sym, i:
    var len = 0
    if i == syms.len()-1:
        -- Last sym len needs text sect's len.
        len = codeBuf.len() - sym.addr
    else:
        len = syms[i+1].addr - sym.addr

    -- Skip ltmp0.
    if len == 0: continue 

    my bin = codeBuf[sym.addr..sym.addr+len] 

    -- Remove ending continuation branch.
    if bin[bin.len()-4..].getInt32(0, .little) == 0x14000000:
        bin = bin[0..bin.len()-4]

    print "$(sym.name) $(sym.addr) $(bin.fmt(.x))"

    var bytes = []
    for bin -> byte:
        bytes.append("0x$(byte.fmt(.x, [pad: `0`, width: 2]))")

    out += "pub const $(sym.name[1..]) = [_]u8{ $(bytes.join(', ')) };\n"

-- llSectIter is already at text section.
-- Visit relocation entries and record them as Zig constants.
var llRelocIter = llvm.GetRelocations(llSectIter)
while llvm.IsRelocationIteratorAtEnd(llSectIter, llRelocIter) == 0:
    var symRef = llvm.GetRelocationSymbol(llRelocIter)
    var csymName = llvm.GetSymbolName(symRef)
    var symName = csymName.fromCstr(0).decode()
    if symName.startsWith('_cont'):
        -- Skip continuations.
        llvm.MoveToNextRelocation(llRelocIter)
        continue

    var offset = llvm.GetRelocationOffset(llRelocIter)
    -- var relocType = llvm.GetRelocationType(llRelocIter)
    var cname = llvm.GetRelocationTypeName(llRelocIter)
    var name = cname.fromCstr(0).decode()
    var cvalue = llvm.GetRelocationValueString(llRelocIter)
    var value = cname.fromCstr(0).decode()

    -- Find relevant func sym.
    my found = false
    for syms -> sym, i:
        if offset >= sym.addr:
            if i < syms.len()-1 and offset >= syms[i+1].addr:
                continue
            found = sym
            break

    if found == false:
        throw error.MissingSym

    out += "pub const $(found.name[1..])_$(symName[1..]) = $(offset-found.addr);\n"

    llvm.MoveToNextRelocation(llRelocIter)

os.writeFile("$(curDir)/a64_stencils.zig", out)