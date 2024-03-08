import os

-- Manual bindings for clang. This is used to bootstrap cbindgen.

var .CXEval_UnExposed = 0
var .CXEval_Int = 1
var .CXEval_Float = 2
var .CXEval_StrLiteral = 4

var .CXType_Void = 2
var .CXType_Bool = 3
var .CXType_UChar = 5
var .CXType_UInt = 9
var .CXType_ULongLong = 11
var .CXType_Char_S = 13
var .CXType_Int = 17
var .CXType_Long = 18
var .CXType_LongLong = 19
var .CXType_Float = 21
var .CXType_Double = 22
var .CXType_Pointer = 101
var .CXType_Record = 105
var .CXType_Enum = 106
var .CXType_Typedef = 107
var .CXType_FunctionProto = 111
var .CXType_ConstantArray = 112
var .CXType_IncompleteArray = 114
var .CXType_Elaborated = 119
var .CXCursor_CXXFunctionalCastExpr = 128

var .CXCursor_UnexposedDecl = 1
var .CXCursor_StructDecl = 2
var .CXCursor_EnumDecl = 5
var .CXCursor_FieldDecl = 6
var .CXCursor_FunctionDecl = 8
var .CXCursor_VarDecl = 9
var .CXCursor_TypedefDecl = 20
var .CXCursor_TypeRef = 43
var .CXCursor_InvalidFile = 70
var .CXCursor_IntegerLiteral = 106
var .CXCursor_InitListExpr = 119
var .CXCursor_MacroDefinition = 501
var .CXCursor_MacroExpansion = 502
var .CXCursor_InclusionDirective = 503

var .CXTranslationUnit_DetailedPreprocessingRecord = 0x01
var .CXTranslationUnit_SkipFunctionBodies = 0x40
var .CXTranslationUnit_KeepGoing = 0x200
var .CXTranslationUnit_SingleFileParse = 0x400

var .CXChildVisit_Break = 0
var .CXChildVisit_Continue = 1
var .CXChildVisit_Recurse = 2

type CXSourceLocation:
    ptr_data List
    int_data int

type CXString:
    data pointer 
    private_flags int

type CXCursor:
    kind  int
    xdata int
    data  List

type CXType:
    kind int
    data List
 
my .lib = load(ffi) -- Make lib depedent on ffi.
my .ffi = none

func load(dummy):
    ffi = os.newFFI()

    -- enum CXTypeKind kind, void *data[2]
    ffi.cbind(CXType, [.int, [os.CArray n: 2, elem: .voidPtr]])

    -- enum CXCursorKind kind, int xdata, const void *data[3]
    ffi.cbind(CXCursor, [.int, .int, [os.CArray n: 3, elem: .voidPtr]])

    -- const void* data, unsigned private_flags
    ffi.cbind(CXString, [.voidPtr, .uint])

    -- const void* ptr_data [2], unsigned int_data
    ffi.cbind(CXSourceLocation, [[os.CArray n: 2, elem: .voidPtr], .uint])

    -- CXIndex (int excludeDeclarationsFromPCH, int displayDiagnostics)
    ffi.cfunc('clang_createIndex', [.int, .int], .voidPtr)

    -- CXTranslationUnit (CXIndex CIdx, const char *source_filename, const char *const *command_line_args, int num_command_line_args, struct CXUnsavedFile *unsaved_files, unsigned num_unsaved_files, unsigned options)
    ffi.cfunc('clang_parseTranslationUnit', [
        .voidPtr, .charPtr, .voidPtr, .int, .voidPtr, .uint, .uint], .voidPtr)

    -- unsigned (CXTranslationUnit Unit)
    ffi.cfunc('clang_getNumDiagnostics', [.voidPtr], .int)

    -- CXDiagnostic (CXTranslationUnit Unit, unsigned Index)
    ffi.cfunc('clang_getDiagnostic', [.voidPtr, .int], .voidPtr)

    -- CXString (CXDiagnostic)
    ffi.cfunc('clang_getDiagnosticSpelling', [.voidPtr], .voidPtr)

    -- CXCursor (CXTranslationUnit)   
    ffi.cfunc('clang_getTranslationUnitCursor', [.voidPtr], CXCursor)

    -- unsigned (CXCursor parent, CXCursorVisitor visitor, CXClientData client_data)
    ffi.cfunc('clang_visitChildren', [CXCursor, .funcPtr, .voidPtr], .uint)

    -- CXString (CXCursor)
    ffi.cfunc('clang_getCursorSpelling', [CXCursor], CXString)

    -- CXString (CXCursor)
    ffi.cfunc('clang_getCursorDisplayName', [CXCursor], CXString)

    -- const char* (CXString string)   
    ffi.cfunc('clang_getCString', [CXString], .charPtr)

    -- CXSourceLocation (CXCursor)
    ffi.cfunc('clang_getCursorLocation', [CXCursor], CXSourceLocation)

    -- int (CXSourceLocation location)   
    ffi.cfunc('clang_Location_isInSystemHeader', [CXSourceLocation], .int)

    -- unsigned (CXCursor C)
    ffi.cfunc('clang_Cursor_isMacroBuiltin', [CXCursor], .uint)

    -- unsigned (CXCursor C)
    ffi.cfunc('clang_Cursor_isMacroFunctionLike', [CXCursor], .uint)

    -- CXType (CXCursor C)
    ffi.cfunc('clang_getCursorType', [CXCursor], CXType)

    -- CXCursor (CXType T)
    ffi.cfunc('clang_getTypeDeclaration', [CXType], CXCursor)

    -- CXString (CXType CT)   
    ffi.cfunc('clang_getTypedefName', [CXType], CXString)

    -- CXString (CXType CT)   
    ffi.cfunc('clang_getTypeSpelling', [CXType], CXString)

    -- CXType  (CXCursor C)   
    ffi.cfunc('clang_getTypedefDeclUnderlyingType', [CXCursor], CXType)

    -- long long (CXType T)   
    ffi.cfunc('clang_getNumElements', [CXType], .long)

    -- CXType (CXType T)   
    ffi.cfunc('clang_getElementType', [CXType], CXType)

    -- long long (CXCursor C)   
    ffi.cfunc('clang_getEnumConstantDeclValue', [CXCursor], .long)

    -- CXType (CXType T)   
    ffi.cfunc('clang_getResultType', [CXType], CXType)

    -- int (CXType T)
    ffi.cfunc('clang_getNumArgTypes', [CXType], .int)

    -- CXType (CXType T, unsigned i)
    ffi.cfunc('clang_getArgType', [CXType, .uint], CXType)

    -- CXCursor (CXCursor C, unsigned i)
    ffi.cfunc('clang_Cursor_getArgument', [CXCursor, .uint], CXCursor)

    -- CXEvalResult (CXCursor C)   
    ffi.cfunc('clang_Cursor_Evaluate', [CXCursor], .voidPtr)

    -- CXEvalResultKind (CXEvalResult E)   
    ffi.cfunc('clang_EvalResult_getKind', [.voidPtr], .int)

    -- void (CXEvalResult E)   
    ffi.cfunc('clang_EvalResult_dispose', [.voidPtr], .void)

    -- long long (CXEvalResult E)   
    ffi.cfunc('clang_EvalResult_getAsLongLong', [.voidPtr], .long)

    -- const char *(CXEvalResult E)
    ffi.cfunc('clang_EvalResult_getAsStr', [.voidPtr], .charPtr)

    -- double (CXEvalResult E)
    ffi.cfunc('clang_EvalResult_getAsDouble', [.voidPtr], .double)

    -- CXCursor (CXCursor cursor)
    ffi.cfunc('clang_Cursor_getVarDeclInitializer', [CXCursor], CXCursor)

    -- CXType (CXCursor C)
    ffi.cfunc('clang_Cursor_getReceiverType', [CXCursor], CXType)

    -- CXType clang_Type_getNamedType(CXType T);
    ffi.cfunc('clang_Type_getNamedType', [CXType], CXType)

    return ffi.bindLib('libclang.dylib')
    -- return ffi.bindLib('/Library/Developer/CommandLineTools/usr/lib/libclang.dylib')
