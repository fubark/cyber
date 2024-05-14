use os

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
 
let .lib = load(ffi) -- Make lib dependent on ffi.
let .ffi = false

func load(dummy any):
    ffi = os.newFFI()

    -- enum CXTypeKind kind, void *data[2]
    ffi.cbind(CXType, [symbol.int, os.CArray{n: 2, elem: symbol.voidPtr}])

    -- enum CXCursorKind kind, int xdata, const void *data[3]
    ffi.cbind(CXCursor, [symbol.int, symbol.int, os.CArray{n: 3, elem: symbol.voidPtr}])

    -- const void* data, unsigned private_flags
    ffi.cbind(CXString, [symbol.voidPtr, symbol.uint])

    -- const void* ptr_data [2], unsigned int_data
    ffi.cbind(CXSourceLocation, [os.CArray{n: 2, elem: symbol.voidPtr}, symbol.uint])

    -- CXIndex (int excludeDeclarationsFromPCH, int displayDiagnostics)
    ffi.cfunc('clang_createIndex', [symbol.int, symbol.int], symbol.voidPtr)

    -- CXTranslationUnit (CXIndex CIdx, const char *source_filename, const char *const *command_line_args, int num_command_line_args, struct CXUnsavedFile *unsaved_files, unsigned num_unsaved_files, unsigned options)
    ffi.cfunc('clang_parseTranslationUnit', [
        symbol.voidPtr, symbol.charPtr, symbol.voidPtr, symbol.int, symbol.voidPtr, symbol.uint, symbol.uint], symbol.voidPtr)

    -- unsigned (CXTranslationUnit Unit)
    ffi.cfunc('clang_getNumDiagnostics', [symbol.voidPtr], symbol.int)

    -- CXDiagnostic (CXTranslationUnit Unit, unsigned Index)
    ffi.cfunc('clang_getDiagnostic', [symbol.voidPtr, symbol.int], symbol.voidPtr)

    -- CXString (CXDiagnostic)
    ffi.cfunc('clang_getDiagnosticSpelling', [symbol.voidPtr], symbol.voidPtr)

    -- CXCursor (CXTranslationUnit)   
    ffi.cfunc('clang_getTranslationUnitCursor', [symbol.voidPtr], CXCursor)

    -- unsigned (CXCursor parent, CXCursorVisitor visitor, CXClientData client_data)
    ffi.cfunc('clang_visitChildren', [CXCursor, symbol.funcPtr, symbol.voidPtr], symbol.uint)

    -- CXString (CXCursor)
    ffi.cfunc('clang_getCursorSpelling', [CXCursor], CXString)

    -- CXString (CXCursor)
    ffi.cfunc('clang_getCursorDisplayName', [CXCursor], CXString)

    -- const char* (CXString string)   
    ffi.cfunc('clang_getCString', [CXString], symbol.charPtr)

    -- CXSourceLocation (CXCursor)
    ffi.cfunc('clang_getCursorLocation', [CXCursor], CXSourceLocation)

    -- int (CXSourceLocation location)   
    ffi.cfunc('clang_Location_isInSystemHeader', [CXSourceLocation], symbol.int)

    -- unsigned (CXCursor C)
    ffi.cfunc('clang_Cursor_isMacroBuiltin', [CXCursor], symbol.uint)

    -- unsigned (CXCursor C)
    ffi.cfunc('clang_Cursor_isMacroFunctionLike', [CXCursor], symbol.uint)

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
    ffi.cfunc('clang_getNumElements', [CXType], symbol.long)

    -- CXType (CXType T)   
    ffi.cfunc('clang_getElementType', [CXType], CXType)

    -- long long (CXCursor C)   
    ffi.cfunc('clang_getEnumConstantDeclValue', [CXCursor], symbol.long)

    -- CXType (CXType T)   
    ffi.cfunc('clang_getResultType', [CXType], CXType)

    -- int (CXType T)
    ffi.cfunc('clang_getNumArgTypes', [CXType], symbol.int)

    -- CXType (CXType T, unsigned i)
    ffi.cfunc('clang_getArgType', [CXType, symbol.uint], CXType)

    -- CXCursor (CXCursor C, unsigned i)
    ffi.cfunc('clang_Cursor_getArgument', [CXCursor, symbol.uint], CXCursor)

    -- CXEvalResult (CXCursor C)   
    ffi.cfunc('clang_Cursor_Evaluate', [CXCursor], symbol.voidPtr)

    -- CXEvalResultKind (CXEvalResult E)   
    ffi.cfunc('clang_EvalResult_getKind', [symbol.voidPtr], symbol.int)

    -- void (CXEvalResult E)   
    ffi.cfunc('clang_EvalResult_dispose', [symbol.voidPtr], symbol.void)

    -- long long (CXEvalResult E)   
    ffi.cfunc('clang_EvalResult_getAsLongLong', [symbol.voidPtr], symbol.long)

    -- const char *(CXEvalResult E)
    ffi.cfunc('clang_EvalResult_getAsStr', [symbol.voidPtr], symbol.charPtr)

    -- double (CXEvalResult E)
    ffi.cfunc('clang_EvalResult_getAsDouble', [symbol.voidPtr], symbol.double)

    -- CXCursor (CXCursor cursor)
    ffi.cfunc('clang_Cursor_getVarDeclInitializer', [CXCursor], CXCursor)

    -- CXType (CXCursor C)
    ffi.cfunc('clang_Cursor_getReceiverType', [CXCursor], CXType)

    -- CXType clang_Type_getNamedType(CXType T);
    ffi.cfunc('clang_Type_getNamedType', [CXType], CXType)

    return ffi.bindLib(Option[String].some('libclang.dylib'))
    -- return ffi.bindLib('/Library/Developer/CommandLineTools/usr/lib/libclang.dylib')
