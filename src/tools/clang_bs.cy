use os

-- Manual bindings for clang. This is used to bootstrap cbindgen.

var .CXEval_UnExposed = 0
var .CXEval_Int = 1
var .CXEval_Float = 2
var .CXEval_StrLiteral = 4

var .CXType_Void = 2
var .CXType_Bool = 3
var .CXType_UChar = 5
var .CXType_UShort = 8
var .CXType_UInt = 9
var .CXType_ULong = 10
var .CXType_ULongLong = 11
var .CXType_Char_S = 13
var .CXType_SChar = 14
var .CXType_Short = 16
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
var .CXCursor_LinkageSpec = 23
var .CXCursor_TypeRef = 43
var .CXCursor_InvalidFile = 70
var .CXCursor_UnexposedExpr = 100
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
    ptr_data [2]*void
    int_data int

type CXString:
    data *void 
    private_flags int

type CXCursor:
    kind  int
    xdata int
    data  [3]*void

type CXType:
    kind int
    data [2]*void

func getNumDiagnostics(a *void) int:
    return lib['clang_getNumDiagnostics'](a)

func getDiagnostic(a *void, i int) *void:
    return lib['clang_getDiagnostic'](a, i)

func getDiagnosticSpelling(a *void) *void:
    return lib['clang_getDiagnosticSpelling'](a)

func parseTranslationUnit(CIdx *void, source_filename *void, command_line_args *void, num_command_line_args int, unsaved_files *void, num_unsaved_files int, options int) *void:
    return lib['clang_parseTranslationUnit'](CIdx, source_filename, command_line_args, num_command_line_args, unsaved_files, num_unsaved_files, options)

func getTranslationUnitCursor(a *void) CXCursor:
    return lib['clang_getTranslationUnitCursor'](a)

func visitChildren(parent CXCursor, visitor *void, client_data *void) int:
    return lib['clang_visitChildren'](parent, visitor, client_data)

func createIndex(excludeDeclarationsFromPCH int, displayDiagnostics int) *void:
    return lib['clang_createIndex'](excludeDeclarationsFromPCH, displayDiagnostics)

func getCursorDisplayName(a CXCursor) CXString:
    return lib['clang_getCursorDisplayName'](a)

func getCursorLocation(a CXCursor) CXSourceLocation:
    return lib['clang_getCursorLocation'](a)

func getCString(str CXString) *void:
    return lib['clang_getCString'](str)

func getCursorSpelling(a CXCursor) CXString:
    return lib['clang_getCursorSpelling'](a)

func getCursorType(C CXCursor) CXType:
    return lib['clang_getCursorType'](C)

func Location_isInSystemHeader(location CXSourceLocation) int:
    return lib['clang_Location_isInSystemHeader'](location)

func Cursor_isMacroBuiltin(C CXCursor) int:
    return lib['clang_Cursor_isMacroBuiltin'](C)

func Cursor_isMacroFunctionLike(C CXCursor) int:
    return lib['clang_Cursor_isMacroFunctionLike'](C)

func Cursor_getVarDeclInitializer(cursor CXCursor) CXCursor:
    return lib['clang_Cursor_getVarDeclInitializer'](cursor)

func getTypedefDeclUnderlyingType(C CXCursor) CXType:
    return lib['clang_getTypedefDeclUnderlyingType'](C)

func getNumElements(T CXType) int:
    return lib['clang_getNumElements'](T)

func getElementType(T CXType) CXType:
    return lib['clang_getElementType'](T)

func getTypeDeclaration(T CXType) CXCursor:
    return lib['clang_getTypeDeclaration'](T)

func getTypedefName(CT CXType) CXString:
    return lib['clang_getTypedefName'](CT)

func getTypeSpelling(CT CXType) CXString:
    return lib['clang_getTypeSpelling'](CT)

func getResultType(T CXType) CXType:
    return lib['clang_getResultType'](T)

func getNumArgTypes(T CXType) int:
    return lib['clang_getNumArgTypes'](T)

func getArgType(T CXType, i int) CXType:
    return lib['clang_getArgType'](T, i)

func Cursor_getArgument(C CXCursor, i int) CXCursor:
    return lib['clang_Cursor_getArgument'](C, i)

func getEnumConstantDeclValue(C CXCursor) int:
    return lib['clang_getEnumConstantDeclValue'](C)

func Cursor_Evaluate(C CXCursor) *void:
    return lib['clang_Cursor_Evaluate'](C)

func EvalResult_getKind(E *void) int:
    return lib['clang_EvalResult_getKind'](E)

func EvalResult_dispose(E *void) void:
    lib['clang_EvalResult_dispose'](E)

func EvalResult_getAsLongLong(E *void) int:
    return lib['clang_EvalResult_getAsLongLong'](E)

func EvalResult_getAsDouble(E *void) float:
    return lib['clang_EvalResult_getAsDouble'](E)

func EvalResult_getAsStr(E *void) *void:
    return lib['clang_EvalResult_getAsStr'](E)

var .lib = load(ffi) -- Make lib dependent on ffi.
dyn .ffi = false

func load(dummy any) Map:
    var ffi_ = os.newFFI()

    -- enum CXTypeKind kind, void *data[2]
    ffi_.cbind(CXType, .{symbol.int, os.CArray{n=2, elem=symbol.voidPtr}})

    -- enum CXCursorKind kind, int xdata, const void *data[3]
    ffi_.cbind(CXCursor, .{symbol.int, symbol.int, os.CArray{n=3, elem=symbol.voidPtr}})

    -- const void* data, unsigned private_flags
    ffi_.cbind(CXString, .{symbol.voidPtr, symbol.uint})

    -- const void* ptr_data [2], unsigned int_data
    ffi_.cbind(CXSourceLocation, .{os.CArray{n=2, elem=symbol.voidPtr}, symbol.uint})

    -- CXIndex (int excludeDeclarationsFromPCH, int displayDiagnostics)
    ffi_.cfunc('clang_createIndex', .{symbol.int, symbol.int}, symbol.voidPtr)

    -- CXTranslationUnit (CXIndex CIdx, const char *source_filename, const char *const *command_line_args, int num_command_line_args, struct CXUnsavedFile *unsaved_files, unsigned num_unsaved_files, unsigned options)
    ffi_.cfunc('clang_parseTranslationUnit', .{
        symbol.voidPtr, symbol.charPtr, symbol.voidPtr, symbol.int, symbol.voidPtr, symbol.uint, symbol.uint}, symbol.voidPtr)

    -- unsigned (CXTranslationUnit Unit)
    ffi_.cfunc('clang_getNumDiagnostics', .{symbol.voidPtr}, symbol.int)

    -- CXDiagnostic (CXTranslationUnit Unit, unsigned Index)
    ffi_.cfunc('clang_getDiagnostic', .{symbol.voidPtr, symbol.int}, symbol.voidPtr)

    -- CXString (CXDiagnostic)
    ffi_.cfunc('clang_getDiagnosticSpelling', .{symbol.voidPtr}, symbol.voidPtr)

    -- CXCursor (CXTranslationUnit)   
    ffi_.cfunc('clang_getTranslationUnitCursor', .{symbol.voidPtr}, CXCursor)

    -- unsigned (CXCursor parent, CXCursorVisitor visitor, CXClientData client_data)
    ffi_.cfunc('clang_visitChildren', .{CXCursor, symbol.funcPtr, symbol.voidPtr}, symbol.uint)

    -- CXString (CXCursor)
    ffi_.cfunc('clang_getCursorSpelling', .{CXCursor}, CXString)

    -- CXString (CXCursor)
    ffi_.cfunc('clang_getCursorDisplayName', .{CXCursor}, CXString)

    -- const char* (CXString string)   
    ffi_.cfunc('clang_getCString', .{CXString}, symbol.charPtr)

    -- CXSourceLocation (CXCursor)
    ffi_.cfunc('clang_getCursorLocation', .{CXCursor}, CXSourceLocation)

    -- int (CXSourceLocation location)   
    ffi_.cfunc('clang_Location_isInSystemHeader', .{CXSourceLocation}, symbol.int)

    -- unsigned (CXCursor C)
    ffi_.cfunc('clang_Cursor_isMacroBuiltin', .{CXCursor}, symbol.uint)

    -- unsigned (CXCursor C)
    ffi_.cfunc('clang_Cursor_isMacroFunctionLike', .{CXCursor}, symbol.uint)

    -- CXType (CXCursor C)
    ffi_.cfunc('clang_getCursorType', .{CXCursor}, CXType)

    -- CXCursor (CXType T)
    ffi_.cfunc('clang_getTypeDeclaration', .{CXType}, CXCursor)

    -- CXString (CXType CT)   
    ffi_.cfunc('clang_getTypedefName', .{CXType}, CXString)

    -- CXString (CXType CT)   
    ffi_.cfunc('clang_getTypeSpelling', .{CXType}, CXString)

    -- CXType  (CXCursor C)   
    ffi_.cfunc('clang_getTypedefDeclUnderlyingType', .{CXCursor}, CXType)

    -- long long (CXType T)   
    ffi_.cfunc('clang_getNumElements', .{CXType}, symbol.long)

    -- CXType (CXType T)   
    ffi_.cfunc('clang_getElementType', .{CXType}, CXType)

    -- long long (CXCursor C)   
    ffi_.cfunc('clang_getEnumConstantDeclValue', .{CXCursor}, symbol.long)

    -- CXType (CXType T)   
    ffi_.cfunc('clang_getResultType', .{CXType}, CXType)

    -- int (CXType T)
    ffi_.cfunc('clang_getNumArgTypes', .{CXType}, symbol.int)

    -- CXType (CXType T, unsigned i)
    ffi_.cfunc('clang_getArgType', .{CXType, symbol.uint}, CXType)

    -- CXCursor (CXCursor C, unsigned i)
    ffi_.cfunc('clang_Cursor_getArgument', .{CXCursor, symbol.uint}, CXCursor)

    -- CXEvalResult (CXCursor C)   
    ffi_.cfunc('clang_Cursor_Evaluate', .{CXCursor}, symbol.voidPtr)

    -- CXEvalResultKind (CXEvalResult E)   
    ffi_.cfunc('clang_EvalResult_getKind', .{symbol.voidPtr}, symbol.int)

    -- void (CXEvalResult E)   
    ffi_.cfunc('clang_EvalResult_dispose', .{symbol.voidPtr}, symbol.void)

    -- long long (CXEvalResult E)   
    ffi_.cfunc('clang_EvalResult_getAsLongLong', .{symbol.voidPtr}, symbol.long)

    -- const char *(CXEvalResult E)
    ffi_.cfunc('clang_EvalResult_getAsStr', .{symbol.voidPtr}, symbol.charPtr)

    -- double (CXEvalResult E)
    ffi_.cfunc('clang_EvalResult_getAsDouble', .{symbol.voidPtr}, symbol.double)

    -- CXCursor (CXCursor cursor)
    ffi_.cfunc('clang_Cursor_getVarDeclInitializer', .{CXCursor}, CXCursor)

    -- CXType (CXCursor C)
    ffi_.cfunc('clang_Cursor_getReceiverType', .{CXCursor}, CXType)

    -- CXType clang_Type_getNamedType(CXType T);
    ffi_.cfunc('clang_Type_getNamedType', .{CXType}, CXType)

    ffi = ffi_

    return ffi.bindLib(Option[String].some('libclang.dylib'))
    -- return ffi.bindLib('/Library/Developer/CommandLineTools/usr/lib/libclang.dylib')
