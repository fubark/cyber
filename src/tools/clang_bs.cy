use os
use c
use meta

type c_int = c.c_int
type c_uint = c.c_uint

-- Manual bindings for clang. This is used to bootstrap cbindgen.

#if meta.is_vm_target():
    -- #c.bindLib('/Library/Developer/CommandLineTools/usr/lib/libclang.dylib')
    #c.bindLib('libclang.dylib')

const CXEval_UnExposed i32 = 0
const CXEval_Int i32 = 1
const CXEval_Float i32 = 2
const CXEval_StrLiteral i32 = 4

const CXType_Void i32 = 2
const CXType_Bool i32 = 3
const CXType_UChar i32 = 5
const CXType_UShort i32 = 8
const CXType_UInt i32 = 9
const CXType_ULong i32 = 10
const CXType_ULongLong i32 = 11
const CXType_Char_S i32 = 13
const CXType_SChar i32 = 14
const CXType_Short i32 = 16
const CXType_Int i32 = 17
const CXType_Long i32 = 18
const CXType_LongLong i32 = 19
const CXType_Float i32 = 21
const CXType_Double i32 = 22
const CXType_Float16 i32 = 32
const CXType_Pointer i32 = 101
const CXType_Record i32 = 105
const CXType_Enum i32 = 106
const CXType_Typedef i32 = 107
const CXType_FunctionProto i32 = 111
const CXType_ConstantArray i32 = 112
const CXType_IncompleteArray i32 = 114
const CXType_Auto i32 = 118
const CXType_Elaborated i32 = 119

const CXCursor_UnexposedDecl i32 = 1
const CXCursor_StructDecl i32 = 2
const CXCursor_UnionDecl i32 = 3
const CXCursor_EnumDecl i32 = 5
const CXCursor_FieldDecl i32 = 6
const CXCursor_FunctionDecl i32 = 8
const CXCursor_VarDecl i32 = 9
const CXCursor_TypedefDecl i32 = 20
const CXCursor_LinkageSpec i32 = 23
const CXCursor_TypeRef i32 = 43
const CXCursor_InvalidFile i32 = 70
const CXCursor_UnexposedExpr i32 = 100
const CXCursor_IntegerLiteral i32 = 106
const CXCursor_CStyleCastExpr i32 = 117
const CXCursor_InitListExpr i32 = 119
const CXCursor_CXXFunctionalCastExpr i32 = 128
const CXCursor_CXXNullPtrLiteralExpr i32 = 131
const CXCursor_MacroDefinition i32 = 501
const CXCursor_MacroExpansion i32 = 502
const CXCursor_InclusionDirective i32 = 503
const CXCursor_StaticAssert i32 = 602

const CXTranslationUnit_DetailedPreprocessingRecord = c_int(0x01)
const CXTranslationUnit_SkipFunctionBodies          = c_int(0x40)
const CXTranslationUnit_KeepGoing                   = c_int(0x200)
const CXTranslationUnit_SingleFileParse             = c_int(0x400)

const CXChildVisit_Break    = c_int(0)
const CXChildVisit_Continue = c_int(1)
const CXChildVisit_Recurse  = c_int(2)

const CXTypeLayoutError_Incomplete = c_int(-2)

type CXFile = Ptr[void]

type CXSourceLocation cstruct:
    ptr_data [2]Ptr[void]
    int_data c_uint

-- const void* data, unsigned private_flags
type CXString cstruct:
    data Ptr[byte]
    private_flags c_uint

-- enum CXCursorKind kind, int xdata, const void *data[3]
type CXCursor cstruct:
    kind  i32
    xdata i32
    data  [3]Ptr[void]

-- enum CXTypeKind kind, void *data[2]
type CXType cstruct:
    kind i32
    data [2]Ptr[void]

#[extern='clang_equalCursors']
fn equalCursors(x CXCursor, y CXCursor) -> c_uint

-- unsigned (CXTranslationUnit Unit)
#[extern='clang_getNumDiagnostics']
fn getNumDiagnostics(a Ptr[void]) -> c_uint

#[extern='clang_getCursorDefinition']
fn getCursorDefinition(x CXCursor) -> CXCursor

-- CXDiagnostic (CXTranslationUnit Unit, unsigned Index)
#[extern]
fn clang_getDiagnostic(a Ptr[void], i c_uint) -> Ptr[void]

fn getDiagnostic(a Ptr[void], i int) -> Ptr[void]:
    return clang_getDiagnostic(a, as i)

-- CXString (CXDiagnostic)
#[extern='clang_getDiagnosticSpelling']
fn getDiagnosticSpelling(a Ptr[void]) -> CXString

#[extern='clang_getExpansionLocation']
fn getExpansionLocation(location CXSourceLocation, file Ptr[CXFile], line Ptr[c_uint], column Ptr[c_uint], offset Ptr[c_uint])

#[extern='clang_getFileLocation']
fn getFileLocation(location CXSourceLocation, file Ptr[CXFile], line Ptr[c_uint], column Ptr[c_uint], offset Ptr[c_uint])

#[extern='clang_getSpellingLocation']
fn getSpellingLocation(location CXSourceLocation, file Ptr[CXFile], line Ptr[c_uint], column Ptr[c_uint], offset Ptr[c_uint])

#[extern='clang_isCursorDefinition']
fn isCursorDefinition(x CXCursor) -> c_uint

-- CXTranslationUnit (CXIndex CIdx, const char *source_filename, const char *const *command_line_args, int num_command_line_args, struct CXUnsavedFile *unsaved_files)
#[extern='clang_parseTranslationUnit']
fn parseTranslationUnit(CIdx Ptr[void], source_filename Ptr[byte], command_line_args Ptr[Ptr[byte]], num_command_line_args i32, unsaved_files Ptr[void], num_unsaved_files c_uint, options c_uint) -> Ptr[void]

-- CXCursor (CXTranslationUnit)   
#[extern='clang_getTranslationUnitCursor']
fn getTranslationUnitCursor(a Ptr[void]) -> CXCursor

-- enum CXChildVisitResult (*CXCursorVisitor)(CXCursor cursor, CXCursor parent, CXClientData client_data)
type CXCursorVisitor = #[extern] fn(cursor CXCursor, parent CXCursor, client_data Ptr[void]) -> i32

#[extern='clang_visitChildren']
fn visitChildren(parent CXCursor, visitor CXCursorVisitor, client_data Ptr[void]) -> c_uint

-- CXIndex (int excludeDeclarationsFromPCH, int displayDiagnostics)
#[extern='clang_createIndex']
fn createIndex(excludeDeclarationsFromPCH i32, displayDiagnostics i32) -> Ptr[void]

#[extern='clang_getCanonicalType']
fn getCanonicalType(a CXType) -> CXType

-- CXString (CXCursor)
#[extern='clang_getCursorDisplayName']
fn getCursorDisplayName(a CXCursor) -> CXString

-- CXSourceLocation (CXCursor)
#[extern='clang_getCursorLocation']
fn getCursorLocation(a CXCursor) -> CXSourceLocation

-- const char* (CXString string)   
#[extern='clang_getCString']
fn getCString(str CXString) -> Ptr[byte]

-- CXString (CXCursor)
#[extern='clang_getCursorSpelling']
fn getCursorSpelling(a CXCursor) -> CXString

-- CXType (CXCursor C)
#[extern='clang_getCursorType']
fn getCursorType(C CXCursor) -> CXType

#[extern='clang_getFileName']
fn getFileName(SFile CXFile) -> CXString

-- int (CXSourceLocation location)   
#[extern='clang_Location_isInSystemHeader']
fn Location_isInSystemHeader(location CXSourceLocation) -> i32

#[extern='clang_Cursor_isMacroBuiltin']
fn Cursor_isMacroBuiltin(C CXCursor) -> c_uint

#[extern='clang_Cursor_isMacroFunctionLike']
fn Cursor_isMacroFunctionLike(C CXCursor) -> c_uint

#[extern='clang_Cursor_isNull']
fn Cursor_isNull(cursor CXCursor) -> c_int

-- CXCursor (CXCursor cursor)
#[extern='clang_Cursor_getVarDeclInitializer']
fn Cursor_getVarDeclInitializer(cursor CXCursor) -> CXCursor

-- CXType  (CXCursor C)   
#[extern='clang_getTypedefDeclUnderlyingType']
fn getTypedefDeclUnderlyingType(C CXCursor) -> CXType

-- long long (CXType T)   
#[extern='clang_getNumElements']
fn getNumElements(T CXType) -> int

#[extern='clang_getElementType']
fn getElementType(T CXType) -> CXType

#[extern='clang_getPointeeType']
fn getPointeeType(T CXType) -> CXType

#[extern='clang_getTypeDeclaration']
fn getTypeDeclaration(T CXType) -> CXCursor

#[extern='clang_getTypedefName']
fn getTypedefName(CT CXType) -> CXString

#[extern='clang_getTypeSpelling']
fn getTypeSpelling(CT CXType) -> CXString

#[extern='clang_getResultType']
fn getResultType(T CXType) -> CXType

#[extern='clang_getNumArgTypes']
fn getNumArgTypes(T CXType) -> c_int

#[extern]
fn clang_getArgType(T CXType, i c_uint) -> CXType

fn getArgType(T CXType, i int) -> CXType:
    return clang_getArgType(T, i32(i))

#[extern]
fn clang_Cursor_getArgument(C CXCursor, i c_uint) -> CXCursor

fn Cursor_getArgument(C CXCursor, i int) -> CXCursor:
    return clang_Cursor_getArgument(C, i32(i))

-- long long (CXCursor C)   
#[extern='clang_getEnumConstantDeclValue']
fn getEnumConstantDeclValue(C CXCursor) -> int

-- CXEvalResult (CXCursor C)   
#[extern='clang_Cursor_Evaluate']
fn Cursor_Evaluate(C CXCursor) -> Ptr[void]

-- CXEvalResultKind (CXEvalResult E)   
#[extern='clang_EvalResult_getKind']
fn EvalResult_getKind(E Ptr[void]) -> i32

-- void (CXEvalResult E)
#[extern='clang_EvalResult_dispose']
fn EvalResult_dispose(E Ptr[void]) -> void

-- long long (CXEvalResult E)   
#[extern='clang_EvalResult_getAsLongLong']
fn EvalResult_getAsLongLong(E Ptr[void]) -> int

-- double (CXEvalResult E)
#[extern='clang_EvalResult_getAsDouble']
fn EvalResult_getAsDouble(E Ptr[void]) -> float

-- const char *(CXEvalResult E)
#[extern='clang_EvalResult_getAsStr']
fn EvalResult_getAsStr(E Ptr[void]) -> Ptr[byte]

-- CXType (CXCursor C)
#[extern='clang_Cursor_getReceiverType']
fn Cursor_getReceiverType(C CXCursor) -> CXType

-- CXType (CXType T)
#[extern='clang_Type_getNamedType']
fn Type_getNamedType(T CXType) -> CXType

#[extern='clang_Type_getSizeOf']
fn Type_getSizeOf(T CXType) -> int
