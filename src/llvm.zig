const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Analysis.h");
    @cInclude("llvm-c/ExecutionEngine.h");
});

pub usingnamespace c;

/// Top Level.
pub const DisposeMessage = c.LLVMDisposeMessage;
pub const True: c.LLVMBool = 1;
pub const False: c.LLVMBool = 0;

/// Types.
pub const TypeRef = c.LLVMTypeRef;
pub const IntType = c.LLVMIntType;
pub const IntTypeInContext = c.LLVMIntTypeInContext;
pub const FloatType = c.LLVMFloatType;
pub const DoubleType = c.LLVMDoubleType;

/// Context.
pub const ContextRef = c.LLVMContextRef;
pub const ContextCreate = c.LLVMContextCreate;

/// Module.
pub const ModuleRef = c.LLVMModuleRef;
pub const ModuleCreateWithName = c.LLVMModuleCreateWithName;
pub const ModuleCreateWithNameInContext = c.LLVMModuleCreateWithNameInContext;
pub const SetTarget = c.LLVMSetTarget;
pub const SetDataLayout = c.LLVMSetDataLayout;
pub const DumpModule = c.LLVMDumpModule;

/// Function.
pub const FunctionType = c.LLVMFunctionType;
pub const AddFunction = c.LLVMAddFunction;
pub const DeleteFunction = c.LLVMDeleteFunction;
pub const GetParam = c.LLVMGetParam;
pub const GetLastBasicBlock = c.LLVMGetLastBasicBlock;

/// Basic block.
pub const BasicBlockRef = c.LLVMBasicBlockRef;
pub const AppendBasicBlock = c.LLVMAppendBasicBlock;
pub const AppendBasicBlockInContext = c.LLVMAppendBasicBlockInContext;
pub const CreateBasicBlockInContext = c.LLVMCreateBasicBlockInContext;
pub const InsertBasicBlockInContext = c.LLVMInsertBasicBlockInContext;
pub const AppendExistingBasicBlock = c.LLVMAppendExistingBasicBlock;

/// Builder/Inst.
pub const ValueRef = c.LLVMValueRef;
pub const DumpValue = c.LLVMDumpValue;
pub const BuilderRef = c.LLVMBuilderRef;
pub const CreateBuilder = c.LLVMCreateBuilder;
pub const CreateBuilderInContext = c.LLVMCreateBuilderInContext;
pub const PositionBuilderAtEnd = c.LLVMPositionBuilderAtEnd;
pub const BuildICmp = c.LLVMBuildICmp;
pub const IntEQ = c.LLVMIntEQ;
pub const IntNE = c.LLVMIntNE;
pub const IntUGT = c.LLVMIntUGT;
pub const IntUGE = c.LLVMIntUGE;
pub const IntULT = c.LLVMIntULT;
pub const IntULE = c.LLVMIntULE;
pub const IntSGT = c.LLVMIntSGT;
pub const IntSGE = c.LLVMIntSGE;
pub const IntSLT = c.LLVMIntSLT;
pub const IntSLE = c.LLVMIntSLE;
pub const BuildFCmp = c.LLVMBuildFCmp;
pub const RealPredicateFalse = c.LLVMRealPredicateFalse;
pub const RealOEQ = c.LLVMRealOEQ;         
pub const RealOGT = c.LLVMRealOGT;         
pub const RealOGE = c.LLVMRealOGE;         
pub const RealOLT = c.LLVMRealOLT;         
pub const RealOLE = c.LLVMRealOLE;         
pub const RealONE = c.LLVMRealONE;         
pub const RealORD = c.LLVMRealORD;         
pub const RealUNO = c.LLVMRealUNO;         
pub const RealUEQ = c.LLVMRealUEQ;         
pub const RealUGT = c.LLVMRealUGT;         
pub const RealUGE = c.LLVMRealUGE;         
pub const RealULT = c.LLVMRealULT;         
pub const RealULE = c.LLVMRealULE;         
pub const RealUNE = c.LLVMRealUNE;         
pub const RealPredicate = c.LLVMRealPredicate;         
pub const RealPredicateTrue = c.LLVMRealPredicateTrue;
pub const GetInsertBlock = c.LLVMGetInsertBlock;
pub const BuildBr = c.LLVMBuildBr;
pub const BuildCondBr = c.LLVMBuildCondBr;
pub const ConstInt = c.LLVMConstInt;
pub const ConstReal = c.LLVMConstReal;
pub const BuildRetVoid = c.LLVMBuildRetVoid;
pub const BuildRet = c.LLVMBuildRet;
pub const BuildAdd = c.LLVMBuildAdd;
pub const BuildSub = c.LLVMBuildSub;
pub const BuildCall2 = c.LLVMBuildCall2;

/// Analysis.
pub const VerifyModule = c.LLVMVerifyModule;
pub const VerifyFunction = c.LLVMVerifyFunction;
pub const AbortProcessAction = c.LLVMAbortProcessAction; // verifier will print to stderr and abort()
pub const PrintMessageAction = c.LLVMPrintMessageAction; // verifier will print to stderr and return 1
pub const ReturnStatusAction = c.LLVMReturnStatusAction;

/// PassManager.
pub const PassManagerRef = c.LLVMPassManagerRef;
pub const CreatePassManager = c.LLVMCreatePassManager;
pub const RunPassManager = c.LLVMRunPassManager;

/// Target/TargetMachine.
pub const InitializeNativeTarget = c.LLVMInitializeNativeTarget;
pub const InitializeAllAsmPrinters = c.LLVMInitializeAllAsmPrinters;
pub const GetDefaultTargetTriple = c.LLVMGetDefaultTargetTriple;
pub const TargetRef = c.LLVMTargetRef;
pub const GetTargetFromTriple = c.LLVMGetTargetFromTriple;
pub const TargetMachineRef = c.LLVMTargetMachineRef;
pub const CreateTargetMachine = c.LLVMCreateTargetMachine;
pub const CodeGenLevelNone = c.LLVMCodeGenLevelNone;
pub const CodeGenLevelLess = c.LLVMCodeGenLevelLess;
pub const CodeGenLevelDefault = c.LLVMCodeGenLevelDefault;
pub const CodeGenLevelAggressive = c.LLVMCodeGenLevelAggressive;
pub const RelocDefault = c.LLVMRelocDefault;
pub const RelocStatic = c.LLVMRelocStatic;
pub const RelocPIC = c.LLVMRelocPIC;
pub const RelocDynamicNoPic = c.LLVMRelocDynamicNoPic;
pub const RelocROPI = c.LLVMRelocROPI;
pub const RelocRWPI = c.LLVMRelocRWPI;
pub const RelocROPI_RWPI = c.LLVMRelocROPI_RWPI;
pub const CodeModelDefault = c.LLVMCodeModelDefault;
pub const CodeModelJITDefault = c.LLVMCodeModelJITDefault;
pub const CodeModelTiny = c.LLVMCodeModelTiny;
pub const CodeModelSmall = c.LLVMCodeModelSmall;
pub const CodeModelKernel = c.LLVMCodeModelKernel;
pub const CodeModelMedium = c.LLVMCodeModelMedium;
pub const CodeModelLarg = c.LLVMCodeModelLarg;
pub const CopyStringRepOfTargetData = c.LLVMCopyStringRepOfTargetData;
pub const CreateTargetDataLayout = c.LLVMCreateTargetDataLayout;
pub const TargetMachineEmitToFile = c.LLVMTargetMachineEmitToFile;
pub const TargetMachineEmitToMemoryBuffer = c.LLVMTargetMachineEmitToMemoryBuffer;
pub const CodeGenFileType = struct {
    pub const AssemblyFile = c.LLVMAssemblyFile;
    pub const ObjectFile = c.LLVMObjectFile;
};