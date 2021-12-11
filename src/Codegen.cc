
#include "src/Codegen-inl.h"
#include "src/token.h"

#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

#include <cstdarg>
#include <stdio.h>
#include <iostream>

namespace Kaleidoscope {

CodegenContext::CodegenContext(const char* src_name)
  : SourceName(src_name) {

  InitializeNativeTarget();
  InitializeNativeTargetAsmPrinter();
  InitializeNativeTargetAsmParser();

  TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
  InitializeModuleAndPassManager();

  InitializeMainFunction();
  InitializeMainScope();
  InitLLVMTypeMap();
}

CodegenContext::~CodegenContext() {
  // FIXME: do not use unique_ptr for mainFunction.
  MainFunction.release();
}

void CodegenContext::InitializeModuleAndPassManager() {
  // open a new module
  // this may be optimized
  TheContext = std::make_unique<LLVMContext>();
  TheModule = std::make_unique<Module>("MainModule", *TheContext);
  TheModule->setDataLayout(TheJIT->getDataLayout());
  TheModule->setSourceFileName(SourceName);

  Builder = std::make_unique<IRBuilder<>>(*TheContext);

  TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());

  TheFPM->add(createPromoteMemoryToRegisterPass());
  TheFPM->add(createInstructionCombiningPass());
  TheFPM->add(createReassociatePass());
  TheFPM->add(createGVNPass());
  TheFPM->add(createCFGSimplificationPass());

  TheFPM->doInitialization();
}

void CodegenContext::doOptimization(Function& func) {
  TheFPM->run(func);
}

void CodegenContext::JITCompileToplevel(std::string func_name) {
  auto RT = TheJIT->getMainJITDylib().createResourceTracker();

  auto TSM = ThreadSafeModule(std::move(TheModule), std::move(TheContext));
  ExitOnErr(TheJIT->addModule(std::move(TSM), RT));

  InitializeModuleAndPassManager();

  auto ExprSymbol = ExitOnErr(TheJIT->lookup(func_name));
  // the LLVM JIT compiler matches the native platform ABI, this means that
  // you can just cast the result pointer to a function pointer of that type
  // and call it directly
  double (*fp)() = (double(*)())static_cast<intptr_t>(ExprSymbol.getAddress());
  fprintf(stderr, "Evaluated to %f\n", fp());

  ExitOnErr(RT->remove());
}

Function* CodegenContext::get_function(std::string name) {
  // check the module 
  // every time we do a jit compilation, TheContext and TheModule
  // will be reinitialized again, thus the parsed functions will be 
  // abandon. Therefore, we maintain a map to restore the Prototype.
  Function* result = TheModule->getFunction(name);
  if (result) return result;

  // check the FunctionProtos map
  // auto entry = FunctionProtos.find(name);
  // if (entry != FunctionProtos.end())
  //   return entry->second->codegen(*this);
  return nullptr;
}

void CodegenContext::add_protos(std::unique_ptr<Prototype> proto) {
  FunctionProtos[proto->getName()] = std::move(proto);
}

void CodegenContext::add_module() {
  ExitOnErr(TheJIT->addModule(
      ThreadSafeModule(std::move(TheModule), std::move(TheContext))));
  InitializeModuleAndPassManager();
}

AllocaInst* CodegenContext::CreateEntryBlockAlloca(
    Function* func, const std::string& var_name) {
  IRBuilder<> tempB(&func->getEntryBlock(), func->getEntryBlock().begin());
  return tempB.CreateAlloca(
      Type::getDoubleTy(*TheContext), 0, var_name.c_str());
}

void CodegenContext::LogError(const char* format, ...) {
  AddCodegenError();
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
}

Value* CodegenContext::get_default_value(Token::Value val) {
  switch (val) {
    case Token::INT:
      return ConstantInt::get(get_llvmcontext(), APInt(32, 0));
    case Token::DOUBLE:
      return ConstantFP::get(get_llvmcontext(), APFloat(0.0));
    default:
      return nullptr;
  }
}

void CodegenContext::EnsureMainFunctionTerminate() {
  BasicBlock* last = Builder->GetInsertBlock();
  if (last->getTerminator() != nullptr) {
    ReturnInst::Create(*TheContext, 0, last);
  }
}

AllocaInst* ContextScope::find_val(const std::string& name) const {
  auto entry = ValMap.find(name);
  if (entry != ValMap.end())  return entry->second;
  return nullptr;
}

bool ContextScope::insert_val(const std::string& name, AllocaInst* allo) {
  auto entry = ValMap.find(name);
  if (entry == ValMap.end()) {
    ValMap.insert(std::pair<const std::string, AllocaInst*>(name, allo));
    return true;
  }
  return false;
}

CodegenDriver::CodegenDriver(const char* src_name, const char* src, size_t len) :
    script_(src, len), parser_(script_), ctx_(src_name) {}

void CodegenDriver::run() {
}

void CodegenDriver::generate_code() {
  ContextScope scope(ctx_);
  std::unique_ptr<Block> root = parser_.ParseToplevel();
  if (parser_.HasParserError()) {
    printf("\nWe got parser erorrs, please check your source code.\n");
    return;
  }
  root->codegen(ctx_);
  ctx_.EnsureMainFunctionTerminate();
#if 1
  if (ctx_.HasError())
    printf("Error(s) occured during code generation.\n");
  else
    ctx_.get_moduleptr()->dump();
#endif // DEBUG
}

// Question:
// 1> is a compiled function still available by calling TheJIT->lookup
//    after the module is reinitialized?
// -  The answer is yes. The Function is added to TheJIT after addModule
// -  in JITCompileToplevel. TheJIT can search functions across modules.

} // namespace Kaleidoscope
