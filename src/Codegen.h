# ifndef CODEGEN_H
# define CODEGEN_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "include/KaleidoscopeJIT.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/TargetSelect.h"

#include "src/parser.h"

#include <map>
#include <string>

namespace Kaleidoscope {

using namespace llvm;
using namespace llvm::orc;

typedef
std::map<Token::Value, std::unique_ptr<PrototypeAST>> OpProtoMap;

class CodegenContext {
  std::unique_ptr<LLVMContext> TheContext;
  std::unique_ptr<Module> TheModule;
  std::unique_ptr<IRBuilder<>> Builder;
  std::map<std::string, AllocaInst*> ValMap;

  // for code optimization
  std::unique_ptr<legacy::FunctionPassManager> TheFPM;
  ExitOnError ExitOnErr;
  std::unique_ptr<KaleidoscopeJIT> TheJIT;

  // for function code regeneration
  std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
  // for user-defined operators
  // OpProtoMap OpProtos;
  // Parser* const parser_;

 public:
  CodegenContext() {

    InitializeNativeTarget();
    InitializeNativeTargetAsmPrinter();
    InitializeNativeTargetAsmParser();


    TheJIT = ExitOnErr(KaleidoscopeJIT::Create());
    InitializeModuleAndPassManager();
  }

  LLVMContext& get_llvmcontext() { return *TheContext; }

  IRBuilder<>& get_irbuilder() { return *Builder; }

  Module* get_moduleptr() { return TheModule.get(); }

  std::map<std::string, AllocaInst*>& get_valmap() { return ValMap; }

  void InitializeModuleAndPassManager();

  void doOptimization(Function& func);

  void JITCompileToplevel(std::string func_name);

  Function* get_function(std::string name);

  void add_protos(std::unique_ptr<PrototypeAST> proto);

  void add_module();

  AllocaInst* CreateEntryBlockAlloca(
      Function* func, const std::string& var_name);
};

class CodegenDriver {
  const char* src_;
  Parser parser_;
  CodegenContext ctx_;

  void HandleToplevelExpression();
  void HandleExtern();
  void HandleDefinition();

 public:
  CodegenDriver(const char* src, size_t len);

  void run();
};

} // Kaleidoscope

#endif  // CODEGEN_H