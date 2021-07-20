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

class CodegenContext {
  std::unique_ptr<LLVMContext> TheContext;
  std::unique_ptr<Module> TheModule;
  std::unique_ptr<IRBuilder<>> Builder;
  std::map<std::string, Value*> ValMap;

  // for code optimization
  std::unique_ptr<legacy::FunctionPassManager> TheFPM;
  ExitOnError ExitOnErr;
  std::unique_ptr<KaleidoscopeJIT> TheJIT;

  // for function code regeneration
  std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;

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

  std::map<std::string, Value*>& get_valmap() { return ValMap; }

  void InitializeModuleAndPassManager();

  void doOptimization(Function& func);

  void JITCompileToplevel(std::string func_name);

  Function* get_function(std::string name);

  void add_protos(std::unique_ptr<PrototypeAST> proto);

  void add_module();
};

class CodegenDriver {
  const char* src_;
  CodegenContext ctx_;
  Parser parser_;

  void HandleToplevelExpression();
  void HandleExtern();
  void HandleDefinition();

 public:
  CodegenDriver(const char* src);

  void run();
};

} // Kaleidoscope

#endif  // CODEGEN_H