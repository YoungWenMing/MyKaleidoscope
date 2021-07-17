# ifndef CODEGEN_H
# define CODEGEN_H

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "src/parser.h"

#include <map>
#include <string>

namespace Kaleidoscope {

using namespace llvm;

class CodegenContext {
  std::unique_ptr<LLVMContext> TheContext;
  std::unique_ptr<Module> TheModule;
  std::unique_ptr<IRBuilder<>> Builder;
  std::map<std::string, Value*> ValMap;

 public:
  CodegenContext() :
      ValMap() {
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("MYJIT", *TheContext);
    Builder = std::make_unique<IRBuilder<>>(*TheContext);
  }

  LLVMContext& get_llvmcontext() { return *TheContext; }

  IRBuilder<>& get_irbuilder() { return *Builder; }

  Module* get_moduleptr() { return TheModule.get(); }

  std::map<std::string, Value*>& get_valmap() { return ValMap; }
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