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
std::map<Token::Value, std::unique_ptr<Prototype>> OpProtoMap;
typedef
std::vector<std::map<std::string, AllocaInst*>> ScopeStack;

class ContextScope;
class CodegenContext {
  friend class ContextScope;

  std::unique_ptr<LLVMContext> TheContext;
  std::unique_ptr<Module> TheModule;
  std::unique_ptr<Function> MainFunction;
  std::unique_ptr<IRBuilder<>> Builder;

  std::vector<ContextScope*> scope_stack_;
  ContextScope* current_scope_;

  // for code optimization
  std::unique_ptr<legacy::FunctionPassManager> TheFPM;
  ExitOnError ExitOnErr;
  std::unique_ptr<KaleidoscopeJIT> TheJIT;

  // for function code regeneration
  std::map<std::string, std::unique_ptr<Prototype>> FunctionProtos;

  inline void InitializeMainFunction();
  inline void InitializeMainScope();
  inline void DeinitializeMainScope();

 public:
  CodegenContext();
  ~CodegenContext();

  LLVMContext& get_llvmcontext() { return *TheContext; }

  IRBuilder<>& get_irbuilder() { return *Builder; }

  Module* get_moduleptr() { return TheModule.get(); }
  void InitializeModuleAndPassManager();

  void doOptimization(Function& func);

  void JITCompileToplevel(std::string func_name);

  Function* get_function(std::string name);
  Function* get_main_function() { return MainFunction.get(); }

  void add_protos(std::unique_ptr<Prototype> proto);
  void add_module();

  inline AllocaInst* find_val(const std::string& name);
  inline bool insert_val(const std::string& name, AllocaInst* val);
  inline BasicBlock* current_block() const;

  inline void EnterScope(BasicBlock*);
  inline void ExitScope();

  void EnsureMainFunctionTerminate();

  AllocaInst* CreateEntryBlockAlloca(
      Function* func, const std::string& var_name);

  void LogError(const char* format, ...);
};

class ContextScope {
  BasicBlock* bb_;
  std::map<const std::string, AllocaInst*> ValMap;
  friend class CodegenContext;
 public:
  ContextScope(BasicBlock* block)
      : bb_(block) {}
  ~ContextScope() {}

  AllocaInst* find_val(const std::string& name) const;
  bool        insert_val(const std::string&, AllocaInst*);
  BasicBlock* current_block() const { return bb_; }
};

class CodegenDriver {
  const char* src_;
  Parser parser_;
  CodegenContext ctx_;

 public:
  CodegenDriver(const char* src, size_t len);

  void generate_code();
  void run();
};

} // Kaleidoscope

#endif  // CODEGEN_H