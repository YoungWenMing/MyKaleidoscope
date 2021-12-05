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
  IRBuilder<>* OldBuilder = nullptr;

  std::vector<ContextScope*> scope_stack_;
  ContextScope* current_scope_;

  // for code optimization
  std::unique_ptr<legacy::FunctionPassManager> TheFPM;
  ExitOnError ExitOnErr;
  std::unique_ptr<KaleidoscopeJIT> TheJIT;

  // for function code regeneration
  std::map<std::string, std::unique_ptr<Prototype>> FunctionProtos;

  std::string SourceName;

  Type* llvm_type_map_[Token::TOKEN_TYPES_NUM];

  bool HasCodegenError = false;

  inline void InitializeMainFunction();
  inline void InitializeMainScope();
  inline void DeinitializeMainScope();
  inline void InitLLVMTypeMap();

 public:
  CodegenContext(const char* src_name);
  ~CodegenContext();

  LLVMContext& get_llvmcontext() { return *TheContext; }

  IRBuilder<>& get_irbuilder() { return *Builder; }

  Module* get_moduleptr() { return TheModule.get(); }

  ContextScope& get_current_scope() { return *current_scope_; }

  void InitializeModuleAndPassManager();

  void doOptimization(Function& func);

  void JITCompileToplevel(std::string func_name);

  Function* get_function(std::string name);
  Function* get_main_function() { return MainFunction.get(); }

  void add_protos(std::unique_ptr<Prototype> proto);
  void add_module();

  inline AllocaInst* find_val(const std::string& name);
  inline bool insert_val(const std::string& name, AllocaInst* val);

  inline void EnterScope();
  inline void ExitScope();

  void EnsureMainFunctionTerminate();

  AllocaInst* CreateEntryBlockAlloca(
      Function* func, const std::string& var_name);

  void AddCodegenError() { HasCodegenError = true; }
  bool HasError() { return HasCodegenError; }
  void LogError(const char* format, ...);
  Value* get_default_value(Token::Value token);
  inline Type* get_llvm_type(Token::Value token);

  inline void AddTempIRBuilder(IRBuilder<>* builder);
  inline void RecoverIRBuilder();
};

class ContextScope {
  CodegenContext& ctx_;
  std::map<const std::string, AllocaInst*> ValMap;
  friend class CodegenContext;
 public:
  ContextScope(CodegenContext& ctx) : ctx_(ctx) {
    ctx_.scope_stack_.push_back(this);
  }
  ~ContextScope() {
    ctx_.scope_stack_.pop_back();
  }

  AllocaInst* find_val(const std::string& name) const;
  bool        insert_val(const std::string&, AllocaInst*);
};

class CodegenDriver {
  // const char* src_;
  Script script_;
  Parser parser_;
  CodegenContext ctx_;

 public:
  CodegenDriver(const char* src_name, const char* src, size_t len);

  void generate_code();
  void run();
};

} // Kaleidoscope

#endif  // CODEGEN_H