#ifndef CODEGEN_INL_H
#define CODEGEN_INL_H

#include "src/Codegen.h"

namespace Kaleidoscope {

void CodegenContext::InitializeMainFunction() {
  std::vector<Type*> args;
  FunctionType *ftype =
      FunctionType::get(Type::getVoidTy(*TheContext), args, false);
  MainFunction = std::unique_ptr<Function>(Function::Create(
      ftype, GlobalValue::InternalLinkage, "main", TheModule.get()));
}

void CodegenContext::InitializeMainScope() {
  BasicBlock* main_bb = BasicBlock::Create(*TheContext, "mainentry", MainFunction.get());
  Builder->SetInsertPoint(main_bb);
  EnterScope();
}

void CodegenContext::InitLLVMTypeMap() {
  llvm_type_map_[Token::INT]     =  Type::getInt32Ty(get_llvmcontext());
  llvm_type_map_[Token::DOUBLE]  =  Type::getDoubleTy(get_llvmcontext());
  llvm_type_map_[Token::VOID]    =  Type::getVoidTy(get_llvmcontext());
}

void CodegenContext::DeinitializeMainScope() {
  ExitScope();
}

AllocaInst* CodegenContext::find_val(const std::string& name) {
  for (int i = scope_stack_.size() - 1; i >= 0; --i) {
    AllocaInst* t = scope_stack_[i]->find_val(name);
    if (t != nullptr)   return t;
  }
  return nullptr;
}

bool CodegenContext::insert_val(const std::string& name, AllocaInst* val) {
  return current_scope_->insert_val(name, val);
}

void CodegenContext::EnterScope() {
  ContextScope* s = new ContextScope(*this);
  current_scope_ = s;
}

void CodegenContext::ExitScope() {
  delete current_scope_;
}

Type* CodegenContext::get_llvm_type(Token::Value token) {
  DCHECK(Token::IsType(token));
  return llvm_type_map_[token];
}

void CodegenContext::AddTempIRBuilder(IRBuilder<>* builder) {
  // TODO: consider recursive calling to this function.
  OldBuilder = Builder.release();
  Builder = std::unique_ptr<IRBuilder<>>(builder);
}

void CodegenContext::RecoverIRBuilder() {
  if (OldBuilder != nullptr) {
    Builder.release();
    Builder = std::unique_ptr<IRBuilder<>>(OldBuilder);
    OldBuilder = nullptr;
  }
}
} // Kaleidoscope 
#endif // CODEGEN_INL_H