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
  EnterScope(main_bb);
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

void CodegenContext::EnterScope(BasicBlock* bb) {
  ContextScope* s = new ContextScope(bb);
  scope_stack_.push_back(s);
  current_scope_ = s;
}

BasicBlock* CodegenContext::current_block() const {
  return current_scope_->current_block();
}

void CodegenContext::ExitScope() {
  scope_stack_.pop_back();
  delete current_scope_;
  current_scope_ = scope_stack_.back();
}


} // Kaleidoscope 
#endif // CODEGEN_INL_H