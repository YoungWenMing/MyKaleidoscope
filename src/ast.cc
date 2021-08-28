#include "src/ast.h"

#include "src/Codegen-inl.h"
#include "src/util.h"

namespace Kaleidoscope {

Value* NumberLiteral::codegen(CodegenContext& ctx) {
  return ConstantFP::get(ctx.get_llvmcontext(), APFloat(val_));
}

Value* ExpressionStatement::codegen(CodegenContext& ctx) {
  return expr_->codegen(ctx);
}

Value* Identifier::codegen(CodegenContext& ctx) {
  AllocaInst* allo = ctx.find_val(name_);
  if (allo == nullptr) {
    PrintErrorF("Use identifier %s before declaration.\n", name_.c_str());
    return nullptr;
  }
  return ctx.get_irbuilder().CreateLoad(
      allo->getAllocatedType(), allo, name_.c_str());
}

Value* BinaryExpression::codegen(CodegenContext& ctx) {
  // if (op_ == Token::ASSIGN) {
  //   assert(lhs_->getType() == AstNode::kIdentifier);
  //   Identifier* vlhs = static_cast<Identifier*>(lhs_.get());

  //   AllocaInst* allo = ctx.find_val(vlhs->var_name());
  //   if (allo == nullptr)
  //     return LogErrorV("Unknown variable name.");

  //   Value* r = rhs_->codegen(ctx);
  //   if (r == nullptr) return nullptr;

  //   ctx.get_irbuilder().CreateStore(r, allo);
  //   return r;
  // }
  Value* left = lhs_->codegen(ctx), *right = rhs_->codegen(ctx);
  IRBuilder<>& irbuilder = ctx.get_irbuilder();
  switch(op_) {
    case Token::ADD:
      // create float add
      return irbuilder.CreateFAdd(left, right, "addtmp");
    case Token::SUB:
      return irbuilder.CreateFSub(left, right, "subtmp");
    case Token::MUL:
      return irbuilder.CreateFMul(left, right, "multmp");
    case Token::DIV:
      return irbuilder.CreateFDiv(left, right, "divtmp");
    case Token::LT:
      left = irbuilder.CreateFCmpULT(left, right, "lttmp");
      return irbuilder.CreateUIToFP(
          left, Type::getDoubleTy(ctx.get_llvmcontext()), "booltmp");
    case Token::GT:
      left = irbuilder.CreateFCmpUGT(left, right, "gttmp");
      return irbuilder.CreateUIToFP(
          left, Type::getDoubleTy(ctx.get_llvmcontext()), "booltmp");
    default:
      break;
  }
  // consider user-defined operators
  std::string opFnName("binary");
  opFnName.append(Token::TokenName(op_));

  Function* opFn = ctx.get_function(opFnName);
  assert(opFn && " binary operation definition not found.");
  return ctx.get_irbuilder().CreateCall(opFn, {left, right}, "optmp");
}

Value* UnaryExpression::codegen(CodegenContext& ctx) {
  std::string opFnName("unary");
  opFnName.append(Token::TokenName(op_));
  Function* opFn = ctx.get_function(opFnName);
  assert(opFn && " unary operation not defined.");
  
  Value* operand =val_->codegen(ctx);
  return ctx.get_irbuilder().CreateCall(opFn, {operand}, "unarytmp");
}

Value* UnaryOperation::codegen(CodegenContext& ctx) {
  Value* target = operand_->codegen(ctx);
  if (target == nullptr)
    return nullptr;

  // we can only get SUB or ADD here.
  if (op_ == Token::SUB) {
    Value* zero = ConstantFP::get(ctx.get_llvmcontext(), APFloat(0.0));
    return ctx.get_irbuilder().CreateFSub(zero, target, "unatmp");
  }
  return target;
}

Value* VariableDeclaration::codegen(CodegenContext& ctx) {
  IRBuilder<>& builder = ctx.get_irbuilder();

  BasicBlock* curBB = builder.GetInsertBlock();
  Function* parenFn = curBB->getParent();

  AllocaInst* allo = new AllocaInst(
        Type::getDoubleTy(ctx.get_llvmcontext()),
        0, name_.c_str(), curBB);

  ContextScope& cur_scope = ctx.get_current_scope();
  if (!cur_scope.insert_val(name_, allo))   return nullptr;

  // build store instruction.
  if (init_expr_ != nullptr) {
    init_expr_->codegen(ctx);
  } else {
    // store default initial value 0.0
    builder.CreateStore(
        ConstantFP::get(ctx.get_llvmcontext(), APFloat(0.0)), allo);
  }

  VariableDeclaration *next = next_.get();
  Value* ret = static_cast<Value*>(allo);

  if (next != nullptr)    next->codegen(ctx);

  return ret;
}

Value* CallExpression::codegen(CodegenContext& ctx) {
  Function* calleeFn = ctx.get_function(callee_);
  if (!calleeFn)
    return LogErrorV("Callee function is not defined."); // LogError here
  
  if (calleeFn->arg_size() != args_.size())
    return LogErrorV("Incorrect quantity of arguments for this function call");  // LogError here
  
  std::vector<Value*> ArgsV;
  for (int i = 0; i < args_.size(); ++i) {
    ArgsV.push_back(args_[i]->codegen(ctx));
    if (!ArgsV.back())
      return nullptr;
  }

  return ctx.get_irbuilder().CreateCall(calleeFn, ArgsV, "calltemp");
}

Function* Prototype::codegen(CodegenContext& ctx) {
  // all arguments are double type
  std::vector<Type*> doubles(args_.size(),
                             Type::getDoubleTy(ctx.get_llvmcontext()));
  
  // both return value and all arguments are of double type
  FunctionType* funcTy = FunctionType::get(
      Type::getDoubleTy(ctx.get_llvmcontext()), doubles, false);
  
  // with function type, name and the module we are talking about
  // we create a function in thie module, so that we can find it.
  // Note that the module has a symbol table
  Function* F = Function::Create(
      funcTy, GlobalValue::ExternalLinkage, name_, ctx.get_moduleptr());
  
  unsigned idx = 0;
  for (auto &arg : F->args())
    arg.setName(args_[idx++]);

  return F;
}

Function* FunctionDeclaration::codegen(CodegenContext& ctx) {
  // get prototype from CodegenContext
  // store the function in FunctionProtos
  Prototype& P = *proto_; 
  ctx.add_protos(std::move(proto_));
  Function* func = ctx.get_function(P.getName());

  if (!func)
    return nullptr;   // LogError here
  
  if (!func->empty())
    return (Function*)LogErrorV("Function cannot be redefined.");
  
  BasicBlock* BB = BasicBlock::Create(ctx.get_llvmcontext(), "entry", func);
  // make BB the next insertion place
  IRBuilder<>& builder = ctx.get_irbuilder();
  builder.SetInsertPoint(BB);

  // enter a new scope
  ContextScope scope(ctx);
  for(auto & arg : func->args()) {
    std::string arg_name = arg.getName().str();
    AllocaInst* allo = ctx.CreateEntryBlockAlloca(func, arg_name);
    builder.CreateStore(&arg, allo);
    ctx.insert_val(arg_name, allo);
  }
  
  if (Value* retVal = body_->codegen(ctx)) {
    ctx.get_irbuilder().CreateRet(retVal);
    verifyFunction(*func);

    ctx.doOptimization(*func);

    return func;
  }
  func->eraseFromParent();
  return nullptr;
}

Value* IfStatement::codegen(CodegenContext& ctx) {
  Value* cond = condition_->codegen(ctx);

  if (!cond)  return nullptr;

  IRBuilder<>& builder = ctx.get_irbuilder();

  cond = builder.CreateFCmpONE(
      cond, ConstantFP::get(ctx.get_llvmcontext(), APFloat(0.0)), "ifcond");

  Function* parenFn = builder.GetInsertBlock()->getParent();

  BasicBlock* thenBB =
      BasicBlock::Create(ctx.get_llvmcontext(), "then", parenFn);
  BasicBlock* elseBB =
      BasicBlock::Create(ctx.get_llvmcontext(), "else");
  BasicBlock* mergeBB =
      BasicBlock::Create(ctx.get_llvmcontext(), "ifcont");
  // 1. create a condition branch selection in the current BB
  builder.CreateCondBr(cond, thenBB, elseBB);
  // 2. set insertPoint to thenBB which is already added to current funtion 
  builder.SetInsertPoint(thenBB);

  Value* thenV = thenB_->codegen(ctx);

  // TODO: figure out that why the llvm context and builder
  // do not need to be reset to clear the BBs
  if (!thenV) return nullptr;

  // every basic block must be terminated with a control flow instruction such as ret or branch
  builder.CreateBr(mergeBB);
  // the BB builder is inserting to can be changed after codegen because of potential recursion
  thenBB = builder.GetInsertBlock();
  // Note that elseBB is not added to parenFn during creation
  parenFn->getBasicBlockList().push_back(elseBB);
  builder.SetInsertPoint(elseBB);
  Value* elseV = elseB_->codegen(ctx);

  if (!elseV) return nullptr;
  builder.CreateBr(mergeBB);
  elseBB = builder.GetInsertBlock();

  parenFn->getBasicBlockList().push_back(mergeBB);
  builder.SetInsertPoint(mergeBB); // phi node is eventually added to mergeBB

  return mergeBB;
}

Value* ForLoopStatement::codegen(CodegenContext& ctx) {
  LLVMContext& Lctx = ctx.get_llvmcontext();
  IRBuilder<>& builder = ctx.get_irbuilder();

  BasicBlock* preBB = builder.GetInsertBlock();
  Function* parenFn = preBB->getParent();
  BasicBlock* initBB = BasicBlock::Create(ctx.get_llvmcontext(), "init", parenFn);
  BasicBlock* exitBB = BasicBlock::Create(ctx.get_llvmcontext(), "exit");
  BasicBlock* loopBB = BasicBlock::Create(ctx.get_llvmcontext(), "loop");

  builder.CreateBr(initBB);
  ContextScope scope(ctx);
  builder.SetInsertPoint(initBB);

  if (init_ != nullptr) {
    init_->codegen(ctx);
  }

  if (condition_ == nullptr) {
    builder.CreateBr(loopBB);
  } else {
    Value* cond_val = condition_->codegen(ctx);
    Value* cond = builder.CreateFCmpOEQ(
        cond_val, ConstantFP::get(Lctx, APFloat(1.0)), "condition");
    builder.CreateCondBr(cond, loopBB, exitBB);
  }

  parenFn->getBasicBlockList().push_back(loopBB);
  builder.SetInsertPoint(loopBB);
  body_->codegen(ctx);
  if (next_ != nullptr)   next_->codegen(ctx);

  Value* cond_val = condition_->codegen(ctx);
  Value* cond = builder.CreateFCmpOEQ(
        cond_val, ConstantFP::get(Lctx, APFloat(1.0)));
  builder.CreateCondBr(cond, loopBB, exitBB);

  parenFn->getBasicBlockList().push_back(exitBB);
  builder.SetInsertPoint(exitBB);
  return exitBB;
}

Value* Assignment::codegen(CodegenContext& ctx) {
  if (target_->getType() != kIdentifier) {
    ctx.LogError("Only identifiers are assignable yet.");
    return nullptr;
  }

  const std::string& var_name = static_cast<Identifier*>(target_.get())->var_name();
  AllocaInst* allo =
      ctx.find_val(var_name);
  if (allo == nullptr) {
    ctx.LogError("Assignment before an identifier was declared.");
    return nullptr;
  }
  Value* val = value_->codegen(ctx);
  if (val == nullptr) {
    ctx.LogError("Invalid value in assignment for variable %s.", var_name.c_str());
    return nullptr;
  }

  return ctx.get_irbuilder().CreateStore(val, allo);
}

Value* EmptyStatement::codegen(CodegenContext& ctx) {
  return nullptr;
}

Value* ReturnStatement::codegen(CodegenContext& ctx) {
  return nullptr;
}

Value* Block::codegen(CodegenContext& ctx) {
  Value* last = nullptr;
  for (int i = 0; i < statements_->size(); ++i) {
    last = statements_->at(i)->codegen(ctx);
  }
  return last;
}

} // Kaleidoscope 
