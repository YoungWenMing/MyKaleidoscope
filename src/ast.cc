#include "src/ast.h"

#include "src/Codegen-inl.h"
#include "src/util.h"

namespace Kaleidoscope {

using llvm::getLoadStorePointerOperand;

Value* SmiLiteral::codegen(CodegenContext& ctx) {
  return ConstantInt::get(ctx.get_llvmcontext(), APInt(32, val_));
}

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
  Value* left = lhs_->codegen(ctx), *right = rhs_->codegen(ctx);

  if (left == nullptr || right == nullptr) {
    ctx.LogError("Got invalid value for binary operation.");
    return nullptr;
  }

  IRBuilder<>& irbuilder = ctx.get_irbuilder();
  // support float and int
  auto rType = right->getType();
  bool r_is_double = rType->isDoubleTy();
  bool is_same_ty = rType == left->getType();

  if (!is_same_ty) {
    Type* doubleTy = Type::getDoubleTy(ctx.get_llvmcontext());
    if (r_is_double) {
      auto op = CastInst::getCastOpcode(left, true, doubleTy, true);
      left = irbuilder.CreateCast(op, left, doubleTy, "castdb");
    } else {
      auto op = CastInst::getCastOpcode(right, true, doubleTy, true);
      right = irbuilder.CreateCast(op, right, doubleTy, "castdb");
    }
  }


  switch(op_) {
    case Token::ADD:
      // create float add
      return r_is_double?
                irbuilder.CreateFAdd(left, right, "addtmp") :
                irbuilder.CreateAdd(left, right, "addtmp");
    case Token::SUB:
      return r_is_double?
                irbuilder.CreateFSub(left, right, "subtmp") :
                irbuilder.CreateSub(left, right, "subtmp");
    case Token::MUL:
      return r_is_double?
                irbuilder.CreateFMul(left, right, "multmp") :
                irbuilder.CreateMul(left, right, "multmp");
    case Token::DIV:
      return r_is_double?
                irbuilder.CreateFDiv(left, right, "divtmp"):
                irbuilder.CreateSDiv(left, right, "divtmp");
    case Token::LT:{
      Value *val = r_is_double?
                irbuilder.CreateFCmpULT(left, right, "lttmp") :
                irbuilder.CreateICmpULT(left, right, "lttmp");
      return irbuilder.CreateUIToFP(
          val, Type::getDoubleTy(ctx.get_llvmcontext()), "booltmp");
    }
    case Token::GT:{
      Value *val = r_is_double?
                irbuilder.CreateFCmpUGT(left, right, "gttmp") :
                irbuilder.CreateICmpUGT(left, right, "gttmp");
      return irbuilder.CreateUIToFP(
          val, Type::getDoubleTy(ctx.get_llvmcontext()), "booltmp");
    }
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

  // TODO(yang): cover different types, including string\boolean
  Type* declTy = ctx.get_llvm_type(decl_type_);
  AllocaInst* allo = new AllocaInst(declTy, 0, name_.c_str(), curBB);

  ContextScope& cur_scope = ctx.get_current_scope();
  if (!cur_scope.insert_val(name_, allo)) {
    ctx.LogError("Redeclaration of variable '%s'.\n", name_.c_str());
    return nullptr;
  }

  Value* init_val;
  // build store instruction.
  if (init_val_ != nullptr) {
    init_val = init_val_->codegen(ctx);
    if (init_val != nullptr && init_val->getType() != declTy) {
      if (decl_type_ == Token::DOUBLE) {
        // need casting from int to double
        Type* doubleTy = Type::getDoubleTy(ctx.get_llvmcontext());
        auto op = CastInst::getCastOpcode(init_val, true, doubleTy, true);
        init_val = builder.CreateCast(op, init_val, doubleTy, "castdb");
      } else {
        ctx.LogError("Incompatible type of variable %s.", name_.c_str());
        return nullptr;
      }
    } else {
      return nullptr;
    }
  } else {
    init_val = ctx.get_default_value(decl_type_);
  }
  builder.CreateStore(init_val, allo);

  VariableDeclaration *next = next_.get();
  Value* ret = static_cast<Value*>(allo);

  if (next != nullptr)    next->codegen(ctx);

  return ret;
}

Value* CallExpression::codegen(CodegenContext& ctx) {
  Function* calleeFn = ctx.get_function(callee_);
  if (!calleeFn) {
    ctx.LogError("Call an undeclared function: %s.\n", callee_.c_str());
    return nullptr;
  }
  
  if (calleeFn->arg_size() != args_.size()) {
    ctx.LogError("Incorrect quantity of arguments for function: %s\n", callee_.c_str());
    return nullptr;
  }
  
  std::vector<Value*> ArgsV;
  for (int i = 0; i < args_.size(); ++i) {
    Value* a = args_[i]->codegen(ctx);
    if (a == nullptr) return nullptr;
    ArgsV.push_back(a);
  }

  return ctx.get_irbuilder().CreateCall(calleeFn, ArgsV, "calltemp");
}

Value* CountOperation::codegen(CodegenContext& ctx) {
  IRBuilder<> &builder = ctx.get_irbuilder();
  BasicBlock *curBB = builder.GetInsertBlock();

  Value* right_val = expr_->codegen(ctx);
  Value* left_val = getLoadStorePointerOperand(right_val);
  if (left_val == nullptr) {
    ctx.LogError("Illegal target of Count Operation -- it must be a left value.");
    return nullptr;
  }

  Type *ty = right_val->getType();
  CHECK(ty->isIntegerTy() || ty->isDoubleTy());
  int delta = tok_ == Token::INC ? 1 : -1;
  Value *delta_val = nullptr;
  if (ty->isIntegerTy())
    delta_val = ConstantInt::get(ctx.get_llvmcontext(), APInt(32, delta));
  else
    delta_val = ConstantFP::get(ctx.get_llvmcontext(), APFloat((double)delta));

  Value *post_val = ty->isIntegerTy() ?
                      builder.CreateAdd(right_val, delta_val, "inc_op") :
                      builder.CreateFAdd(right_val, delta_val, "inc_op");
  builder.CreateStore(post_val, left_val);
  return is_postfix_? right_val : post_val;
}

Function* Prototype::codegen(CodegenContext& ctx) {
  // all arguments are double type
  std::vector<Type*> types(arg_types_.size(), nullptr);
  for (size_t i = 0; i < arg_types_.size(); ++i)
    types[i] = ctx.get_llvm_type(arg_types_[i]);
  
  // both return value and all arguments are of double type
  FunctionType* funcTy = FunctionType::get(ctx.get_llvm_type(ret_type_), types, false);
  
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
  // generate code for function declaration
  // Step 1: get arguments types and function type
  // Step 2: create Function object
  Function* func_target = proto_->codegen(ctx);
  // Step 3: create basic block
  BasicBlock* funcBB = BasicBlock::Create(ctx.get_llvmcontext(),
                                          "func_entry",
                                          func_target);
  IRBuilder<> tbuilder(funcBB, funcBB->begin());
  ContextScope scope(ctx);
  ctx.AddTempIRBuilder(&tbuilder);
  // Step 4: initialize arguments
  Function::arg_iterator actualArg = func_target->arg_begin();
  for (llvm::Argument &arg : func_target->args()) {
    std::string arg_name = arg.getName().str();
    AllocaInst* allo = new AllocaInst(arg.getType(),
                                      0, arg_name.c_str(), funcBB);
    if (!scope.insert_val(arg_name, allo)) {
      PrintErrorF("Redefinition of argument %s in function \"%s\"\n",
                  arg_name.c_str(), proto_->getName().c_str());
    }
    tbuilder.CreateStore(&arg, allo);
  }
  // Step 5: generate code for body
  auto body_val = body_->codegen(ctx);
  if (body_val == nullptr) {
    PrintErrorF("Invalid body for function %s\n", proto_->getName().c_str());
    return nullptr;
  }
  // Step 6: create return instruction
  if (tbuilder.GetInsertBlock()->getTerminator() == nullptr) {
    if (func_target->getReturnType()->isVoidTy()) {
      tbuilder.CreateRetVoid();
    } else {
      ctx.LogError("Function %s must has a return statement.",
                   proto_->getName().c_str());
      return nullptr;
    }
  }

  // Here is an assumption that only the last statement can be a return statement.
  if (body_val->getType() != func_target->getReturnType()) {
    ctx.LogError("Function %s returns an incompatible type.\n",
                 proto_->getName().c_str());
    return nullptr;
  }
  ctx.RecoverIRBuilder();
  return func_target;
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
      BasicBlock::Create(ctx.get_llvmcontext(), "merge");
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

  if (condition_ == nullptr) {
    builder.CreateBr(loopBB);
  } else {
    Value* cond_val = condition_->codegen(ctx);
    Value* cond = builder.CreateFCmpOEQ(
          cond_val, ConstantFP::get(Lctx, APFloat(1.0)));
    builder.CreateCondBr(cond, loopBB, exitBB);
  }

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
  if (expression_ == nullptr)
    return ctx.get_irbuilder().CreateRetVoid();
  Value* ret = expression_->codegen(ctx);
  if (ret == nullptr) {
    PrintErrorF("** Invalid Return Statement.**\n");
    return nullptr;
  }
  ctx.get_irbuilder().CreateRet(ret);
  return ret;
}

Value* Block::codegen(CodegenContext& ctx) {
  Value* last = nullptr;
  for (int i = 0; i < statements_->size(); ++i) {
    last = statements_->at(i)->codegen(ctx);
  }
  return last;
}


#undef RECORD_ERR_AND_RETURN
#undef RETURN_NULL_FOR_ERR

} // Kaleidoscope 
