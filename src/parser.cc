
#include "src/parser.h"
#include "src/Codegen.h"
#include <iostream>

namespace Kaleidoscope {

Value* NumberExprAST::codegen(CodegenContext& ctx) {
  return ConstantFP::get(ctx.get_llvmcontext(), APFloat(val_));
}

Value* VariableExprAST::codegen(CodegenContext& ctx) {
  Value* val = ctx.get_valmap()[name_];
  if (!val)
      return LogErrorV("variable is not defined.");
  return val;
}

Value* BinaryExprAST::codegen(CodegenContext& ctx) {
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

Value* UnaryExprAST::codegen(CodegenContext& ctx) {
  std::string opFnName("unary");
  opFnName.append(Token::TokenName(op_));
  Function* opFn = ctx.get_function(opFnName);
  assert(opFn && " unary operation not defined.");
  
  Value* operand =val_->codegen(ctx);
  return ctx.get_irbuilder().CreateCall(opFn, {operand}, "unarytmp");
}

Value* CallExprAST::codegen(CodegenContext& ctx) {
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

Function* PrototypeAST::codegen(CodegenContext& ctx) {
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

Function* FunctionAST::codegen(CodegenContext& ctx) {
  // get prototype from CodegenContext
  // store the function in FunctionProtos
  PrototypeAST& P = *proto_; 
  ctx.add_protos(std::move(proto_));
  Function* func = ctx.get_function(P.getName());

  if (!func)
    return nullptr;   // LogError here
  
  if (!func->empty())
    return (Function*)LogErrorV("Function cannot be redefined.");
  
  BasicBlock* BB = BasicBlock::Create(ctx.get_llvmcontext(), "entry", func);
  // make BB the next insertion place
  ctx.get_irbuilder().SetInsertPoint(BB);

  // make the ValMap current context
  auto & val_map = ctx.get_valmap();
  val_map.clear();
  for(auto & arg : func->args())
    val_map[arg.getName().str()] = &arg;
  
  if (Value* retVal = body_->codegen(ctx)) {
    ctx.get_irbuilder().CreateRet(retVal);
    verifyFunction(*func);

    ctx.doOptimization(*func);

    return func;
  }
  func->eraseFromParent();
  return nullptr;
}

Value* IfExprAST::codegen(CodegenContext& ctx) {
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

  PHINode* pn =
      builder.CreatePHI(Type::getDoubleTy(ctx.get_llvmcontext()), 2, "iftmp");
  pn->addIncoming(thenV, thenBB);
  pn->addIncoming(elseV, elseBB);
  // use Phi node to represent the value after branch under SSA
  return pn;
}

Value* ForloopAST::codegen(CodegenContext& ctx) {
  // create the loop variable's initial value
  Value* start_val = start_->codegen(ctx);
  if (!start_val) return nullptr;
  LLVMContext& Lctx = ctx.get_llvmcontext();
  IRBuilder<>& builder = ctx.get_irbuilder();

  BasicBlock* preBB = builder.GetInsertBlock();
  Function* parenFn = preBB->getParent();

  BasicBlock* loopBB =
      BasicBlock::Create(Lctx, "loop", parenFn);
  
  // this command create a br label 'loop'
  builder.CreateBr(loopBB);
  builder.SetInsertPoint(loopBB);

  // this phi node is now inserted into the loopBB
  PHINode* pn = builder.CreatePHI(
      Type::getDoubleTy(Lctx), 2, var_name_.c_str());
  // add the first branch to the phi node
  pn->addIncoming(start_val, preBB);

  // cache the old value with identical name with loop variable
  Value* oldVal = ctx.get_valmap()[var_name_];
  ctx.get_valmap()[var_name_] = pn;

  if (!body_->codegen(ctx)) return nullptr;

  Value* delta;
  if (!step_) {
    delta = ConstantFP::get(Lctx, APFloat(1.0));
  } else {
    delta = step_->codegen(ctx);
    if (!delta) return nullptr;
  }

  Value* next = builder.CreateFAdd(pn, delta, "nextvar");

  Value* end = end_->codegen(ctx);
  if (!end)   return nullptr;


  Value* cond = builder.CreateFCmpOEQ(
      end, ConstantFP::get(Lctx, APFloat(1.0)), "condition");

  BasicBlock* endBB = builder.GetInsertBlock();
  BasicBlock* postBB = BasicBlock::Create(Lctx, "postloop", parenFn);

  builder.CreateCondBr(cond, loopBB, postBB);

  builder.SetInsertPoint(postBB);

  // set the end instruction as the predecessor of this phi node
  pn->addIncoming(next, endBB);

  if (oldVal)   ctx.get_valmap()[var_name_] = oldVal;
  else          ctx.get_valmap().erase(var_name_);

  return Constant::getNullValue(Type::getDoubleTy(Lctx));
}

Parser::Parser(const char* src, size_t len) :
    lexer_(src, len) {}

int Parser::getOpsPrecedence(Token::Value token) {
  auto entry = preceMap.find(token);
  if (entry != preceMap.end()) {
    return entry->second;
  }
  return Token::Precedence(token);
}

std::unique_ptr<NumberExprAST> Parser::ParseNumberExpr() {
  std::unique_ptr<NumberExprAST> result =
      std::make_unique<NumberExprAST>(lexer_.NumberVal());
  getNextToken();
  return result;
}

std::unique_ptr<ExprAST> Parser::ParseIdentifierExpr() {
  // handles simple identifiers and function calls
  std::string id = lexer_.IdentifierStr();

  getNextToken();
  if (curToken != Token::LPAREN) {
    return std::make_unique<VariableExprAST>(id);
  }

  // handle the function call cases
  getNextToken();
  std::vector<std::unique_ptr<ExprAST>> args;
  if (curToken != Token::RPAREN) {
    while (true) {
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      } else {
        // LogError
        return nullptr;
      }
      
      if (curToken == Token::RPAREN)
        break;

      if (curToken != Token::COMMA)
        return nullptr;

      getNextToken();
    }
  }
  getNextToken();
  return std::make_unique<CallExprAST>(id, std::move(args));
}

std::unique_ptr<ExprAST> Parser::ParseParenExpr() {
  getNextToken();
  auto expr = ParseExpression();

  if (curToken != Token::RPAREN)
    return LogError("[Parsing Error] Expecting a ')'.");

  getNextToken();
  return expr;
}

std::unique_ptr<ExprAST> Parser::ParsePrimary() {
  switch (curToken) {
    case Token::NUMBER:
      return ParseNumberExpr();
    case Token::IDENTIFIER:
      return ParseIdentifierExpr();
    case Token::LPAREN:
      return ParseParenExpr();
    case Token::IF:
      return ParseIfExpr();
    case Token::FOR:
      return ParseForloop();
    default:
      return LogError("[Parsing Error] unknown token when parsing an expression.");
  }
}

std::unique_ptr<ExprAST> Parser::ParseBinopRhs(
    int last_prec, std::unique_ptr<ExprAST> lhs) {
  while (true) {
    // curToken is one binary operator now
    int cur_prec = getOpsPrecedence(curToken);
    // next operator has lower precedency
    // return lhs directly
    // e.g.  1 * 4 + 3
    // last operator is '*', the current operator is '+'
    if (cur_prec <= last_prec)  return lhs;

    Token::Value cur_op = curToken;
    // eat this binary operator
    getNextToken();

    // parse the next primary expression first
    // 1 + +3 or 1 + -3 is also valid
    std::unique_ptr<ExprAST> rhs = ParseUnaryExpr();
    if (rhs == nullptr)
      return LogError("[Parsing Error] Expecting a primary expression.");

    int next_prec = getOpsPrecedence(curToken);

    // if next operator has higher precedency
    // the right will be parsed as a whole entity,
    // left combination is chosen otherwise
    if (next_prec > cur_prec) {
      rhs = ParseBinopRhs(cur_prec, std::move(rhs));
      if (!rhs) return nullptr;
    }
  
    lhs = std::make_unique<BinaryExprAST>(
        cur_op, std::move(lhs), std::move(rhs));
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
std::unique_ptr<ExprAST> Parser::ParseUnaryExpr() {
  if (!Token::IsUnaryOp(curToken))  return ParsePrimary();

  Token::Value op = curToken;
  getNextToken();
  if (auto val = ParseUnaryExpr()) {
    return std::make_unique<UnaryExprAST>(op, std::move(val));
  }
  return nullptr;
}

std::unique_ptr<ExprAST> Parser::ParseExpression() {
  std::unique_ptr<ExprAST> lhs = ParseUnaryExpr();
  if (!lhs)   return lhs;
  return ParseBinopRhs(0, std::move(lhs));
}

// prototype
//   ::= id '(' id* ')'
std::unique_ptr<PrototypeAST> Parser::ParsePrototype() {
  bool isOp = false;
  if (curToken != Token::IDENTIFIER) {
    if (curToken == Token::BINARY || curToken == Token::UNARY)
      isOp = true;
    else
      return LogErrorP("[Parsing Error] Expecting an identifier here.");
  }

  std::string FnName(lexer_.IdentifierStr());
  int precedence = -1;
  Token::Value Op = Token::ILLEGAL;
  // eat the function name idenrifier
  getNextToken();
  if (isOp) {
    // append the operator char
    Op = curToken;
    FnName.append(lexer_.IdentifierStr());
    // FnName.append(Token::TokenName(Op));
    getNextToken();
    if (FnName.front() == 'b') {
      // get the precedence for binary operator
      if (curToken != Token::NUMBER)
        return LogErrorP("[Parsing Error] Expecting a number for binary operator's precedence.");
      precedence = static_cast<int>(lexer_.NumberVal());
      setOpsPrecedence(Op, precedence);
      assert(precedence >= 0 && "Precedence can only be a non-negative integers.");
      getNextToken();
    }
  }

  if (curToken != Token::LPAREN)
    return LogErrorP("[Parsing Error] Expecting a left parenthesis '(' here.");

  // eat the left parenthesis of function def
  getNextToken();

  std::vector<std::string> args_;
  while (curToken == Token::IDENTIFIER) {
    args_.push_back(lexer_.IdentifierStr());
    getNextToken();

    // eat comma directly
    if (curToken == Token::COMMA)
      getNextToken();
  }

  if (curToken != Token::RPAREN)
    return LogErrorP("[Parsing Error] Expecting an identifier here.");
  // eat ')'
  getNextToken();
  return precedence != -1? 
            std::make_unique<PrototypeAST>(FnName, args_, precedence, Op) :
            std::make_unique<PrototypeAST>(FnName, args_);
}

std::unique_ptr<FunctionAST> Parser::ParseDefinition() {
  // eat 'def'
  getNextToken();
  // get prototype
  std::unique_ptr<PrototypeAST> proto = ParsePrototype();
  if (!proto)
    return nullptr;
  std::unique_ptr<ExprAST> body = ParseExpression();
  if (body)
    return std::make_unique<FunctionAST>(std::move(proto), std::move(body));
  return nullptr;
}

std::unique_ptr<PrototypeAST> Parser::ParseExtern() {
  // eat 'extern'
  getNextToken();
  // get prototype
  std::unique_ptr<PrototypeAST> proto = ParsePrototype();
  return proto;
}

std::unique_ptr<FunctionAST> Parser::ParseToplevelExpr() {
  std::unique_ptr<ExprAST> expr = ParseExpression();
  if (expr) {
    std::unique_ptr<PrototypeAST> proto =
        std::make_unique<PrototypeAST>(std::string("__anon_expr"), std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
  }
  return nullptr;
}

std::unique_ptr<ExprAST> Parser::ParseIfExpr() {
  // eat 'if'
  getNextToken();
  std::unique_ptr<ExprAST> cond = ParseExpression();

  if (!cond)
    return nullptr;

  if (curToken != Token::THEN)
    return LogError("[Parse Error] Expecting a 'then' keyword after 'if'");
  
  // eat 'then'
  getNextToken();
  std::unique_ptr<ExprAST> thenB = ParseExpression();
  std::unique_ptr<ExprAST> elseB = nullptr;

  if (curToken == Token::ELSE) {
    // eat 'else'
    getNextToken();
    elseB = ParseExpression();
  }

  std::unique_ptr<IfExprAST> result =
      std::make_unique<IfExprAST>(std::move(cond),
                                  std::move(thenB),
                                  std::move(elseB));
  return std::move(result);
}

std::unique_ptr<ExprAST> Parser::ParseForloop() {
  // eat 'for'
  getNextToken();

  if (curToken != Token::IDENTIFIER)
    return LogError("[Parse Error] Expecting a loop variable here.");
  
  std::string variable_name(lexer_.IdentifierStr());
  // eat variable name
  getNextToken();

  if (curToken != Token::ASSIGN)
    return LogError("[Parse Error] expecting '=' after loop variable.");
  // eat '='
  getNextToken();

  std::unique_ptr<ExprAST> start_val = ParseExpression();
  if (start_val == nullptr)
    return LogError("[Parse Error] Expecting primary after '='.");
  // eat expression after '='
  if (curToken != Token::COMMA)
    return LogError("[Parse Error] Expecting ',' as a delimiter.");
  
  getNextToken();
  std::unique_ptr<ExprAST> last_val = ParseExpression();
  if (!last_val)
    return nullptr;

  std::unique_ptr<ExprAST> step = nullptr;
  if (curToken == Token::COMMA) {
    getNextToken();
    step = ParseExpression();
    if (!step)  return nullptr;
  }
  // eat ',' or last token of an expression
  if (curToken != Token::IN)
    LogError("[Syntax Error] keyword 'in' is necessarry in a forloop.");
  
  getNextToken();
  std::unique_ptr<ExprAST> body = ParseExpression();
  if (!body)  return nullptr;
  return std::make_unique<ForloopAST>(variable_name,
      std::move(start_val), std::move(last_val), std::move(step), std::move(body));
}

void Parser::ParseToplevel(std::vector<std::unique_ptr<ExprAST>>& stmts) {
  getNextToken();
  std::unique_ptr<ExprAST> res;
  while (curToken != Token::EOS) {
    switch (curToken) {
      case Token::DEF:
        res = ParseDefinition();
        stmts.push_back(std::move(res));
        break;
      case Token::EXTERN:
        res = ParseExtern();
        stmts.push_back(std::move(res));
      case Token::SEMICOLON:
        // eat directly
        getNextToken();
        break;
      default:
        res = ParseExpression();
        stmts.push_back(std::move(res));
    }
  }
}

std::unique_ptr<ExprAST> LogError(const char* info) {
  std::cerr << info << std::endl;
  return nullptr;
}

Value* LogErrorV(const char* info) {
  LogError(info);
  return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char* info) {
  std::cerr << info << std::endl;
  return nullptr;
}

} // namespace Kaleidoscope
