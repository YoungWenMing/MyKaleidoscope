
#include "src/parser.h"
#include <iostream>

namespace Kaleidoscope {

Value* NumberExprAST::codegen() {
  return ConstantFP::get(TheContext, APFloat(val_));
}

Value* VariableExprAST::codegen() {
  Value* val = ValMap[name_];
  // if val is nullptr, logerror here
  return val;
}

Value* BinaryExprAST::codegen() {
  Value* left = lhs_->codegen(), *right = rhs_->codegen();
  switch(op_) {
    case '+':
      // create float add
      return Builder.CreateFAdd(left, right, "addtmp");
    case '-':
      return Builder.CreateFSub(left, right, "subtmp");
    case '*':
      return Builder.CreateFMul(left, right, "multmp");
    case '/':
      return Builder.CreateFDiv(left, right, "divtmp");
    default:
      // TODO: logerror
      return nullptr;
  }
}

Parser::Parser(const char* src) :
    lexer_(src),
    cur_token(0) {}

int Parser::BinopPrecedency::get_precedency(char op) {
  switch (op) {
    case '<':
    case '>':
      return 1;
    case '-':
    case '+':
      return 2;
    case '*':
    case '/':
      return 3;
    default:
      return -1;
  }
}

std::unique_ptr<NumberExprAST> Parser::ParseNumberExpr() {
  std::unique_ptr<NumberExprAST> result =
      std::make_unique<NumberExprAST>(lexer_.number_val());
  cur_token = lexer_.next_token();
  return result;
}

std::unique_ptr<ExprAST> Parser::ParseIdentifierExpr() {
  // handles simple identifiers and function calls
  std::string id = lexer_.identifier_str();

  get_next_token();
  if (cur_token != '(') {
    return std::make_unique<VariableExprAST>(id);
  }

  // handle the function call cases
  get_next_token();
  std::vector<std::unique_ptr<ExprAST>> args;
  if (cur_token != ')') {
    while (true) {
      if (auto arg = ParseExpression()) {
        args.push_back(std::move(arg));
      } else {
        // LogError
        return nullptr;
      }
      
      if (cur_token == ')')
        break;

      if (cur_token != ',') {
        // LogError
        return nullptr;
      }
      get_next_token();
    }
  }
  get_next_token();
  return std::make_unique<CallExprAST>(id, std::move(args));
}

std::unique_ptr<ExprAST> Parser::ParseParenExpr() {
  get_next_token();  // eat '('
  auto expr = ParseExpression();

  if (cur_token != ')')
    return LogError("[Parsing Error] Expecting a ')'.");

  get_next_token();  // eat ')'
  return expr;
}

std::unique_ptr<ExprAST> Parser::ParsePrimary() {
  switch (cur_token) {
    case Lexer::token_number:
      return ParseNumberExpr();
    case Lexer::token_identifier:
      return ParseIdentifierExpr();
    case '(':
      return ParseParenExpr();
    default:
      return LogError("[Parsing Error] unknown token when parsing an expression.");
  }
}

std::unique_ptr<ExprAST> Parser::ParseBinopRhs(
    int last_prec, std::unique_ptr<ExprAST> lhs) {
  while (true) {
    // cur_token is one binary operator now
    int cur_prec = BinopPrecedency::get_precedency(cur_token);
    // next operator has lower precedency
    // return lhs directly
    // e.g.  1 * 4 + 3
    // last operator is '*', the current operator is '+'
    if (cur_prec <= last_prec)  return lhs;

    char cur_op = cur_token;
    // eat this binary operator
    get_next_token();

    // parse the next primary expression first
    std::unique_ptr<ExprAST> rhs = ParsePrimary();
    if (rhs == nullptr)
      return LogError("[Parsing Error] Expecting a primary expression.");

    int next_prec = BinopPrecedency::get_precedency(cur_token);

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

std::unique_ptr<ExprAST> Parser::ParseExpression() {
  std::unique_ptr<ExprAST> lhs = ParsePrimary();
  if (!lhs)   return lhs;
  return ParseBinopRhs(0, std::move(lhs));
}

// prototype
//   ::= id '(' id* ')'
std::unique_ptr<PrototypeAST> Parser::ParsePrototype() {
  if (cur_token != Lexer::token_identifier)
    return LogErrorP("[Parsing Error] Expecting an identifier here.");

  std::string FnName(lexer_.identifier_str());
  get_next_token();

  if (cur_token != '(')
    return LogErrorP("[Parsing Error] Expecting a left parenthesis '(' here.");
  return nullptr;

  std::vector<std::string> args_;
  while (cur_token == Lexer::token_identifier)
    args_.push_back(lexer_.identifier_str());

  if (cur_token != ')')
    return LogErrorP("[Parsing Error] Expecting an identifier here.");
  // eat ')'
  get_next_token();
  return std::make_unique<PrototypeAST>(FnName, args_);
}


std::unique_ptr<FunctionAST> Parser::ParseDefinition() {
  // eat 'def'
  get_next_token();
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
  get_next_token();
  // get prototype
  std::unique_ptr<PrototypeAST> proto = ParsePrototype();
  return proto;
}

std::unique_ptr<FunctionAST> Parser::ParseToplevelExpr() {
  std::unique_ptr<ExprAST> expr = ParseExpression();
  if (expr) {
    std::unique_ptr<PrototypeAST> proto =
        std::make_unique<PrototypeAST>(std::string(""), std::vector<std::string>());
    return std::make_unique<FunctionAST>(std::move(proto), std::move(expr));
  }
  return nullptr;
}

void Parser::ParseToplevel(std::vector<std::unique_ptr<ExprAST>>& stmts) {
  get_next_token();
  std::unique_ptr<ExprAST> res;
  while (cur_token != Lexer::token_eof) {
    switch (cur_token) {
      case Lexer::token_def:
        res = ParseDefinition();
        stmts.push_back(std::move(res));
        break;
      case Lexer::token_extern:
        res = ParseExtern();
        stmts.push_back(std::move(res));
      case ';':
        // eat directly
        get_next_token();
        break;
      default:
        res = ParseExpression();
        stmts.push_back(std::move(res));
    }
  }
}

std::unique_ptr<ExprAST> Parser::LogError(const char* info) {
  std::cerr << info << std::endl;
  return nullptr;
}

std::unique_ptr<PrototypeAST> Parser::LogErrorP(const char* info) {
  std::cerr << info << std::endl;
  return nullptr;
}

} // namespace Kaleidoscope
