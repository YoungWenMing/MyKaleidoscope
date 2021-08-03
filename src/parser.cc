
#include "src/parser.h"
#include "src/Codegen.h"
#include <iostream>

namespace Kaleidoscope {


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
    case Token::VAR:
      return ParseVariableDecl();
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
    auto entry = unarySet.find(op);
    if (entry != unarySet.end())
      return std::make_unique<UnaryExprAST>(op, std::move(val));
    else
      // only handle intrinsic unary operators
      return BuildUnaryExpr(std::move(val), op);
  }
  return nullptr;
}

std::unique_ptr<ExprAST> Parser::BuildUnaryExpr(
    std::unique_ptr<ExprAST> expr, Token::Value val) {
  if (expr->getType() == ExprAST::kNumberExpr) {
    NumberExprAST* num_expr =
        static_cast<NumberExprAST*>(expr.get());
    if (val == Token::SUB)
      return std::make_unique<NumberExprAST>(-num_expr->value());
    if (val == Token::ADD)
      return std::move(expr);
    if (val == Token::NOT) {
      double nv = num_expr->value();
      return std::make_unique<NumberExprAST>(nv == 0 ? 1 : 0);
    }
  }
  if (val == Token::NOT) {
    auto zero = std::make_unique<NumberExprAST>(0);
    auto one = std::make_unique<NumberExprAST>(1);
    return std::make_unique<IfExprAST>(
              std::move(expr), std::move(zero), std::move(one));
  }
  return std::make_unique<UnaryOperation>(val, std::move(expr));
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
    } else {
      unarySet.insert(Op);
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

std::unique_ptr<ExprAST> Parser::ParseVariableDecl() {
  // eat keyword 'var'
  getNextToken();

  typedef std::pair<std::string, std::unique_ptr<ExprAST>> SEpair;
  std::vector<SEpair> list;

  while (true) {
    if (curToken != Token::IDENTIFIER)
      return LogError("Expecting an identifier after keyword 'var'.");
    SEpair cur;
    cur.first = lexer_.IdentifierStr();
    cur.second = nullptr;
    // eat variable name
    getNextToken();
    if (curToken == Token::ASSIGN) {
      getNextToken();  // eat '='
      auto initializer = ParseUnaryExpr();
      if (!initializer)
        return LogError("Expecting initializer after '='.");
      cur.second = std::move(initializer);
    }
    list.push_back(std::move(cur));

    if (curToken == Token::COMMA)
      getNextToken();
    else if (curToken == Token::SEMICOLON)
      break;
    else
      return LogError("Expecting ',' or ';' in variable declaration.");
  }
  return std::make_unique<VariableDeclaration>(std::move(list));
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
