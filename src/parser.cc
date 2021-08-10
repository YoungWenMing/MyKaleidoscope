
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

std::unique_ptr<NumberLiteral> Parser::ParseNumberLiteral() {
  std::unique_ptr<NumberLiteral> result =
      std::make_unique<NumberLiteral>(lexer_.NumberVal());
  getNextToken();
  return result;
}

std::unique_ptr<Expression> Parser::ParseIdentifierExpr() {
  // handles simple identifiers and function calls
  std::string id = lexer_.IdentifierStr();

  getNextToken();
  if (curToken != Token::LPAREN) {
    return std::make_unique<Identifier>(id);
  }

  // handle the function call cases
  getNextToken();
  std::vector<std::unique_ptr<Expression>> args;
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
  return std::make_unique<CallExpression>(id, std::move(args));
}

std::unique_ptr<Expression> Parser::ParseParenExpr() {
  getNextToken();
  auto expr = ParseExpression();

  if (curToken != Token::RPAREN) {
    LogError("[Parsing Error] Expecting a ')'.");
    return nullptr;
  }

  getNextToken();
  return expr;
}

std::unique_ptr<Expression> Parser::ParsePrimary() {
  switch (curToken) {
    case Token::NUMBER:
      return ParseNumberLiteral();
    case Token::IDENTIFIER:
      return ParseIdentifierExpr();
    case Token::LPAREN:
      return ParseParenExpr();
    default:
      LogError("[Parsing Error] unknown token when parsing an expression.");
      return nullptr;
  }
}

std::unique_ptr<Assignment>
    Parser::ParseAssignment(std::unique_ptr<Expression> lhs) {
  // eat assign operator
  if (lhs->getType() != AstNode::kIdentifier) {
    LogError("Only variables is assignable.");
    return nullptr;
  }
  getNextToken();
  auto value = ParseExpression();
  return std::make_unique<Assignment>(std::move(lhs), std::move(value));
}

std::unique_ptr<Expression> Parser::ParseBinopRhs(
    int last_prec, std::unique_ptr<Expression> lhs) {
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
    std::unique_ptr<Expression> rhs = ParseUnaryExpr();
    if (rhs == nullptr) {
      LogError("[Parsing Error] Expecting a primary expression.");
      return nullptr;
    }

    int next_prec = getOpsPrecedence(curToken);

    // if next operator has higher precedency
    // the right will be parsed as a whole entity,
    // left combination is chosen otherwise
    if (next_prec > cur_prec) {
      rhs = ParseBinopRhs(cur_prec, std::move(rhs));
      if (!rhs) return nullptr;
    }
  
    lhs = std::make_unique<BinaryExpression>(
        cur_op, std::move(lhs), std::move(rhs));
  }
}

/// unary
///   ::= primary
///   ::= '!' unary
std::unique_ptr<Expression> Parser::ParseUnaryExpr() {
  if (!Token::IsUnaryOp(curToken))  return ParsePrimary();

  Token::Value op = curToken;
  getNextToken();
  if (auto val = ParseUnaryExpr()) {
    auto entry = unarySet.find(op);
    if (entry != unarySet.end())
      return std::make_unique<UnaryExpression>(op, std::move(val));
    else
      // only handle intrinsic unary operators
      return BuildUnaryExpr(std::move(val), op);
  }
  return nullptr;
}

std::unique_ptr<Expression> Parser::BuildUnaryExpr(
    std::unique_ptr<Expression> expr, Token::Value val) {
  if (expr->getType() == AstNode::kNumberLiteral) {
    NumberLiteral* num_expr =
        static_cast<NumberLiteral*>(expr.get());
    if (val == Token::SUB)
      return std::make_unique<NumberLiteral>(-num_expr->value());
    if (val == Token::ADD)
      return std::move(expr);
    if (val == Token::NOT) {
      double nv = num_expr->value();
      return std::make_unique<NumberLiteral>(nv == 0 ? 1 : 0);
    }
  }
  if (val == Token::NOT) {
    auto zero = std::make_unique<NumberLiteral>(0);
    auto one = std::make_unique<NumberLiteral>(1);
    return nullptr;
  }
  return std::make_unique<UnaryOperation>(val, std::move(expr));
}

std::unique_ptr<Expression> Parser::ParseExpression() {
  std::unique_ptr<Expression> lhs = ParseUnaryExpr();
  if (!lhs)   return lhs;

  if (curToken == Token::ASSIGN)
    return ParseAssignment(std::move(lhs));
  return ParseBinopRhs(0, std::move(lhs));
}

// prototype
//   ::= id '(' id* ')'
std::unique_ptr<Prototype> Parser::ParsePrototype() {
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
        return LogErrorP("[Parsing Error] Expecting a number"
                  "for binary operator's precedence.");
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
            std::make_unique<Prototype>(FnName, args_, precedence, Op) :
            std::make_unique<Prototype>(FnName, args_);
}

std::unique_ptr<FunctionDeclaration> Parser::ParseFunctionDecl() {
  // eat 'def'
  getNextToken();
  // get prototype
  std::unique_ptr<Prototype> proto = ParsePrototype();
  if (!proto)   return nullptr;
  if (curToken == Token::LBRACE) {
    auto body = ParseBlock();
    return std::make_unique<FunctionDeclaration>(
                std::move(proto), std::move(body));
  }
  return nullptr;
}

std::unique_ptr<Prototype> Parser::ParseExtern() {
  // eat 'extern'
  getNextToken();
  // get prototype
  std::unique_ptr<Prototype> proto = ParsePrototype();
  return proto;
}

// IfStatement :
//    'if' '(' Expression ')' Statement
//         ('else' Statement) ?
std::unique_ptr<IfStatement> Parser::ParseIfStatement() {
  // eat 'if'
  getNextToken();
  if (!Expect(Token::LPAREN)) return nullptr;
  std::unique_ptr<Expression> cond = ParseExpression();
  if (!cond)
    return nullptr;

  if (!Expect(Token::RPAREN)) return nullptr;
  auto then_stmt = ParseStatement();
  
  if (!then_stmt) return nullptr;

  std::unique_ptr<Statement> else_stmt = nullptr;
  if (Expect(Token::ELSE))
    else_stmt = ParseStatement();

  return std::make_unique<IfStatement>(std::move(cond),
            std::move(then_stmt), std::move(else_stmt));
}

// Forloop
//    := 'for' '(' statement ';' expression';' statement')' statement
std::unique_ptr<ForLoopStatement> Parser::ParseForloop() {
  // eat 'for'
  getNextToken();

  if (!Expect(Token::LPAREN)) {
    LogError("Expecting '(' following keyword 'for' in a ForLoop statement.");
    return nullptr;
  }
 
  auto init_stmt = ParseStatement();
  auto cond_expr = ParseExpression();
  if (!Expect(Token::SEMICOLON))  return nullptr;
  auto next_stmt = ParseStatement();

  if (!Expect(Token::RPAREN)) {
    LogError("Expecting ')' in a ForLoop statement.");
    return nullptr;
  }

  auto body = ParseStatement();
  if (body == nullptr)  return nullptr;
  return std::make_unique<ForLoopStatement>(std::move(init_stmt),
            std::move(cond_expr), std::move(next_stmt), std::move(body));
}

// Variable Declaration
// var a = 1, b, c;
std::unique_ptr<VariableDeclaration> Parser::ParseVariableDecl() {
  // eat keyword 'var'
  getNextToken();

  typedef std::pair<std::string, std::unique_ptr<AstNode>> SEpair;
  std::vector<SEpair> list;
  std::unique_ptr<VariableDeclaration> decl = nullptr;
  VariableDeclaration* current = nullptr;

  while (true) {
    if (curToken != Token::IDENTIFIER) {
      LogError("Expecting an identifier after keyword 'var'.");
      return nullptr;
    }

    std::string var_name(getIdentifierStr());
    std::unique_ptr<Assignment> assign = nullptr;
    // eat variable's id string
    getNextToken();
    if (curToken == Token::ASSIGN) {
      // eat '='
      std::unique_ptr<Identifier> target =
          std::make_unique<Identifier>(var_name);
      assign = ParseAssignment(std::move(target));
    }
    auto temp = std::make_unique<VariableDeclaration>(var_name, std::move(assign));
    if (decl == nullptr) {
      decl = std::unique_ptr<VariableDeclaration>(temp.release());
      current = decl.get();
    } else {
      auto t = temp.get();
      current->set_next(std::move(temp));
      current = t;
    }

    if (curToken == Token::COMMA) {
      getNextToken();
      continue;
    } else if (curToken == Token::SEMICOLON){
      // eat ';'
      getNextToken();
      return decl;
    } else {
      LogError("Expecting an initializer expression or ',' after variable name.");
      return nullptr;
    }
  }
}

// BlockStatement 
//   := '{' statement* '}'
std::unique_ptr<Block> Parser::ParseBlock() {
  // eat left brace
  getNextToken();
  StmtsList *slist = new StmtsList();
  ParseStatementsList(*slist);
  if (!Expect(Token::RBRACE)) {
    LogError("Expecting '}' at the end of a block.");
    return nullptr;
  }
  return std::make_unique<Block>(slist);
}

// ExpressionStatement
//   := expression ';'
std::unique_ptr<ExpressionStatement> Parser::ParseExpressionStmt() {
  auto expr = ParseExpression();
  if (!expr)   return nullptr;
  if (curToken != Token::SEMICOLON) {
    LogError("Expecting a semicolon after expression.");
    return nullptr;
  }
  return std::make_unique<ExpressionStatement>(std::move(expr));
}

std::unique_ptr<EmptyStatement> Parser::ParseEmptyStatement() {
  DCHECK(curToken == Token::SEMICOLON);
  getNextToken();
  return std::make_unique<EmptyStatement>();
}


// ReturnStatement
//   := 'return' expression ';'
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement() {
  DCHECK(curToken == Token::RETURN);
  // eat 'return' keyword
  getNextToken();
  auto expr = ParseExpression();
  DCHECK(curToken == Token::SEMICOLON);
  // eat ';'
  getNextToken();
  return std::make_unique<ReturnStatement>(std::move(expr));
}

// Statement
//   := FunctionDeclaration 
//    | IfStatement
//    | Forloop
//    | BlockStatement
//    | ReturnStatement
//    | EmptyStatement
std::unique_ptr<Statement> Parser::ParseStatement() {
  std::unique_ptr<Statement> result;
  switch (curToken) {
    case Token::DEF:
    case Token::EXTERN:
      result = ParseFunctionDecl();
      break;
    case Token::IF:
      result = ParseIfStatement();
      break;
    case Token::FOR:
      result = ParseForloop();
      break;
    case Token::LBRACE:
      result = ParseBlock();
      break;
    case Token::RETURN:
      result = ParseReturnStatement();
      break;
    case Token::SEMICOLON:
      result = ParseEmptyStatement();
      break;
    case Token::VAR:
      result = ParseVariableDecl();
      break;
    default:
      result = ParseExpressionStmt();
  }
  // eat ';'
  if (curToken == Token::SEMICOLON) getNextToken();
  return result;
}

void Parser::ParseStatementsList(StmtsList& list) {
  while (curToken != Token::EOS &&
            curToken != Token::RBRACE) {
    auto t = ParseStatement();
    if (t == nullptr)   return;
    list.push_back(std::move(t));
  }
}

std::unique_ptr<Block> Parser::ParseToplevel() {
  // move the cursor to the very first AST node.
  getNextToken();
  StmtsList *statement_list = new StmtsList();
  ParseStatementsList(*statement_list);
  return std::make_unique<Block>(statement_list);
}

bool Parser::Expect(Token::Value val) {
  if (curToken == val) {
    getNextToken();
    return true;
  }
  return false;
}

std::unique_ptr<AstNode> LogError(const char* info) {
  std::cerr << info << std::endl;
  return nullptr;
}

Value* LogErrorV(const char* info) {
  LogError(info);
  return nullptr;
}

std::unique_ptr<Prototype> LogErrorP(const char* info) {
  std::cerr << info << std::endl;
  return nullptr;
}

} // namespace Kaleidoscope
