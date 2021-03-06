
#include "src/parser.h"
#include "src/Codegen-inl.h"
#include "src/token-inl.h"
#include <iostream>

namespace Kaleidoscope {

#define RECORD_ERR_AND_RETURN_NULL(message)         \
  { const Location loc(lexer_.current_location());  \
    RecordError(message                             \
                ": at %d:%d\n", loc.line, loc.col); \
    return nullptr;                                 \
  }


Parser::Parser(const Script& script) :
    lexer_(script) {}

int Parser::getOpsPrecedence(Token::Value token) {
  auto entry = preceMap.find(token);
  if (entry != preceMap.end()) {
    return entry->second;
  }
  return Token::Precedence(token);
}

std::unique_ptr<SmiLiteral> Parser::ParseSmiLiteral() {
  std::unique_ptr<SmiLiteral> result =
      std::make_unique<SmiLiteral>(current_pos(), lexer_.SmiVal());
  getNextToken();
  return result;
}

std::unique_ptr<NumberLiteral> Parser::ParseNumberLiteral() {
  std::unique_ptr<NumberLiteral> result =
      std::make_unique<NumberLiteral>(current_pos(),
                                      lexer_.NumberVal());
  getNextToken();
  return result;
}

std::unique_ptr<Expression> Parser::ParseIdentifierExpr() {
  // handles simple identifiers and function calls
  std::string id = lexer_.IdentifierStr();

  int pos = current_pos();
  getNextToken();
  if (curToken != Token::LPAREN) {
    return std::make_unique<Identifier>(pos, id);
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
  return std::make_unique<CallExpression>(pos, id, std::move(args));
}

std::unique_ptr<Expression> Parser::ParseParenExpr() {
  getNextToken();
  auto expr = ParseExpression();

  if (curToken != Token::RPAREN)
    RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a right parenthesis.")

  getNextToken();
  return expr;
}

std::unique_ptr<Expression> Parser::ParsePrimary() {
  switch (curToken) {
    case Token::SMI:
      return ParseSmiLiteral();
    case Token::NUMBER:
      return ParseNumberLiteral();
    case Token::IDENTIFIER:
      return ParseIdentifierExpr();
    case Token::LPAREN:
      return ParseParenExpr();
    default:
      RECORD_ERR_AND_RETURN_NULL(
          "[Parsing Error] unexpected token when parsing an expression ")
  }
}

std::unique_ptr<Assignment>
    Parser::ParseAssignment(std::unique_ptr<Expression> lhs) {
  // eat assign operator
  if (!IsValidReference(lhs.get()))
    RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a valid reference.")

  Token::Value op = curToken;
  int pos = current_pos();
  getNextToken();
  auto value = ParseExpression();
  return std::make_unique<Assignment>(
            pos, op, std::move(lhs), std::move(value));
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
    if (rhs == nullptr)
      RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a primary expression.");

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
///   ::= '--' unary
///   ::= '++' unary
std::unique_ptr<Expression> Parser::ParseUnaryExpr() {
  // TODO(yang): both unary op and count op must be considered
  if (!Token::IsUnaryOrCountOp(curToken))  return ParsePostfixExpr();

  Token::Value op = curToken;
  int pos = current_pos();
  getNextToken();
  // Left-recursion is avoided by eating next token.
  if (auto val = ParseUnaryExpr()) {
    if (Token::IsCount(op)) {
      if (!IsValidReference(val.get()))
        RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a"
            " valid reference for count operation ");
      return BuildCountExpr(pos, op, false, std::move(val));
    } else {
      return BuildUnaryExpr(pos, op, std::move(val));
    }
  }
  return nullptr;
}

std::unique_ptr<UnaryOperation> Parser::BuildUnaryExpr(
    int pos, Token::Value val, std::unique_ptr<Expression> expr) {
  return std::make_unique<UnaryOperation>(pos, val, std::move(expr));
}

std::unique_ptr<CountOperation> Parser::BuildCountExpr(
    int pos, Token::Value val, bool is_postfix,
    std::unique_ptr<Expression> expr) {
  return std::make_unique<CountOperation>(pos, val, is_postfix, std::move(expr));
}

std::unique_ptr<Expression> Parser::ParsePostfixExpr() {
  auto expr = ParseLeftHandSideExpr();
  if (!Token::IsCount(curToken)) return std::move(expr);
  // build operation for INC and DEC
  if (!IsValidReference(expr.get()))
    RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a"
        " valid reference for count operation ");
  auto result = BuildCountExpr(expr->pos(), curToken, true, std::move(expr));
  getNextToken();   // consume '++' or '--'
  return std::move(result);
}

std::unique_ptr<Expression> Parser::ParseLeftHandSideExpr() {
  auto expr = ParseMemberExpr();
  if (!Token::IsPropertyOrCall(curToken))  return std::move(expr);
  UNIMPLEMENTED();
  return nullptr;
}

// Member Expression:
//   PrinmaryExpr
//      '[' Expression ']' | '.' Identifier
std::unique_ptr<Expression> Parser::ParseMemberExpr() {
  auto expr = ParsePrimary();
  if (!Token::IsProperty(curToken)) return std::move(expr);
  return ParseMemberExprContinuation(std::move(expr));
}

std::unique_ptr<Expression> Parser::ParseMemberExprContinuation(
    std::unique_ptr<Expression> expr) {
  UNIMPLEMENTED();
  return nullptr;
}

// Initializer list expression
//    '{' {Expression,}* '}'
std::unique_ptr<InitListExpr>
    Parser::ParseInitListExpr(int size, Token::Value element_ty) {
  // eat '{'
  getNextToken();
  int pos = current_pos();
  std::vector<std::unique_ptr<Expression>> init_vec;
  int list_size = 0;
  while (curToken != Token::RBRACE) {
    if (curToken == Token::EOS)
      RECORD_ERR_AND_RETURN_NULL("[Parsing Error]"
          " Unexpected eof in an initializer list ");
    if (curToken == Token::COMMA) {
      init_vec.push_back(nullptr);
      getNextToken();
    } else {
      init_vec.push_back(ParseExpression());
      if (curToken == Token::COMMA)
        getNextToken();
    }
    ++list_size;
  }
  // eat '}'
  getNextToken();
  if (list_size > size)
    RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Too many "
        "elements in initializer list.");
  return std::make_unique<InitListExpr>(pos, element_ty,
                                        size, std::move(init_vec));
}

void Parser::RecordError(const char* format, ...) {
  ++errNums;
  va_list args;
  va_start(args, format);
  VPrintError(format, args);
  va_end(args);
}

std::unique_ptr<Expression> Parser::ParseExpression() {
  std::unique_ptr<Expression> lhs = ParseUnaryExpr();
  if (!lhs)   return lhs;

  if (Token::IsAssignmentOp(curToken))
    return ParseAssignment(std::move(lhs));
  return ParseBinopRhs(0, std::move(lhs));
}

// prototype
//   ::= id '(' type id* ')' : ret_type
std::unique_ptr<Prototype> Parser::ParsePrototype() {
  bool isOp = false;
  if (curToken != Token::IDENTIFIER) {
    if (curToken == Token::BINARY || curToken == Token::UNARY)
      isOp = true;
    else
      RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting an identifier")
  }

  std::string FnName(lexer_.IdentifierStr());
  int precedence = -1;
  int pos = current_pos();
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
    RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a left parenthesis")
  // eat the left parenthesis of function def
  getNextToken();

  std::vector<std::string> args_;
  std::vector<Token::Value> arg_types_;
  while (true) {
    if (!Token::IsParamType(curToken))
      RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a type specifier")

    arg_types_.push_back(curToken);

    getNextToken();
    if (curToken != Token::IDENTIFIER)
      RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting an identifier")

    args_.push_back(lexer_.IdentifierStr());
    getNextToken();

    // eat comma directly
    if (curToken == Token::COMMA)
      getNextToken();
    else if(curToken == Token::RPAREN)
      break;
  }

  // eat ')'
  getNextToken();
  if (curToken == Token::COLON) {
    // eat ':'
    getNextToken();
    if (!Token::IsType(curToken))
      RECORD_ERR_AND_RETURN_NULL("[Parsing Error] Expecting a type specifier")
    auto res = std::make_unique<Prototype>(pos, FnName, args_, arg_types_, curToken);
    // eat return type specifier
    getNextToken();
    return std::move(res);
  }
  return std::make_unique<Prototype>(pos, FnName, args_, arg_types_);
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
  int pos = current_pos();
  getNextToken();
  if (!Check(Token::LPAREN)) return nullptr;
  std::unique_ptr<Expression> cond = ParseExpression();
  if (!cond)
    return nullptr;

  if (!Check(Token::RPAREN)) return nullptr;
  auto then_stmt = ParseStatement();
  
  if (!then_stmt) return nullptr;

  std::unique_ptr<Statement> else_stmt = nullptr;
  if (curToken == Token::ELSE) {
    getNextToken();
    else_stmt = ParseStatement();
  }

  return std::make_unique<IfStatement>(pos, std::move(cond),
            std::move(then_stmt), std::move(else_stmt));
}

// Forloop
//    := 'for' '(' statement ';' expression';' statement')' statement
std::unique_ptr<ForLoopStatement> Parser::ParseForloop() {
  // eat 'for'
  int pos = current_pos();
  getNextToken();

  if (!Check(Token::LPAREN))
    RECORD_ERR_AND_RETURN_NULL("Expecting '(' following keyword 'for' in a ForLoop statement.")
 
  auto init_stmt = ParseStatement();

  std::unique_ptr<Expression> cond_expr = nullptr;
  if (curToken != Token::SEMICOLON)
    cond_expr = ParseExpression();
  if (!Check(Token::SEMICOLON))  return nullptr;

  std::unique_ptr<ExpressionStatement> next_stmt = nullptr;
  if (curToken != Token::RPAREN) {
    auto next_expr = ParseExpression();
    next_stmt =
        std::make_unique<ExpressionStatement>(std::move(next_expr));
  }
  if (!Check(Token::RPAREN))
    RECORD_ERR_AND_RETURN_NULL("Expecting ')' in a ForLoop statement.")

  auto body = ParseStatement();
  if (body == nullptr)  return nullptr;
  return std::make_unique<ForLoopStatement>(pos, std::move(init_stmt),
            std::move(cond_expr), std::move(next_stmt), std::move(body));
}

// Variable Declaration
// int a = 1, b, c;
// double t = 10;
// int a[] = {1,2,3};
std::unique_ptr<VariableDeclaration> Parser::ParseVariableDecl() {
  // eat type specifier
  Token::Value decl_type = curToken;
  int pos = current_pos();
  getNextToken();

  typedef std::pair<std::string, std::unique_ptr<AstNode>> SEpair;
  std::vector<SEpair> list;
  std::unique_ptr<VariableDeclaration> decl = nullptr;
  VariableDeclaration* current = nullptr;

  while (true) {
    if (curToken != Token::IDENTIFIER)
      RECORD_ERR_AND_RETURN_NULL(
          "Expecting an identifier after a type specifier or a comma.")
    
    std::string var_name(getIdentifierStr());
    std::unique_ptr<Expression> init_val = nullptr;
    std::unique_ptr<VariableDeclaration> temp =
        std::make_unique<VariableDeclaration>(pos, var_name,
                                              decl_type);
    // eat variable's id string
    getNextToken();
    // check array case
    if (curToken == Token::LBRACK) {
      temp->set_is_array();
      getNextToken();
      if (curToken != Token::RBRACK) {
        Expect(Token::SMI);
        auto size_expr = ParseSmiLiteral();
        temp->set_array_size(size_expr->value());
      }
      Check(Token::RBRACK);
    }
    if (curToken == Token::ASSIGN) {
      // eat '='
      getNextToken();
      if (curToken == Token::LBRACE) {
        init_val = ParseInitListExpr(temp->array_size(), decl_type);
        if (temp->array_size() == -1 && init_val != nullptr) {
          temp->set_array_size(
              static_cast<InitListExpr*>(init_val.get())->size());
        }
      } else
        init_val = ParseExpression();
      temp->set_init_val(std::move(init_val));
    }
    if (temp->is_array() && temp->array_size() == -1)
      RECORD_ERR_AND_RETURN_NULL("Definition of variable with array "
          "type needs an explicit size or an initialize");
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
    } else
      RECORD_ERR_AND_RETURN_NULL("Expecting an initializer expression,"
                                 "',' or ';' after variable name.")
  }
}

// BlockStatement 
//   := '{' statement* '}'
std::unique_ptr<Block> Parser::ParseBlock() {
  // eat left brace
  int pos = current_pos();
  getNextToken();
  StmtsList *slist = new StmtsList();
  ParseStatementsList(*slist);
  if (!Check(Token::RBRACE))
    RECORD_ERR_AND_RETURN_NULL("Expecting '}' at the end of a block.")
  return std::make_unique<Block>(pos, slist);
}

// ExpressionStatement
//   := expression ';'
std::unique_ptr<ExpressionStatement> Parser::ParseExpressionStmt() {
  auto expr = ParseExpression();
  if (!expr)   return nullptr;
  if (!Check(Token::SEMICOLON))   return nullptr;
  return std::make_unique<ExpressionStatement>(std::move(expr));
}

std::unique_ptr<EmptyStatement> Parser::ParseEmptyStatement() {
  DCHECK(curToken == Token::SEMICOLON);
  return std::make_unique<EmptyStatement>(current_pos());
}


// ReturnStatement
//   := 'return' expression ';'
std::unique_ptr<ReturnStatement> Parser::ParseReturnStatement() {
  DCHECK(curToken == Token::RETURN);
  // eat 'return' keyword
  int pos = current_pos();
  getNextToken();
  auto expr = ParseExpression();
  DCHECK(curToken == Token::SEMICOLON);
  // eat ';'
  getNextToken();
  // TODO(yang): distinguish empty expression or error
  return std::make_unique<ReturnStatement>(pos, std::move(expr));
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
    case Token::INT:
    case Token::DOUBLE:
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
  return std::make_unique<Block>(0, statement_list);
}

bool Parser::Check(Token::Value val) {
  if (Expect(val)) {
    getNextToken();
    return true;
  }
  return false;
}

bool Parser::Expect(Token::Value val) {
  if (curToken != val) {
    const Location loc(lexer_.current_location());
    RecordError("Unexpected token: %s, while the right one is %s, at %d:%d \n",
                Token::TokenName(curToken), Token::TokenName(val),
                loc.line, loc.col);
    return false;
  }
  return true;
}

bool Parser::IsValidReference(Expression* expr) {
  // TODO(yang): support property expressions.
  return expr->IsIdentifier();
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

#undef RECORD_ERR_AND_RETURN_NULL

} // namespace Kaleidoscope
