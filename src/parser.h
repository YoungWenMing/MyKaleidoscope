#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <memory>
#include <vector>
#include <map>
#include <set>
#include "src/lexer.h"
#include "src/ast.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

namespace Kaleidoscope {

using namespace llvm;

class CodegenContext;

typedef std::map<Token::Value, int> PreceMap;
typedef std::set<Token::Value> UnaryOpSet;


std::unique_ptr<ExprAST> LogError(const char* info);
Value* LogErrorV(const char* info);
std::unique_ptr<PrototypeAST> LogErrorP(const char* info);
class Parser {
  Lexer lexer_;
  Token::Value curToken = Token::UNINITIALIZED;
  PreceMap preceMap;
  UnaryOpSet unarySet;

  friend class CodegenDriver;

  void getNextToken() {
    curToken = lexer_.NextToken();
  }

  std::string& getIdentifierStr() {
    return lexer_.IdentifierStr();
  }

  int getOpsPrecedence(Token::Value token);
  void setOpsPrecedence(Token::Value token, uint32_t prece) {
    preceMap[token] = prece;
  }

  std::unique_ptr<Expression>             ParseExpression();
  std::unique_ptr<NumberLiteral>          ParseNumberExpr();
  std::unique_ptr<Expression>             ParseIdentifierExpr();
  std::unique_ptr<Expression>             ParseParenExpr();
  std::unique_ptr<Expression>             ParsePrimary();

  std::unique_ptr<Assignment>             ParseAssignment(
      std::unique_ptr<Expression> target);

  std::unique_ptr<Expression>             ParseBinopRhs(
      int last_prec, std::unique_ptr<Expression> lhs);
  std::unique_ptr<Expression>             ParseUnaryExpr();

  std::unique_ptr<Expression>             BuildUnaryExpr(
      std::unique_ptr<Expression> expr, Token::Value val);

  std::unique_ptr<PrototypeAST>           ParsePrototype();

  std::unique_ptr<PrototypeAST>           ParseExtern();
  std::unique_ptr<FunctionDeclaration>    ParseFunctionDecl();

  std::unique_ptr<IfStatement>            ParseIfStatement();
  std::unique_ptr<ForloopStatement>       ParseForloop();
  std::unique_ptr<VariableDeclaration>    ParseVariableDecl();
  std::unique_ptr<Block>                  ParseBlock();
  std::unique_ptr<ExpressionStatement>    ParseExpressionStmt();
  std::unique_ptr<EmptyStatement>         ParseEmptyStatement();
  std::unique_ptr<ReturnStatement>        ParseReturnStatement();

  void ParseStatementsList(StmtsList& list);
  std::unique_ptr<Statement> ParseStatement();

  bool Expect(Token::Value val);

#if DEBUG
  void LogInfo(const char* info);
#endif

 public:
  Parser(const char* src, size_t len);

  std::unique_ptr<Block> ParseToplevel();
};

} // Kaleidoscope

#endif // PARSER_H