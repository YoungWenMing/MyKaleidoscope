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


std::unique_ptr<AstNode> LogError(const char* info);
Value* LogErrorV(const char* info);
std::unique_ptr<Prototype> LogErrorP(const char* info);
class Parser {
  Lexer lexer_;
  Token::Value curToken = Token::UNINITIALIZED;
  PreceMap preceMap;
  UnaryOpSet unarySet;
  bool hasParseError = false;
  int errNums = 0;

  friend class CodegenDriver;

  void getNextToken() {
    curToken = lexer_.NextToken();
  }

  int current_pos() const { return lexer_.current_pos(); }

  std::string& getIdentifierStr() {
    return lexer_.IdentifierStr();
  }

  int getOpsPrecedence(Token::Value token);
  void setOpsPrecedence(Token::Value token, uint32_t prece) {
    preceMap[token] = prece;
  }

  void RecordError(const char* format, ...);

  std::unique_ptr<Expression>             ParseExpression();
  std::unique_ptr<SmiLiteral>             ParseSmiLiteral();
  std::unique_ptr<NumberLiteral>          ParseNumberLiteral();
  std::unique_ptr<Expression>             ParseIdentifierExpr();
  std::unique_ptr<Expression>             ParseParenExpr();
  std::unique_ptr<Expression>             ParsePrimary();

  std::unique_ptr<Assignment>             ParseAssignment(
      std::unique_ptr<Expression> target);

  std::unique_ptr<Expression>             ParseBinopRhs(
      int last_prec, std::unique_ptr<Expression> lhs);
  std::unique_ptr<Expression>             ParseUnaryExpr();

  std::unique_ptr<UnaryOperation>             BuildUnaryExpr(
      std::unique_ptr<Expression> expr, int pos, Token::Value val);
  std::unique_ptr<CountOperation>             BuildCountExpr(
      std::unique_ptr<Expression> expr, int pos,
      Token::Value val, bool is_postfix);
  std::unique_ptr<Expression>             ParsePostfixExpr();
  std::unique_ptr<Expression>             ParseLeftHandSideExpr();
  std::unique_ptr<Expression>             ParseMemberExpr();
  std::unique_ptr<Expression>             ParseMemberExprContinuation(
      std::unique_ptr<Expression> expr);

  std::unique_ptr<Prototype>              ParsePrototype();

  std::unique_ptr<Prototype>              ParseExtern();
  std::unique_ptr<FunctionDeclaration>    ParseFunctionDecl();

  std::unique_ptr<IfStatement>            ParseIfStatement();
  std::unique_ptr<ForLoopStatement>       ParseForloop();
  std::unique_ptr<VariableDeclaration>    ParseVariableDecl();
  std::unique_ptr<Block>                  ParseBlock();
  std::unique_ptr<ExpressionStatement>    ParseExpressionStmt();
  std::unique_ptr<EmptyStatement>         ParseEmptyStatement();
  std::unique_ptr<ReturnStatement>        ParseReturnStatement();
  std::unique_ptr<Statement>              ParseStatement();

  void ParseStatementsList(StmtsList& list);

  bool Check(Token::Value val);
  bool Expect(Token::Value val);

  void setParserError() { hasParseError = true; }
#if DEBUG
  void LogInfo(const char* info);
#endif

  bool IsValidReference(Expression* expr);

 public:
  Parser(const Script& script);

  std::unique_ptr<Block> ParseToplevel();

  bool HasParserError() const { return errNums != 0; }
};

} // Kaleidoscope

#endif // PARSER_H