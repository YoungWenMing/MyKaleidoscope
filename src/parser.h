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

  int getOpsPrecedence(Token::Value token);
  void setOpsPrecedence(Token::Value token, uint32_t prece) {
    preceMap[token] = prece;
  }

  std::unique_ptr<ExprAST> ParseExpression();
  std::unique_ptr<NumberExprAST> ParseNumberExpr();
  std::unique_ptr<ExprAST> ParseIdentifierExpr();
  std::unique_ptr<ExprAST> ParseParenExpr();
  std::unique_ptr<ExprAST> ParsePrimary();

  std::unique_ptr<ExprAST> ParseBinopRhs(
      int last_prec, std::unique_ptr<ExprAST> lhs);
  std::unique_ptr<ExprAST> ParseUnaryExpr();

  std::unique_ptr<ExprAST> BuildUnaryExpr(
      std::unique_ptr<ExprAST> expr, Token::Value val);

  std::unique_ptr<PrototypeAST> ParsePrototype();
  std::unique_ptr<PrototypeAST> ParseExtern();
  std::unique_ptr<FunctionAST> ParseDefinition();
  std::unique_ptr<FunctionAST> ParseToplevelExpr();

  std::unique_ptr<ExprAST> ParseIfExpr();
  std::unique_ptr<ExprAST> ParseForloop();
  std::unique_ptr<ExprAST> ParseVariableDecl();

#if DEBUG
  void LogInfo(const char* info);
#endif

 public:
  Parser(const char* src, size_t len);

  void ParseToplevel(std::vector<std::unique_ptr<ExprAST>>& stmts);

};

} // Kaleidoscope

#endif // PARSER_H