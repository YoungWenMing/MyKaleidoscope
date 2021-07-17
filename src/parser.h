#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <memory>
#include <vector>
#include <map>
#include "src/lexer.h"

#include "include/llvm/ADT/APFloat.h"
#include "include/llvm/ADT/STLExtras.h"
#include "include/llvm/IR/BasicBlock.h"
#include "include/llvm/IR/Constants.h"
#include "include/llvm/IR/DerivedTypes.h"
#include "include/llvm/IR/Function.h"
#include "include/llvm/IR/IRBuilder.h"
#include "include/llvm/IR/LLVMContext.h"
#include "include/llvm/IR/Module.h"
#include "include/llvm/IR/Type.h"
#include "include/llvm/IR/Verifier.h"

namespace Kaleidoscope {

using llvm::Value;
using llvm::LLVMContext;
using llvm::IRBuilder;
using llvm::Module;

using llvm::ConstantFP;
using llvm::APFloat;
// AST Node types:
// Expression, Variable, Number, BinaryOp, Call, Prototype, Function

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value*> ValMap;


class ExprAST {
 public:
  virtual ~ExprAST() = default;
  virtual Value* codegen() = 0;
};

class VariableExprAST : public ExprAST {
  std::string name_;
 public:
  VariableExprAST(std::string& name) : name_(name) {}
  Value* codegen() override;
};

class NumberExprAST : public ExprAST {
  double val_;
 public:
  NumberExprAST(double val) : val_(val) {}
  Value* codegen() override;
};

class BinaryExprAST : public ExprAST {
  char op_;
  std::unique_ptr<ExprAST> lhs_, rhs_;
 public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs)
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  Value* codegen() override;
};

class CallExprAST : public ExprAST {
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST> > args_;
 public:
  CallExprAST(const std::string& callee,
              std::vector<std::unique_ptr<ExprAST>> args)
      : callee_(callee), args_(std::move(args)) {}
  Value* codegen() override;
};

class PrototypeAST : public ExprAST {
  std::string name_;
  std::vector<std::string> args_;

 public:
  PrototypeAST(const std::string& name,
               std::vector<std::string> args)
      : name_(name), args_(std::move(args)) {}
  Value* codegen() override;
};

class FunctionAST : public ExprAST {
  std::unique_ptr<PrototypeAST> proto_;
  std::unique_ptr<ExprAST> body_;
 public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body)
      : proto_(std::move(proto)), body_(std::move(body)) {}
  Value* codegen() override;
};

class Parser {
  Lexer lexer_;
  int cur_token;

  void get_next_token() {
    cur_token = lexer_.next_token();
  }

  std::unique_ptr<ExprAST> ParseExpression();
  std::unique_ptr<NumberExprAST> ParseNumberExpr();
  std::unique_ptr<ExprAST> ParseIdentifierExpr();
  std::unique_ptr<ExprAST> ParseParenExpr();
  std::unique_ptr<ExprAST> ParsePrimary();

  std::unique_ptr<ExprAST> ParseBinopRhs(
      int last_prec, std::unique_ptr<ExprAST> lhs);

  std::unique_ptr<PrototypeAST> ParsePrototype();
  std::unique_ptr<PrototypeAST> ParseExtern();
  std::unique_ptr<FunctionAST> ParseDefinition();
  std::unique_ptr<FunctionAST> ParseToplevelExpr();

  std::unique_ptr<ExprAST> LogError(const char* info);
  std::unique_ptr<PrototypeAST> LogErrorP(const char* info);
#if DEBUG
  void LogInfo(const char* info);
#endif

 public:
  Parser(const char* src);

  void ParseToplevel(std::vector<std::unique_ptr<ExprAST>>& stmts);

  class BinopPrecedency {
   public:
    static int get_precedency(char op);
  };
};

} // Kaleidoscope

#endif // PARSER_H