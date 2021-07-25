#ifndef PARSER_H
#define PARSER_H

#include <string>
#include <memory>
#include <vector>
#include <map>
#include "src/lexer.h"

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

// AST Node types:
// Expression, Variable, Number, BinaryOp, Call, Prototype, Function

static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value*> ValMap;


class ExprAST {
 public:
  virtual ~ExprAST() = default;
  virtual Value* codegen(CodegenContext& ctx) = 0;
};

class VariableExprAST : public ExprAST {
  std::string name_;
 public:
  VariableExprAST(std::string& name) : name_(name) {}
  Value* codegen(CodegenContext& ctx) override;
};

class NumberExprAST : public ExprAST {
  double val_;
 public:
  NumberExprAST(double val) : val_(val) {}
  Value* codegen(CodegenContext& ctx) override;
};

class BinaryExprAST : public ExprAST {
  char op_;
  std::unique_ptr<ExprAST> lhs_, rhs_;
 public:
  BinaryExprAST(char op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs)
      : op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class CallExprAST : public ExprAST {
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST> > args_;
 public:
  CallExprAST(const std::string& callee,
              std::vector<std::unique_ptr<ExprAST>> args)
      : callee_(callee), args_(std::move(args)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class PrototypeAST : public ExprAST {
  std::string name_;
  std::vector<std::string> args_;

 public:
  PrototypeAST(const std::string& name,
               std::vector<std::string> args)
      : name_(name), args_(std::move(args)) {}
  std::string getName() {
    return name_;
  }
  Function* codegen(CodegenContext& ctx) override;
};

class FunctionAST : public ExprAST {
  std::unique_ptr<PrototypeAST> proto_;
  std::unique_ptr<ExprAST> body_;
 public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body)
      : proto_(std::move(proto)), body_(std::move(body)) {}
  Function* codegen(CodegenContext& ctx) override;
};

class IfExprAST : public ExprAST {
  std::unique_ptr<ExprAST> condition_;
  std::unique_ptr<ExprAST> thenB_;
  std::unique_ptr<ExprAST> elseB_;
 public:
  IfExprAST(std::unique_ptr<ExprAST> condition,
            std::unique_ptr<ExprAST> thenB,
            std::unique_ptr<ExprAST> elseB)
      : condition_(std::move(condition)),
        thenB_(std::move(thenB)),
        elseB_(std::move(elseB)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class ForloopAST : public ExprAST {
  std::string var_name_;
  std::unique_ptr<ExprAST> start_, end_, step_, body_;
 public:
  ForloopAST(const std::string& var_name,
             std::unique_ptr<ExprAST> start,
             std::unique_ptr<ExprAST> end,
             std::unique_ptr<ExprAST> step,
             std::unique_ptr<ExprAST> body)
      : var_name_(var_name),
        start_(std::move(start)),
        end_(std::move(end)),
        step_(std::move(step)),
        body_(std::move(body)) {}
  Value* codegen(CodegenContext& ctx) override;
};

std::unique_ptr<ExprAST> LogError(const char* info);
Value* LogErrorV(const char* info);
std::unique_ptr<PrototypeAST> LogErrorP(const char* info);
class Parser {
  Lexer lexer_;
  int cur_token;

  friend class CodegenDriver;

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

  std::unique_ptr<ExprAST> ParseIfExpr();
  std::unique_ptr<ExprAST> ParseForloop();

#if DEBUG
  void LogInfo(const char* info);
#endif

 public:
  Parser(const char* src, size_t len);

  void ParseToplevel(std::vector<std::unique_ptr<ExprAST>>& stmts);

  class BinopPrecedency {
   public:
    static int get_precedency(char op);
  };
};

} // Kaleidoscope

#endif // PARSER_H