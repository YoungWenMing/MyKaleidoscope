#ifndef AST_H
#define AST_H

#include "src/token.h"

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

class CodegenContext;

using namespace llvm;
using llvm::Value;

// AST Node types:
// Expression, Variable, Number, BinaryOp, Call, Prototype, Function
#define ASTTypeList(V)                  \
  V(VariableExpr)                       \
  V(NumberExpr)                         \
  V(BinaryExpr)                         \
  V(UnaryExpr)                          \
  V(CallExpr)                           \
  V(Prototype)                          \
  V(Function)                           \
  V(IfExpr)                             \
  V(ForLoop)                            \
  V(UnaryOperation)                     \
  V(VariableDecl)

class ExprAST {
 public:
  enum ASTType : uint8_t {
#define DECLARE_AST_TYPES(type) k##type,
  ASTTypeList(DECLARE_AST_TYPES)
#undef DECLARE_AST_TYPES
  };
  ExprAST(ASTType type) : type_(type) {}

  virtual ~ExprAST() = default;
  virtual Value* codegen(CodegenContext& ctx) = 0;
  ASTType getType() const { return type_; }
 private:
  ASTType type_;
};

class VariableExprAST : public ExprAST {
  std::string name_;
 public:
  VariableExprAST(std::string& name) :
    ExprAST(kVariableExpr),
    name_(name) {}
  Value* codegen(CodegenContext& ctx) override;
  const std::string& varName() const { return name_; }
};

class NumberExprAST : public ExprAST {
  double val_;
 public:
  NumberExprAST(double val) :
    ExprAST(kNumberExpr),
    val_(val) {}
  Value* codegen(CodegenContext& ctx) override;
  double value() const { return val_; }
};

class BinaryExprAST : public ExprAST {
  Token::Value op_;
  std::unique_ptr<ExprAST> lhs_, rhs_;
 public:
  BinaryExprAST(Token::Value op, std::unique_ptr<ExprAST> lhs,
                std::unique_ptr<ExprAST> rhs) :
      ExprAST(kBinaryExpr),
      op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class UnaryExprAST : public ExprAST {
  Token::Value op_;
  std::unique_ptr<ExprAST> val_;
 public:
  UnaryExprAST(Token::Value op, std::unique_ptr<ExprAST> val) :
    ExprAST(kUnaryExpr),
    op_(op), val_(std::move(val)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class CallExprAST : public ExprAST {
  std::string callee_;
  std::vector<std::unique_ptr<ExprAST> > args_;
 public:
  CallExprAST(const std::string& callee,
              std::vector<std::unique_ptr<ExprAST>> args) :
      ExprAST(kCallExpr),
      callee_(callee), args_(std::move(args)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class PrototypeAST : public ExprAST {
  std::string name_;
  std::vector<std::string> args_;
  // store operator info and precedence
  uint32_t flags = 0;
  static constexpr int kPrecedenceOffset = 8;
  static constexpr int kTokenValueOffset = 16;

 public:
  PrototypeAST(const std::string& name,
               std::vector<std::string> args) :
      ExprAST(kPrototype),
      name_(name), args_(std::move(args)) {}
  
  PrototypeAST(const std::string& name,
               std::vector<std::string> args,
               uint32_t precedence, Token::Value token)
      : PrototypeAST(name, args)
         {
    flags = ((precedence & 0xFF) << kPrecedenceOffset) & 1;
    flags |= (token & 0xFF) << kTokenValueOffset;
  }

  std::string& getName() {
    return name_;
  }
  Function* codegen(CodegenContext& ctx) override;

  bool isOperator() const { return flags & 0xFF; }
  // we only support one-char operator currently
  Token::Value getOperator() const {
    assert(isOperator());
    return static_cast<Token::Value>((flags >> kTokenValueOffset) & 0xFF);
  }
  bool isBinaryOp() const { return isOperator() && args_.size() == 2; }
  bool isUnaryOp() const { return isOperator() && args_.size() == 1; }
  uint32_t getPrecedence() const { return (flags >> kPrecedenceOffset) & 0xFF; }

};

class FunctionAST : public ExprAST {
  std::unique_ptr<PrototypeAST> proto_;
  std::unique_ptr<ExprAST> body_;
 public:
  FunctionAST(std::unique_ptr<PrototypeAST> proto,
              std::unique_ptr<ExprAST> body)
      : ExprAST(kFunction),
        proto_(std::move(proto)),
        body_(std::move(body)) {}
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
      : ExprAST(kIfExpr),
        condition_(std::move(condition)),
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
      : ExprAST(kForLoop),
        var_name_(var_name),
        start_(std::move(start)),
        end_(std::move(end)),
        step_(std::move(step)),
        body_(std::move(body)) {}
  Value* codegen(CodegenContext& ctx) override;
};

// only handle intrinsic unary operator for now
class UnaryOperation : public ExprAST {
  Token::Value op_;
  std::unique_ptr<ExprAST> operand_;
 public:
  UnaryOperation(Token::Value op, std::unique_ptr<ExprAST> operand)
    : ExprAST(kUnaryOperation),
      op_(op), operand_(std::move(operand)) {}
  Value* codegen(CodegenContext& ctx) override;
};

class VariableDeclaration : public ExprAST {
  typedef std::pair<std::string, std::unique_ptr<ExprAST>> SEpair;
  std::vector<SEpair> initList;
 public:
  VariableDeclaration(
      std::vector<SEpair> initializer)
      : ExprAST(kVariableDecl),
        initList(std::move(initializer)) {}
  Value* codegen(CodegenContext& ctx) override;
};


} // Kaleidoscope 
#endif  // AST_H
