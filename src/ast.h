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
class Parser;

class Block;

using namespace llvm;
using llvm::Value;

#define UPtr(name) std::unique_ptr<name>

// AST Node types:
// Expression, Variable, Number, BinaryOp, Call, Prototype, Function
#define STATEMENT_NODE_LIST(V)          \
  V(EmptyStatement)                     \
  V(IfStatement)                        \
  V(ForLoopStatement)                   \
  V(Block)                              \
  V(ReturnStatement)                    \
  V(ExpressionStatement)                \
  V(VariableDeclaration)                \
  V(FunctionDeclaration)


#define EXPRESSION_NODE_LIST(V)         \
  V(Identifier)                         \
  V(SmiLiteral)                         \
  V(NumberLiteral)                      \
  V(BinaryExpression)                   \
  V(UnaryExpression)                    \
  V(Assignment)                         \
  V(CallExpression)                     \
  V(Prototype)                          \
  V(UnaryOperation)                     \
  V(CountOperation)

#define AST_TYPE_LIST(V)                \
  STATEMENT_NODE_LIST(V)                \
  EXPRESSION_NODE_LIST(V)

class AstNode {
 public:
  enum ASTType : uint8_t {
#define DECLARE_AST_TYPES(type) k##type,
  AST_TYPE_LIST(DECLARE_AST_TYPES)
#undef DECLARE_AST_TYPES
  };

  AstNode(int pos, ASTType type)
      : pos_(pos), type_(type) {}
  virtual ~AstNode() = default;

  virtual Value* codegen(CodegenContext& ctx) = 0;
  ASTType getType() const { return type_; }

#define CHECK_AST_TYPES(type)           \
  bool Is##type() const                 \
  {  return getType() == k##type; }
  AST_TYPE_LIST(CHECK_AST_TYPES)
#undef CHECK_AST_TYPES
  int pos() const { return pos_; }
 private:
  int pos_;
 protected:
  ASTType type_;
};

class Statement : public AstNode {
 public:
  Statement(int pos, ASTType type)
      : AstNode(pos, type) {}
  virtual ~Statement() = default;
};

class Expression : public AstNode {
 public:
  Expression(int pos, ASTType type)
      : AstNode(pos, type) {}
  virtual ~Expression() = default;
#define IS_SOME_EXPR_FUNC(type)             \
  bool Is##type() const {                   \
    return type_ == k##type;                \
  }
  EXPRESSION_NODE_LIST(IS_SOME_EXPR_FUNC)

#undef IS_SOME_EXPR_FUNC
};


typedef std::vector<std::unique_ptr<Statement>> StmtsList;

class ExpressionStatement : public Statement {
  std::unique_ptr<Expression> expr_;
 public:
  ExpressionStatement(std::unique_ptr<Expression> expr)
      : Statement(expr->pos(), kExpressionStatement),
        expr_(std::move(expr)) {}
  Value* codegen(CodegenContext& ctx) override;
  const Expression* expresssion() const {
    return expr_.get();
  }
};

class Identifier : public Expression {
  std::string name_;
 public:
  Identifier(int pos, std::string& name) :
    Expression(pos, kIdentifier),
    name_(name) {}
  Value* codegen(CodegenContext& ctx) override;
  const std::string& var_name() const { return name_; }
};

class SmiLiteral : public Expression {
 public:
  SmiLiteral(int pos, uint32_t val)
      : Expression(pos, kSmiLiteral),
        val_(val) {}

  virtual ~SmiLiteral() = default;
  Value* codegen(CodegenContext& ctx) override;
  uint32_t value() const { return val_; }

 private:
  uint32_t val_;
};

class NumberLiteral : public Expression {
  double val_;
 public:
  NumberLiteral(int pos, double val) :
    Expression(pos, kNumberLiteral),
    val_(val) {}
  Value* codegen(CodegenContext& ctx) override;
  double value() const { return val_; }
};

class BinaryExpression : public Expression {
  Token::Value op_;
  std::unique_ptr<Expression> lhs_, rhs_;
 public:
  BinaryExpression(Token::Value op, std::unique_ptr<Expression> lhs,
                std::unique_ptr<Expression> rhs) :
      Expression(lhs->pos(), kBinaryExpression),
      op_(op), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  Value* codegen(CodegenContext& ctx) override;
  const Expression* left_expr() const { return lhs_.get(); }
  const Expression* right_expr() const { return rhs_.get(); }
  Token::Value operator_token() const { return op_; }
};

class UnaryExpression : public Expression {
  Token::Value op_;
  std::unique_ptr<Expression> val_;
 public:
  UnaryExpression(Token::Value op, std::unique_ptr<Expression> val) :
    Expression(val->pos(), kUnaryExpression),
    op_(op), val_(std::move(val)) {}
  Value* codegen(CodegenContext& ctx) override;
  Token::Value operator_token() const { return op_; }
  const Expression* target_expr() const { return val_.get(); }
};

class CallExpression : public Expression {
  std::string callee_;
  std::vector<std::unique_ptr<Expression> > args_;
 public:
  CallExpression(int pos, const std::string& callee,
              std::vector<std::unique_ptr<Expression>> args) :
      Expression(pos, kCallExpression),
      callee_(callee), args_(std::move(args)) {}
  Value* codegen(CodegenContext& ctx) override;
  const std::string& callee() const { return callee_; }
  const std::vector<std::unique_ptr<Expression> >*
      args() const { return &args_; }
};

class CountOperation : public Expression {
  Token::Value tok_;
  bool is_postfix_;
  std::unique_ptr<Expression> expr_;
 public:
  CountOperation(int pos,
                 Token::Value tok,
                 bool is_postfix,
                 std::unique_ptr<Expression> expr)
    : Expression(pos, kCountOperation),
      tok_(tok),
      is_postfix_(is_postfix),
      expr_(std::move(expr)) {}

  Value* codegen(CodegenContext& ctx) override;
  Token::Value get_operator() const { return tok_; }
  const Expression* target_expr() const { return expr_.get(); }
};

class Prototype : public Expression {
 private:
  friend class FunctionDeclaration;
  std::string name_;
  std::vector<std::string> args_;
  std::vector<Token::Value> arg_types_;
  Token::Value ret_type_;
  // store operator info and precedence
  uint32_t flags = 0;
  static constexpr int kPrecedenceOffset = 8;
  static constexpr int kTokenValueOffset = 16;

 public:
  Prototype(int pos,
            const std::string& name,
            std::vector<std::string> args,
            std::vector<Token::Value> arg_types,
            Token::Value retTy = Token::VOID) :
      Expression(pos, kPrototype),
      name_(name),
      args_(std::move(args)),
      arg_types_(std::move(arg_types)),
      ret_type_(retTy) {}

  const std::string& getName() const {
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
  const std::vector<std::string>& args() const { return args_; }
};

class FunctionDeclaration : public Statement {
  std::unique_ptr<Prototype> proto_;
  std::unique_ptr<Block> body_;
 public:
  FunctionDeclaration (std::unique_ptr<Prototype> proto,
              std::unique_ptr<Block> body)
      : Statement(proto->pos(), kFunctionDeclaration),
        proto_(std::move(proto)),
        body_(std::move(body)) {}
  Function* codegen(CodegenContext& ctx) override;

  const Prototype* prototype() const { return proto_.get(); }
  const Block* body() const { return body_.get(); }
};

class IfStatement : public Statement {
  std::unique_ptr<Expression> condition_;
  std::unique_ptr<Statement> thenB_;
  std::unique_ptr<Statement> elseB_;
 public:
  IfStatement(int pos,
              std::unique_ptr<Expression> condition,
              std::unique_ptr<Statement> thenB,
              std::unique_ptr<Statement> elseB)
      : Statement(pos, kIfStatement),
        condition_(std::move(condition)),
        thenB_(std::move(thenB)),
        elseB_(std::move(elseB)) {}
  Value* codegen(CodegenContext& ctx) override;
  const Expression* condition() const { return condition_.get(); }
  const Statement* then_stmt() const { return thenB_.get(); }
  const Statement* else_stmt() const { return elseB_.get(); }
};

class ForLoopStatement : public Statement {
  std::unique_ptr<Expression> condition_;
  std::unique_ptr<Statement> init_, next_, body_;
 public:
    ForLoopStatement (int pos,
        std::unique_ptr<Statement> init,
        std::unique_ptr<Expression> condition,
        std::unique_ptr<Statement> next,
        std::unique_ptr<Statement> body)
      : Statement(pos, kForLoopStatement),
        init_(std::move(init)),
        condition_(std::move(condition)),
        next_(std::move(next)),
        body_(std::move(body)) {}
  Value* codegen(CodegenContext& ctx) override;
  const Expression* condition() const { return condition_.get(); }
  const Statement* init() const { return init_.get(); }
  const Statement* next() const { return next_.get(); }
  const Statement* body() const { return body_.get(); }
};

// only handle intrinsic unary operator for now
class UnaryOperation : public Expression {
  Token::Value op_;
  std::unique_ptr<Expression> operand_;
 public:
  UnaryOperation(int pos, Token::Value op,
      std::unique_ptr<Expression> operand)
      : Expression(pos, kUnaryOperation),
        op_(op), operand_(std::move(operand)) {}
  Value* codegen(CodegenContext& ctx) override;
  Token::Value operator_token() const { return op_; }
  const Expression* operand() const { return operand_.get(); }
};

class Assignment : public Expression {
  Token::Value op_;
  std::unique_ptr<Expression> target_;
  std::unique_ptr<Expression> value_;
 public:
  Assignment(int pos, Token::Value op,
      std::unique_ptr<Expression> target,
      std::unique_ptr<Expression> value)
    : Expression(pos, kAssignment),
      op_(op),
      target_(std::move(target)),
      value_(std::move(value)) {}
  Value* codegen(CodegenContext& ctx) override;
  const Expression* target() const { return target_.get(); }
  const Expression* value() const { return value_.get(); }
  const Token::Value op_type() const { return op_; }
  ASTType valueType() const { return value_->getType();}
};

class VariableDeclaration : public Statement {
  std::string name_;
  Token::Value decl_type_;
  std::unique_ptr<Expression> init_val_;
  std::unique_ptr<VariableDeclaration> next_ = nullptr;
 public:
  VariableDeclaration(int pos, std::string& var_name,
      Token::Value decl_type, std::unique_ptr<Expression> init_val)
      : Statement(pos, kVariableDeclaration),
        name_(var_name),
        decl_type_(decl_type),
        init_val_(std::move(init_val)) {}
  Value* codegen(CodegenContext& ctx) override;

  void set_next(std::unique_ptr<VariableDeclaration> next) {
    next_ = std::move(next);
  }
  const VariableDeclaration* next() const {
    return next_.get();
  }
  const std::string& var_name() const {
    return name_;
  }
  const Expression* init_val() const {
    return init_val_.get();
  }
};

class Block : public Statement {
  StmtsList* statements_;
 public:
  Block(int pos, StmtsList* statements)
      : Statement(pos, kBlock),
        statements_(std::move(statements)) {}
  Value* codegen(CodegenContext& ctx) override;
  const StmtsList* statements() const {
    return statements_;
  }
};

class EmptyStatement : public Statement {
 public:
  EmptyStatement(int pos) : Statement(pos, kEmptyStatement) {}
  Value* codegen(CodegenContext& ctx);
};

class ReturnStatement : public Statement {
  std::unique_ptr<Expression> expression_;
 public:
  ReturnStatement(int pos, std::unique_ptr<Expression> expr)
      : Statement(pos, kReturnStatement),
        expression_(std::move(expr)) {}
  Value* codegen(CodegenContext& ctx) override;
  const Expression* expression() const {
    return expression_.get();
  }
};

template<typename SubClass>
class AstVisitor {
 public:
  void Visit(AstNode* ast_node) { impl()->Visit(ast_node); }
 protected:
  SubClass* impl() {
    return static_cast<SubClass*>(this);
  }
};

#define AST_VISIT_CASAES(NodeType)                    \
  case AstNode::k##NodeType:                          \
    this->impl()->Visit##NodeType(                    \
        static_cast<const NodeType*>(node));  break;

#define AST_VISITOR_BIG_SWITCH()                      \
  switch(node->getType()) {                           \
  AST_TYPE_LIST(AST_VISIT_CASAES)                     \
  default:  UNREACHABLE();}

#define DECLARE_VISITOR_FUNC(AstNode)                 \
  void Visit##AstNode(AstNode *node);

#define DECLARE_VISITOR_FUNC_CONST(AstNode)           \
  void Visit##AstNode(const AstNode *node);


// #define AST_VISITOR_MEMBERS_DECL(NodeType)



} // Kaleidoscope 
#endif  // AST_H
