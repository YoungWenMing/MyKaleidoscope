#ifndef AST_PRINTER_H
#define AST_PRINTER_H

#include "src/ast.h"
#include "src/global.h"
#include <iostream>
#include <sstream>

namespace Kaleidoscope {

class AstPrinter;
class IndentScope;


class AstPrinter : public AstVisitor<AstPrinter> {
 public:
  AstPrinter() {}

  void Visit(const AstNode* node) {
    AST_VISITOR_BIG_SWITCH();
    // ss_ << std::endl;
  }

  AST_TYPE_LIST(DECLARE_VISITOR_FUNC_CONST)

 private:
  void inc_indent() { indent_val += 2; }
  void dec_indent() { 
    DCHECK(indent_val > 2);
    indent_val -= 2;
  }

  void Print(const char* txt) {
    ss_ << txt;
  }

  void PrintNewLine() {
    ss_ << "\n";
  }

  void PrintIndented(const char* txt);
  void PrintIndentedNewLine(const char* txt);
  void PrintIndentedVisit(const char* txt, const AstNode* node);
  
  friend class IndentScope;

  std::ostream& ss_ = std::cout;
  uint32_t indent_val = 0;
};

class IndentScope {
 public:
  IndentScope(AstPrinter* printer, const char* txt);

  ~IndentScope();
 private:
  AstPrinter* printer_;
};
} // Kaleidoscope 

#endif    //AST_PRINTER_H