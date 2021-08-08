#ifndef AST_PRINTER_H
#define AST_PRINTER_H

#include "src/ast.h"
#include <iostream>

namespace Kaleidoscope {

class AstPrinter;

class AstPrinter : public AstVisitor<AstPrinter> {
 public:
  AstPrinter(std::ostream& os) : os_(os)  {}

  void Visit(AstNode* node) {
    AST_VISITOR_BIG_SWITCH(AST_TYPE_LIST);
  }

#define DECLARE_AST_VISIT_FUNC(AstNode)               \
  void Visit##AstNode(AstNode *node);

   AST_TYPE_LIST(DECLARE_AST_VISIT_FUNC)
#undef DECLARA_AST_VISIT_FUNC

 private:
  std::ostream& os_;
};

} // Kaleidoscope 

#endif    //AST_PRINTER_H