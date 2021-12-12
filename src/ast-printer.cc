#include "src/ast-printer.h"

namespace Kaleidoscope {

IndentScope::IndentScope(AstPrinter* printer, const char* txt)
  : printer_(printer) {
  printer_->PrintIndented(txt);
  printer_->Print("\n");
  printer_->inc_indent();
}

IndentScope::~IndentScope() {
    printer_->dec_indent();
}

void AstPrinter::PrintIndented(const char* txt) {
  for (uint32_t i = 0; i < indent_val; ++i) {
    Print(" ");
  }
  Print(txt);
}

void AstPrinter::PrintIndentedNewLine(const char* txt) {
  PrintIndented(txt);
  Print("\n");
}

void AstPrinter::PrintIndentedVisit(const char* txt, const AstNode* node) {
  if (node != nullptr) {
    IndentScope scope(this, txt);
    this->Visit(node);
  }
}

void AstPrinter::VisitEmptyStatement(const EmptyStatement* node) {
  PrintIndented("[EmptyStatement]");
}

void AstPrinter::VisitIfStatement(const IfStatement* node) {
  IndentScope scope(this, "IF:");
  PrintIndentedVisit("CONDITION:", node->condition());
  PrintIndentedVisit("THEN:", node->then_stmt());
  PrintIndentedVisit("ELSE:", node->else_stmt());
}

void AstPrinter::VisitForLoopStatement(const ForLoopStatement* node) {
  {
    IndentScope scope(this, "ForLoop:{");
    PrintIndentedVisit("INIT:", node->init());
    PrintIndentedVisit("CONDITION:", node->condition());
    PrintIndentedVisit("NEXT:", node->next());
    PrintIndentedVisit("Body:", node->body());
  }
  PrintIndented("}\n");
}

void AstPrinter::VisitBlock(const Block* node) {
  {
    IndentScope scope2(this, "Block: {");
    const StmtsList *statements_list = node->statements();
    for (int i = 0; i < statements_list->size(); ++i)
      this->Visit(statements_list->at(i).get());
  }
  PrintIndented("}\n");
}

void AstPrinter::VisitReturnStatement(const ReturnStatement* node) {
  PrintIndentedVisit("Return:", node->expression());
}

void AstPrinter::VisitExpressionStatement(const ExpressionStatement* node) {
  PrintIndentedVisit("ExpressionStatement:{", node->expresssion());
  PrintIndentedNewLine("}");
}

void AstPrinter::VisitVariableDeclaration(const VariableDeclaration* node) {
  {
    IndentScope scope(this, "VariableDeclaration: [");
    while (node != nullptr) {
      PrintIndented(node->var_name().c_str());
      Print(": {");
      if (node->init_val() != nullptr) {
        PrintIndentedVisit("", node->init_val());
      }
      Print("\n");
      PrintIndentedNewLine("},");
      node = node->next();
    }
  }
  PrintIndented("]\n");
}

void AstPrinter::VisitIdentifier(const Identifier* node) {
  IndentScope scope(this, "Identifier:");
  PrintIndented("name: ");
  Print(node->var_name().c_str());
  Print("\n");
}

void AstPrinter::VisitSmiLiteral(const SmiLiteral* node) {
  IndentScope scope(this, "SmiLiteral:");
  PrintIndented(std::to_string(node->value()).c_str());
  PrintNewLine();
}

void AstPrinter::VisitNumberLiteral(const NumberLiteral* node) {
  IndentScope scope(this, "NumberLiteral:");
  PrintIndented(std::to_string(node->value()).c_str());
  PrintNewLine();
}

void AstPrinter::VisitBinaryExpression(const BinaryExpression* node) {
  IndentScope scope(this, "BinaryExpression:");

  std::string op_info("Operator: ");
  op_info.append(Token::TokenName(node->operator_token()));
  PrintIndentedNewLine(op_info.c_str());
  PrintIndentedVisit("LHS: ", node->left_expr());
  PrintIndentedVisit("RHS: ", node->right_expr());
}

void AstPrinter::VisitUnaryExpression(const UnaryExpression* node) {
  IndentScope scope(this, "UnaryExpression:");
  std::string op_info("Operator: ");
  op_info.append(Token::TokenName(node->operator_token()));
  PrintIndentedNewLine(op_info.c_str());

  PrintIndentedVisit("TargetExpression: ", node->target_expr());
}

void AstPrinter::VisitAssignment(const Assignment* node) {
  IndentScope scope(this, "Assignment:");

  PrintIndentedVisit("Target:", node->target());
  PrintIndentedVisit("Value:", node->value());
}

void AstPrinter::VisitCallExpression(const CallExpression* node) {
  {
    IndentScope scope(this, "CallExpression:{");

    PrintIndented("Callee:");
    Print(node->callee().c_str());
    PrintNewLine();

    auto args_vec = node->args();
    {
      IndentScope scope2(this, "Args: {");
      for (int i = 0; i < args_vec->size(); ++i) {
        PrintIndented(std::to_string(i).c_str());
        Print(":\n");
        this->Visit(args_vec->at(i).get());
      }
    }
    PrintIndentedNewLine("}");
  }
  PrintIndentedNewLine("}");
}

void AstPrinter::VisitCountOperation(const CountOperation* node) {
  IndentScope scope(this, "CountOperation:");
  PrintIndented("Operation: ");
  Print(Token::TokenName(node->get_operator()));
  PrintNewLine();
  {
    IndentScope scope2(this, "Target: {");
    this->Visit(node->target_expr());
  }
  PrintIndentedNewLine("}");
}

void AstPrinter::VisitPrototype(const Prototype* node) {
  IndentScope scope(this, "Prototype:");

  PrintIndented("Name: ");
  Print(node->getName().c_str());
  Print(",\n");
  PrintIndented("Args:[ ");
  auto args = node->args();
  for (int i = 0; i < args.size(); ++i) {
    Print(args[i].c_str());
    Print(", ");
  }
  Print("]\n");

}

void AstPrinter::VisitUnaryOperation(const UnaryOperation* node) {
  {
    IndentScope scope(this, "UnaryOperation:");
    PrintIndented("Operator: ");
    Print(Token::TokenName(node->operator_token()));
    PrintNewLine();
    PrintIndentedVisit("TargetExpression:", node->operand());
  }
  PrintIndented("}\n");
}

void AstPrinter::VisitFunctionDeclaration(const FunctionDeclaration* node) {
  PrintIndented("FunctionDelcaration:{");
  {
    IndentScope scope(this, "");
    PrintIndentedVisit("Prototype:", node->prototype());
    PrintIndentedVisit("Body:", node->body());
  }
  PrintIndented("}\n");

}

} // Kaleidoscope 
