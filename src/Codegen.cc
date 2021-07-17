
#include "src/Codegen.h"

namespace Kaleidoscope {

CodegenDriver::CodegenDriver(const char* src) :
    src_(src), ctx_(), parser_(src) {}


void CodegenDriver::HandleToplevelExpression() {
  std::unique_ptr<FunctionAST> funcAst = parser_.ParseToplevelExpr();
  if (funcAst) {
    Function* funcIR = funcAst->codegen(ctx_);
    if (!funcIR)  return;
    fprintf(stderr, "Read toplevel expression.");
    funcIR->print(errs());
    fprintf(stderr, "\n");

    // remove the anonymous expression
    funcIR->eraseFromParent();
  } else {
    parser_.get_next_token();
  }
}

void CodegenDriver::HandleExtern() {
  std::unique_ptr<PrototypeAST> externAst = parser_.ParseExtern();
  if (externAst) {
    Function* funcIR = externAst->codegen(ctx_);
    if (!funcIR)  return;
    fprintf(stderr, "Read extern prototype.");
    funcIR->print(errs());
    fprintf(stderr, "\n");
  } else {
    parser_.get_next_token();
  }
}

void CodegenDriver::HandleDefinition() {
  std::unique_ptr<FunctionAST> funcAst = parser_.ParseDefinition();
  if (funcAst) {
    Function* funcIR = funcAst->codegen(ctx_);
    if (!funcIR)  return;
    fprintf(stderr, "Read definition.");
    funcIR->print(errs());
    fprintf(stderr, "\n");
  } else {
    parser_.get_next_token();
  }
}

void CodegenDriver::run() {
  parser_.get_next_token();
  while (parser_.cur_token != Lexer::token_eof) {
    fprintf(stderr, "Reading ------------\n");
    switch(parser_.cur_token) {
      case Lexer::token_def:
        HandleDefinition();
        break;
      case Lexer::token_extern:
        HandleExtern();
        break;
      case ';':
        parser_.get_next_token();
        break;
      default:
        HandleToplevelExpression();
    }
  }
}

} // namespace Kaleidoscope
