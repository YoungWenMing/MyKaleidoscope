#include <iostream>
#include <fstream>

#include "src/lexer.h"
#include "src/Codegen.h"
#include "src/token.h"
#include "src/ast-printer.h"

using Kaleidoscope::Token;
using Kaleidoscope::Lexer;
using Kaleidoscope::Script;
using Kaleidoscope::Parser;
using Kaleidoscope::AstNode;
using Kaleidoscope::AstPrinter;
using Kaleidoscope::CodegenDriver;

const char* readSourceFile(const char* path) {
  std::ifstream file_in(path);
  char* buffer = nullptr;

  if (file_in.is_open()) {
    file_in.seekg(0, file_in.end);
    std::size_t length = file_in.tellg();
    file_in.seekg(0, file_in.beg);

    buffer = new char[length];
    file_in.read(buffer, length);
  } else {
    std::cerr << "Cannot open file: " << path << std::endl;
  }
  return buffer;
}

void test_lexer(const char* src) {
  Script script(src, strlen(src));
  Lexer lexer(script);
  // int tok;
  Token::Value tok;
  do {
    tok = lexer.NextToken();
    lexer.PrintCurrentToken(std::cout);
    std::cout << std::endl;
  } while (tok != Token::EOS);
}

void test_ast(const char* src) {
  Script script(src, strlen(src));
  Parser parser(script);
  std::unique_ptr<AstNode> root_block = parser.ParseToplevel();
  AstPrinter printer;
  printer.Visit(root_block.get());
}

void test_codegen(const char* src_name, const char* src) {
  CodegenDriver driver(src_name, src, strlen(src));
  driver.generate_code();
}

int main(int argc, char* argv[]) {
  if (argc > 1) {
    const char* buffer = readSourceFile(argv[argc - 1]);

    bool CheckAST = false;
    bool CheckCode = false;

    for (int i = 1; i < argc - 1; ++i) {
      char* arg = argv[i];
      if (strcmp(arg, "--ast") == 0) {
        CheckAST = true;
      } else if (strcmp(arg, "--code") == 0) {
        CheckCode = true;
      }
    }

    if (buffer && strlen(buffer) != 0) {
      if (CheckAST) {
        test_ast(buffer);
      }
      if (CheckCode) {
        test_codegen(argv[1], buffer);
      }
    }
  }
}
