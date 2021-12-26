#include <iostream>
#include <fstream>

#include "src/lexer.h"
#include "src/Codegen-inl.h"
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
  if (parser.HasParserError()) {
    printf("We got %d error(s) during parsing, please check your source code.",
           parser.GetErrorNums());
  } else {
    AstPrinter printer;
    printer.Visit(root_block.get());
  }
}

void test_codegen(const char* src_name, const char* src) {
  CodegenDriver driver(src_name, src, strlen(src));
  driver.generate_code();
}

void print_helper() {
  std::cout << " Usage: \n"
            << "   cctest [option] target_code_file.kal \n"
            << " Options:\n"
            << "   --token : print all tokens.\n"
            << "   --ast   : print the AST structur.\n"
            << "   --code  : print readable llvm IR.\n"
            << "   --help  : print this doc (for short : -h)\n"
            << std::endl;
}

int main(int argc, char* argv[]) {
  if (argc > 1) {
    const char* buffer = readSourceFile(argv[argc - 1]);

    bool CheckAST       = false;
    bool CheckCode      = false;
    bool CheckToken     = false;
    bool PrintHelpDoc   = false;

    for (int i = 1; i < argc - 1; ++i) {
      char* arg = argv[i];
      if (strcmp(arg, "--ast") == 0) {
        CheckAST = true;
      } else if (strcmp(arg, "--code") == 0) {
        CheckCode = true;
      } else if (strcmp(arg, "--token") == 0) {
        CheckToken = true;
      } else if (strcmp(arg, "--help") == 0
                 || strcmp(arg, "-h") == 0) {
        PrintHelpDoc = true;
      }
    }

    if (buffer && strlen(buffer) != 0) {
      if (PrintHelpDoc) {
        print_helper();
        return 0;
      }
      if (CheckAST) {
        test_ast(buffer);
      }
      if (CheckCode) {
        test_codegen(argv[argc - 1], buffer);
      }
      if (CheckToken) {
        test_lexer(buffer);
      }
    }
  }
}
