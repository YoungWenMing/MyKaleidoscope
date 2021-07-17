#include <iostream>
#include <fstream>

#include "src/lexer.h"
#include "src/Codegen.h"

using Kaleidoscope::Lexer;
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
  Lexer lexer(src);
  int tok;
  do {
    tok = lexer.next_token();
    Lexer::print_token(std::cout, lexer, tok);
    std::cout << std::endl;
  } while (tok != Lexer::token_eof);
}

void test_codegen(const char* src) {
  CodegenDriver driver(src);
  driver.run();
}

int main(int argc, char* argv[]) {
  if (argc > 1) {
    const char* buffer = readSourceFile(argv[1]);

    if (buffer && strlen(buffer) != 0) {
      // test_lexer(buffer);
      test_codegen(buffer);
    }
  }
	// printf("%s\n", "hello world");
}