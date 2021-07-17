#include <iostream>
#include <fstream>

#include "src/lexer.h"

using Kaleidoscope::Lexer;

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

int main(int argc, char* argv[]) {
  if (argc > 1) {
    const char* buffer = readSourceFile(argv[1]);

    if (buffer && strlen(buffer) != 0) {
      Lexer lexer(buffer);
      int tok;
      do {
        tok = lexer.next_token();
        Lexer::print_token(std::cout, lexer, tok);
        std::cout << std::endl;
      } while (tok != Lexer::token_eof);
    }
  }
	// printf("%s\n", "hello world");
}