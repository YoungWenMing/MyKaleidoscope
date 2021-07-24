#ifndef LEXER_H
#define LEXER_H
#include <vector>
#include <string>
#include <iostream>

namespace Kaleidoscope {

class Lexer {
 public:
  enum Token {
    token_eof = -1,
  
  	// command
    token_def = -2,
  	token_extern = -3,
  
    // primary
    token_number = -4,
    token_identifier = -5,

    // branch control
    token_if = -6,
    token_then = -7,
    token_else = -8,

    // loop control
    token_for = -9,
    token_in = -10
  };

  static void print_token(std::ostream& os, Lexer& lexer, int token);

  Lexer() = delete;
  Lexer(const char* src);

  void Reinitialize(const char* src);

  int next_token();
  std::string identifier_str() { return Identifier; }
  double number_val() { return NumVal; }

 private:
  void advance() {
    curChar = src_cursor_[0];
    ++src_cursor_;
  }

  void retreat() {
    --src_cursor_;
    curChar = src_cursor_[0];
  }

  const char* source_;
  const char* src_cursor_;
  std::string Identifier;
  double NumVal;

  char curChar;
  int cur_tok;
};

} // namespace Kaleidoscope
#endif // LEXER_H
