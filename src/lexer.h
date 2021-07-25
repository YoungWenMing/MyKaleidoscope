#ifndef LEXER_H
#define LEXER_H
#include <vector>
#include <string>
#include <iostream>

#include "src/token.h"

namespace Kaleidoscope {

class Lexer {
 public:
  static const int kEndOfSource = -1;
  enum Tokens {
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

  Lexer(const char* src, size_t len);

  void Reinitialize(const char* src);

  int next_token();
  std::string identifier_str() { return Identifier; }
  double number_val() { return NumVal; }

  Token::Value NextToken();
  double NumberVal() { return curToken.number_val; }
  std::string& IdentifierStr() { return curToken.literal_buffer; }

  void PrintCurrentToken(std::ostream&os);

 private:
  void advance();
  struct TokenDesc {
    double number_val;
    std::string literal_buffer;
    Token::Value value = Token::UNINITIALIZED;
  };

  inline void AddLiteralChar(char c);
  inline void AddLiteralCharAdvance();
  inline void ResetDesc();
  inline Token::Value ScanIdentifierOrKeyword();
  Token::Value ScanNumber();
  Token::Value ScanSingleOp();

  template<typename FunctionType>
  inline void AdvanceUntil(FunctionType fn);

  const char* source_;
  const char* src_start_;
  const char* src_cursor_;
  const char* src_end_;
  std::string Identifier;
  double NumVal;

  char c0_;
  int cur_tok;
  TokenDesc curToken;

};

} // namespace Kaleidoscope
#endif // LEXER_H
