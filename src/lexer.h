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

  Lexer(const char* src, size_t len);

  void Reinitialize(const char* src);

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

  const char* src_start_;
  const char* src_cursor_;
  const char* src_end_;

  char c0_;
  TokenDesc curToken;

};

} // namespace Kaleidoscope
#endif // LEXER_H
