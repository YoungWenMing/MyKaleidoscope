#ifndef LEXER_H
#define LEXER_H
#include <vector>
#include <string>
#include <iostream>

#include "src/token.h"
#include "src/script.h"

namespace Kaleidoscope {

class Lexer {
 public:
  static const int kEndOfSource = -1;

  Lexer(const Script& script);

  void Reinitialize(const char* src);

  Token::Value NextToken();
  uint32_t SmiVal()   { return curToken.number_val; }
  double NumberVal() {
    return strtod(curToken.literal_buffer.c_str(), nullptr);
  }

  std::string& IdentifierStr() { return curToken.literal_buffer; }

  void PrintCurrentToken(std::ostream&os);
  int current_pos() const { return src_cursor_ - src_start_; }
  const Location current_location() const {
    return script_.get_location(current_pos());
  }

 private:
  void advance();
  struct TokenDesc {
    uint32_t number_val;
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

  const Script& script_;

  char c0_;
  TokenDesc curToken;

};

} // namespace Kaleidoscope
#endif // LEXER_H
