#include "src/lexer.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>

#include "src/lexer-inl.h"


namespace Kaleidoscope {

Lexer::Lexer(const char* src, size_t len)
	: src_start_(src),
		src_cursor_(src),
    src_end_(src + len),
    c0_(' ') {}

void Lexer::Reinitialize(const char* src) {
  src_start_ = src_cursor_ = src;
  c0_ = ' ';
}

Token::Value Lexer::NextToken() {
  ResetDesc();
  while (isspace(c0_))  advance();

  if (isdigit(c0_)) {
    curToken.value = ScanNumber();
  } else if (isalpha(c0_) || c0_ == '_') {
    curToken.value = ScanIdentifierOrKeyword();
  } else if (c0_ == '#') {
    while (c0_ != '\n' && c0_ != '\r' && c0_ != '\0' && c0_ != kEndOfSource) {
      advance();
    }
    if (c0_ != kEndOfSource)
      curToken.value = NextToken();
    else
      curToken.value = Token::EOS;
  } else if (c0_ == kEndOfSource) {
    curToken.value = Token::EOS;
  } else {
    curToken.value = ScanSingleOp();
    advance();
  }
  return curToken.value;
}

Token::Value Lexer::ScanNumber() {
  uint64_t value = 0;
  while (isdigit(c0_)) {
    value = 10 * value + (c0_ - '0');
    AddLiteralCharAdvance();
  }
  if (IsAsciiIdentifier(c0_)) return Token::ILLEGAL;
  if (c0_ != '.'
      && value <= SMI_MAX_VALUE
      && curToken.literal_buffer.size() <= 10) {
    curToken.number_val = static_cast<uint32_t>(value);
    return Token::SMI;
  }

  if (c0_ == '.') {
    do {
      AddLiteralCharAdvance();
    } while (isdigit(c0_));
    if (IsAsciiIdentifier(c0_)) return Token::ILLEGAL;
  }

  return Token::NUMBER;
}

Token::Value Lexer::ScanSingleOp() {
  AddLiteralChar(c0_);
  switch (c0_) {
    case '+':
      return Token::ADD;
    case '-':
      return Token::SUB;
    case '*':
      return Token::MUL;
    case '/':
      return Token::DIV;
    case '<':
      return Token::LT;
    case '>':
      return Token::GT;
    case '(':
      return Token::LPAREN;
    case ')':
      return Token::RPAREN;
    case '{':
      return Token::LBRACE;
    case '}':
      return Token::RBRACE;
    case ';':
      return Token::SEMICOLON;
    case ',':
      return Token::COMMA;
    case '.':
      return Token::PERIOD;
    case '=':
      return Token::ASSIGN;
    case '|':
      return Token::OR;
    case '&':
      return Token::AND;
    case '!':
      return Token::NOT;
    default:
      return Token::ILLEGAL;
  }
}

void Lexer::PrintCurrentToken(std::ostream& os) {
  switch (curToken.value) {
#define PRINT_TOKEN_VALUE(name, string, precedence)       \
    case (Token::name):                                   \
      os << "[Value: " << #name;  break;
    TOKEN_LIST(PRINT_TOKEN_VALUE)
#undef PRINT_TOKEN_VALUE
    default:
      os << "[Value: " << "UNINITIALIZED";
  }
  if (curToken.value == Token::IDENTIFIER)
    os << ", String: " << curToken.literal_buffer;
  else if (curToken.value == Token::SMI)
    os << ", Smi: " << SmiVal();
  else if (curToken.value == Token::NUMBER)
    os << ", Number: " << NumberVal();
  os << " ]";
}

} // Kaleidoscope
