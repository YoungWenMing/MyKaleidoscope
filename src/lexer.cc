#include "src/lexer.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>

#include "src/lexer-inl.h"


namespace Kaleidoscope {

Lexer::Lexer(const Script& script)
  : src_start_(script.source()),
  	src_cursor_(src_start_),
    src_end_(src_start_ + script.length()),
    c0_(' '),
    script_(script) {}

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
  char base = c0_;
  advance();
  switch (base) {
    case '+':
      if (c0_ == '+') return Select(Token::INC);
      if (c0_ == '=') return Select(Token::ASSIGN_ADD);
      return Token::ADD;
    case '-':
      if (c0_ == '-') return Select(Token::DEC);
      if (c0_ == '=') return Select(Token::ASSIGN_SUB);
      return Token::SUB;
    case '*':
      if (c0_ == '=') return Select(Token::ASSIGN_MUL);
      return Token::MUL;
    case '/':
      if (c0_ == '=') return Select(Token::ASSIGN_DIV);
      return Token::DIV;
    case '<':
      if (c0_ == '=') return Select(Token::LTE);
      return Token::LT;
    case '>':
      if (c0_ == '=') return Select(Token::GTE);
      return Token::GT;
    case '(':
      return Token::LPAREN;
    case ')':
      return Token::RPAREN;
    case '[':
      return Token::LBRACK;
    case ']':
      return Token::RBRACK;
    case '{':
      return Token::LBRACE;
    case '}':
      return Token::RBRACE;
    case ':':
      return Token::COLON;
    case ';':
      return Token::SEMICOLON;
    case ',':
      return Token::COMMA;
    case '.':
      return Token::PERIOD;
    case '=':
      if (c0_ == '=') return Select(Token::EQ);
      return Token::ASSIGN;
    case '|':
      if (c0_ == '=') return Select(Token::ASSIGN_OR);
      return Token::OR;
    case '&':
      if (c0_ == '=') return Select(Token::ASSIGN_AND);
      return Token::AND;
    case '!':
      if (c0_ == '=') return Select(Token::NE);
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
