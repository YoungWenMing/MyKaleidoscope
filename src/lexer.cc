#include "src/lexer.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>

#include "src/lexer-inl.h"


namespace Kaleidoscope {

Lexer::Lexer(const char* src, size_t len)
	: source_(src),
    src_start_(src),
		src_cursor_(src),
    src_end_(src + len),
		Identifier(""),
		NumVal(0),
    c0_(' '),
		cur_tok(0) {}

void Lexer::Reinitialize(const char* src) {
  source_ = src_cursor_ = src;
  NumVal = 0;
  c0_ = ' ';
  cur_tok = 0;
}

int Lexer::next_token() {
  // advance();
  // c0_ = ' ';

  while (isspace(c0_)) {
    advance();
  }

  // for number parsing
  if (isdigit(c0_)) {
    std::string NumberStr;

    // TODO: consider multiple dot in string like '1.23.456'
    do {
      NumberStr.push_back(c0_);
      advance();
    } while (isdigit(c0_) || c0_ == '.');

    NumVal = strtod(NumberStr.c_str(), NULL);

    return token_number;
  }

  // for identifier parsing
  if (isalpha(c0_) || c0_ == '_') {
    Identifier.clear();
    do {
      Identifier.push_back(c0_);
      advance();
    } while (isalnum(c0_) || c0_ == '_');

    if (Identifier == "def")
      return token_def;
    if (Identifier == "extern")
      return token_extern;
    if (Identifier == "if")
      return token_if;
    if (Identifier == "then")
      return token_then;
    if (Identifier == "else")
      return token_else;
    if (Identifier == "for")
      return token_for;
    if (Identifier == "in")
      return token_in;
    return token_identifier;
  }
    

  // single-line comment
  if (c0_ == '#') {
    while (c0_ != '\n' && c0_ != '\r' && c0_ != '\0') {
      advance();
    }

    if (c0_ != '\0') 
      return next_token();
  }

  if (c0_ == '\0') return token_eof;

  // for those charactor we do not recognize yet
  int t = c0_;
  advance();
  return t;
}

Token::Value Lexer::NextToken() {
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
  ResetDesc();
  AddLiteralCharAdvance();
  // we only support decimal number here
  uint8_t dot_num = 0;
  while (isdigit(c0_) || c0_ == '.') {
    AddLiteralCharAdvance();
    if (c0_ == '.' && dot_num < 2)  ++dot_num;
  }
  if (dot_num == 2)  return Token::ILLEGAL;
  curToken.number_val = strtod(curToken.literal_buffer.c_str(), NULL);
  return Token::NUMBER;
}

Token::Value Lexer::ScanSingleOp() {
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
    case ';':
      return Token::SEMICOLON;
    case ',':
      return Token::COMMA;
    case '=':
      return Token::ASSIGN;
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
  else if (curToken.value == Token::NUMBER)
    os << ", Number: " << curToken.number_val;
  os << " ]";
}

void Lexer::print_token(std::ostream& os, Lexer& lexer, int token) {
  switch (token) {
    case token_eof:
      os << "[ Type: EOF, Value: Non ]";
      break;
    case token_def:
      os << "[ Type: KeyWord, Value: def ]";
      break;
    case token_extern:
      os << "[ Type: Keyword, Value: extern ]";
      break;
    case token_identifier:
      os << "[ Type: Primary, Value: " << lexer.identifier_str() << " ]";
      break;
    case token_number:
      os << "[ Type: Primary, Value: " << lexer.number_val() << " ]";
      break;
    case token_if:
      os << "[ Type: Keyword, Value: if ]";
      break;
    case token_then:
      os << "[ Type: Keyword, Value: then ]";
      break;
    case token_else:
      os << "[ Type: Keyword, Value: else ]";
      break;
    case token_for:
      os << "[ Type: Keyword, Value: for ]";
      break;
    case token_in:
      os << "[ Type: Keyword, Value: in ]";
      break;
    default:
      os << "[ Type: Unkown, Value: " << (char)token << " ]";
      break;
  }
}


} // Kaleidoscope
