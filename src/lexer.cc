#include "src/lexer.h"
#include <cctype>
#include <cstdio>
#include <cstdlib>


namespace Kaleidoscope {

Lexer::Lexer(const char* src)
	: source_(src),
		src_cursor_(src),
		Identifier(""),
		NumVal(0),
    curChar(' '),
		cur_tok(0) {}

void Lexer::Reinitialize(const char* src) {
  source_ = src_cursor_ = src;
  NumVal = 0;
  curChar = ' ';
  cur_tok = 0;
}

int Lexer::next_token() {
  // advance();
  // curChar = ' ';

  while (isspace(curChar)) {
    advance();
  }

  // for number parsing
  if (isdigit(curChar)) {
    std::string NumberStr;

    // TODO: consider multiple dot in string like '1.23.456'
    do {
      NumberStr.push_back(curChar);
      advance();
    } while (isdigit(curChar) || curChar == '.');

    NumVal = strtod(NumberStr.c_str(), NULL);

    return token_number;
  }

  // for identifier parsing
  if (isalpha(curChar) || curChar == '_') {
    Identifier.clear();
    do {
      Identifier.push_back(curChar);
      advance();
    } while (isalnum(curChar) || curChar == '_');

    if (Identifier == "def")
      return token_def;
    if (Identifier == "extern")
      return token_extern;
    return token_identifier;
  }
    

  // single-line comment
  if (curChar == '#') {
    while (curChar != '\n' && curChar != '\r' && curChar != '\0') {
      advance();
    }

    if (curChar != '\0') 
      return next_token();
  }

  if (curChar == '\0') return token_eof;

  // for those charactor we do not recognize yet
  int t = curChar;
  advance();
  return t;
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
    default:
      os << "[ Type: Unkown, Value: " << (char)token << " ]";
      break;
  }
}


} // Kaleidoscope
