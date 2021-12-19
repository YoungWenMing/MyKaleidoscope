#ifndef TOKEN_INL_H
#define TOKEN_INL_H

#include "src/token.h"
#include "src/util.h"

namespace Kaleidoscope {

constexpr bool Token::IsUnaryOp(Token::Value val) {
  return IsInRange(val, Token::ADD, Token::NOT);
}

constexpr bool Token::IsParamType(Token::Value val) {
  return IsInRange(val, Token::INT, Token::DOUBLE);
}

constexpr bool Token::IsType(Token::Value val) {
  return IsInRange(val, Token::INT, Token::VOID);
}

constexpr bool Token::IsProperty(Token::Value val) {
  return IsInRange(val, Token::PERIOD, Token::LBRACK);
}

constexpr bool Token::IsPropertyOrCall(Token::Value val) {
  return IsInRange(val, Token::PERIOD, Token::LBRACK);
}
 
constexpr bool Token::IsCount(Token::Value val) {
  return IsInRange(val, Token::INC, Token::DEC);
}

constexpr bool Token::IsUnaryOrCountOp(Token::Value val) {
  return IsUnaryOp(val) || IsCount(val);
}

constexpr bool Token::IsAssignmentOp(Token::Value val) {
  return IsInRange(val, Token::ASSIGN, Token::ASSIGN_SUB);
}

} // Kaleidoscope 
#endif // TOKEN_INL_H