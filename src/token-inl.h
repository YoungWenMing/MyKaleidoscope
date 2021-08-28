#ifndef TOKEN_INL_H
#define TOKEN_INL_H

#include "src/token.h"
#include "src/util.h"

namespace Kaleidoscope {

constexpr bool Token::IsUnaryOp(Token::Value val) {
  return IsInRange(val, Token::ADD, Token::NOT);
}

} // Kaleidoscope 
#endif // TOKEN_INL_H