#ifndef TOKEN_H
#define TOKEN_H

#include "src/global.h"

#include <string>

namespace Kaleidoscope {

#define KEYWORD_LIST(T)                           \
  T(DEF, "def", 0)                                \
  T(EXTERN, "extern", 0)                          \
  T(IF, "if", 0)                                  \
  T(THEN, "then", 0)                              \
  T(ELSE, "else", 0)                              \
  T(FOR, "for", 0)                                \
  T(IN, "in", 0)

#define BINARY_OP_LIST(T)                         \
  T(ADD, "+", 5)                                  \
  T(SUB, "-", 5)                                  \
  T(MUL, "*", 7)                                  \
  T(DIV, "/", 7)                                  \
  T(LT, "<", 3)                                   \
  T(GT, ">", 3)

#define TOKEN_LIST(T)                             \
  T(LPAREN, "(", 0)                               \
  T(RPAREN, ")", 0)                               \
  T(SEMICOLON, ";", 0)                            \
  T(COMMA, ",", 0)                                \
  T(ASSIGN, "=", 0)                               \
  T(NUMBER, nullptr, 0)                           \
  T(IDENTIFIER, nullptr, 0)                       \
  KEYWORD_LIST(T)                                 \
  BINARY_OP_LIST(T)                               \
  T(UNINITIALIZED, nullptr, 0)                    \
  T(ILLEGAL, "ILLEGAL", 0)                        \
  T(EOS, "EOS", 0)

class Token {
 public:
  enum Value : uint8_t {
#define VALUE_LIST(name, str, precedence) name,
  TOKEN_LIST(VALUE_LIST) TOKEN_NUMS
#undef VALUE_LIST
  };

  static int Precedence(Value val) {
    return precedence_[val];
  }

 private:
  static const uint8_t precedence_[TOKEN_NUMS];
};



} // namespace Kaleidoscope

#endif // TOKEN_H
