#ifndef TOKEN_H
#define TOKEN_H

#include "src/global.h"

#include <string>

namespace Kaleidoscope {

#define TYPE_KEYWORD_LITE(T)                      \
  T(INT,        "int",      0)                    \
  T(DOUBLE,     "double",   0)                    \
  T(VOID,       "void",     0)

#define KEYWORD_LIST(T)                           \
  TYPE_KEYWORD_LITE(T)                            \
  T(DEF,        "def",      0)                    \
  T(EXTERN,     "extern",   0)                    \
  T(IF,         "if",       0)                    \
  T(ELSE,       "else",     0)                    \
  T(FOR,        "for",      0)                    \
  T(IN,         "in",       0)                    \
  T(UNARY,      "unary",    0)                    \
  T(BINARY,     "binary",   0)                    \
  T(VAR,        "var",      0)                    \
  T(RETURN,     "return",   0)

#define BINARY_OP_LIST(T)                         \
  T(MUL,        "*",        7)                    \
  T(DIV,        "/",        7)                    \
  T(LT,         "<",        3)                    \
  T(GT,         ">",        3)                    \
  T(OR,         "|",        2)                    \
  T(AND,        "&",        2)                    \
  T(ADD,        "+",        5)                    \
  T(SUB,        "-",        5)
 
#define UNARY_OP_LIST(T)                          \
  T(NOT,        "!",        0)

#define TOKEN_LIST(T)                             \
  T(LPAREN,     "(",        0)                    \
  T(RPAREN,     ")",        0)                    \
  T(LBRACE,     "{",        0)                    \
  T(RBRACE,     "}",        0)                    \
  T(COLON,      ":",        0)                    \
  T(SEMICOLON,  ";",        0)                    \
  T(COMMA,      ",",        0)                    \
  T(PERIOD,     ".",        0)                    \
  T(ASSIGN,     "=",        1)                    \
  T(SMI,        nullptr,    0)                    \
  T(NUMBER,     nullptr,    0)                    \
  T(IDENTIFIER, nullptr,    0)                    \
  KEYWORD_LIST(T)                                 \
  BINARY_OP_LIST(T)                               \
  UNARY_OP_LIST(T)                                \
  T(UNINITIALIZED, nullptr, 0)                    \
  T(ILLEGAL,    "ILLEGAL",  0)                    \
  T(EOS,        "EOS",      0)

class Token {
 public:
  enum Value : uint8_t {
#define VALUE_LIST(name, str, precedence) name,
  TOKEN_LIST(VALUE_LIST) TOKEN_NUMS, TOKEN_TYPES_NUM = DEF
#undef VALUE_LIST
  };

  static int Precedence(Value val) {
    return precedence_[val];
  }

  static const char* TokenName(Value val) {
    return name_[val];
  }

  static inline constexpr bool IsUnaryOp(Token::Value val);
  static inline constexpr bool IsParamType(Token::Value val);
  static inline constexpr bool IsType(Token::Value val);

 private:
  static const uint8_t precedence_[TOKEN_NUMS];
  static const char* const name_[TOKEN_NUMS];
};



} // namespace Kaleidoscope

#endif // TOKEN_H
