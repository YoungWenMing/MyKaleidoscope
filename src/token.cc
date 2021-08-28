#include "src/token.h"

namespace Kaleidoscope {

const uint8_t Token::precedence_[Token::TOKEN_NUMS] = {
#define PICK_TOKEN_PRECEDENCE(name, string, precedence) precedence,
  TOKEN_LIST(PICK_TOKEN_PRECEDENCE)
#undef PICK_TOKEN_PRECEDENCE
};

const char* const Token::name_[Token::TOKEN_NUMS] = {
#define PICK_TOKEN_STRING(name, string, precedence) #name,
  TOKEN_LIST(PICK_TOKEN_STRING)
#undef PICK_TOKEN_STRING
};

} // namespace Kaleidoscope
