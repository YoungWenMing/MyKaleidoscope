#include "src/token.h"

namespace Kaleidoscope {

const uint8_t Token::precedence_[Token::TOKEN_NUMS] = {
#define PICK_TOKEN_PRECEDENCE(name, string, precedence) precedence,
  TOKEN_LIST(PICK_TOKEN_PRECEDENCE)
#undef PICK_TOKEN_PRECEDENCE
};

} // namespace Kaleidoscope
