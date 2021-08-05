#ifndef LEXER_INL_H
#define LEXER_INL_H
#include "src/lexer.h"
#include <unordered_map>


namespace Kaleidoscope {

typedef std::unordered_map<std::string, Token::Value> TokenMap;

#define KEYWORDS(S, G)              \
  G('b')                            \
  S("binary")                       \
  G('d')                            \
  S("def")                          \
  G('e')                            \
  S("extern")                       \
  S("else")                         \
  G('f')                            \
  S("for")                          \
  G('i')                            \
  S("if")                           \
  S("in")                           \
  G('r')                            \
  S("return")                       \
  G('u')                            \
  S("unary")                        \
  G('v')                            \
  S("var")


template<int N>
constexpr bool IsInString(const char(&s)[N], char c, size_t i = 0) {
  return i >= N? false : c == s[i]? true : IsInString(s, c, i + 1);
}

inline constexpr bool IsKeywordChar(char c) {
  return IsInString(
#define PICK_SINGLE_KEYWORD(str) str
#define PICK_GROUP_KEYWORD(str)
  KEYWORDS(PICK_SINGLE_KEYWORD, PICK_GROUP_KEYWORD)
#undef PICK_GROUP_KEYWORD
#undef PICK_SINGLE_KEYWORD
          , c);
}

// inline constexpr bool IsTerminate

inline constexpr bool IsKeywordCharStart(char c) {
  return 
#define PICK_SINGLE_KEYWORD(str)
#define PICK_GROUP_KEYWORD(ch) ch == c ||
  KEYWORDS(PICK_SINGLE_KEYWORD, PICK_GROUP_KEYWORD) false
#undef PICK_GROUP_KEYWORD
#undef PICK_SINGLE_KEYWORD
    ;
}

enum class LexFlags : uint8_t {
  kCanTerminate = 1 << 0,
  kCannotBeKeyword = 1 << 1,
  kCannotBeKeywordStart = 1 << 2,
  FlagsNum
};

enum {
  KEYWORD_MIN_LEN = 2,
  KEYWORD_MAX_LEN = 6,
};


inline constexpr bool IsAsciiIdentifier(char c) {
  return (c >= 'A' && c <= 'Z')
          || (c >= 'a' && c <= 'z')
          || (c >= '0' && c <= '9')
          || c == '_';
}

constexpr uint8_t GetLexFlags(char c) {
  return  // if c is not even an ascii identifier, keyword-checking is useless
          (IsAsciiIdentifier(c) && !IsKeywordChar(c) ?
            static_cast<uint8_t>(LexFlags::kCannotBeKeyword) : 0 ) |
          (IsKeywordCharStart(c) ?
            0 : static_cast<uint8_t>(LexFlags::kCannotBeKeywordStart)) |
          (!IsAsciiIdentifier(c) ?
            static_cast<uint8_t>(LexFlags::kCanTerminate) : 0);
}

#define INT_0_TO_127_LIST(V)                                          \
V(0)   V(1)   V(2)   V(3)   V(4)   V(5)   V(6)   V(7)   V(8)   V(9)   \
V(10)  V(11)  V(12)  V(13)  V(14)  V(15)  V(16)  V(17)  V(18)  V(19)  \
V(20)  V(21)  V(22)  V(23)  V(24)  V(25)  V(26)  V(27)  V(28)  V(29)  \
V(30)  V(31)  V(32)  V(33)  V(34)  V(35)  V(36)  V(37)  V(38)  V(39)  \
V(40)  V(41)  V(42)  V(43)  V(44)  V(45)  V(46)  V(47)  V(48)  V(49)  \
V(50)  V(51)  V(52)  V(53)  V(54)  V(55)  V(56)  V(57)  V(58)  V(59)  \
V(60)  V(61)  V(62)  V(63)  V(64)  V(65)  V(66)  V(67)  V(68)  V(69)  \
V(70)  V(71)  V(72)  V(73)  V(74)  V(75)  V(76)  V(77)  V(78)  V(79)  \
V(80)  V(81)  V(82)  V(83)  V(84)  V(85)  V(86)  V(87)  V(88)  V(89)  \
V(90)  V(91)  V(92)  V(93)  V(94)  V(95)  V(96)  V(97)  V(98)  V(99)  \
V(100) V(101) V(102) V(103) V(104) V(105) V(106) V(107) V(108) V(109) \
V(110) V(111) V(112) V(113) V(114) V(115) V(116) V(117) V(118) V(119) \
V(120) V(121) V(122) V(123) V(124) V(125) V(126) V(127)

static constexpr const uint8_t char_lex_flags[128] = {
#define COMPUTE_FLAG_LIST(N) GetLexFlags(N),
  INT_0_TO_127_LIST(COMPUTE_FLAG_LIST)
#undef COMPUTE_FLAG_LIST
};

#define KEYWORD_MAP_LIST(name, string, precedence)                    \
  {string, Token::name},
static const TokenMap keywords_map = {
  KEYWORD_LIST(KEYWORD_MAP_LIST)
};
#undef KEYWORD_MAP_LIST

bool Terminates(uint8_t flag) {
  return flag & (uint8_t)LexFlags::kCanTerminate;
}

bool CanBeKeyword(uint8_t flag) {
  return !(flag & (uint8_t)LexFlags::kCannotBeKeyword);
}

bool CanBeKeywordStart(uint8_t flag) {
  return !(flag & (uint8_t)LexFlags::kCannotBeKeywordStart);
}

Token::Value GetKeywordOrIdentifier(const std::string& id) {
  size_t len = id.size();
  if (len >= KEYWORD_MIN_LEN  && len <= KEYWORD_MAX_LEN) {
    auto entry = keywords_map.find(id);
    if (entry != keywords_map.end()) {
      return entry->second;
    }
  }
  return Token::IDENTIFIER;
}

template <typename FunctionType>
void Lexer::AdvanceUntil(FunctionType fn) {
  src_cursor_ = std::find_if(
    src_cursor_, src_end_, [&fn](char c){
      return fn(c);
  });
  if (src_cursor_ == src_end_)  c0_ = kEndOfSource;
  else {
    c0_ = *src_cursor_;
    ++src_cursor_;
  }
}

void Lexer::advance() {
  if (src_cursor_ == src_end_) {
    c0_ = kEndOfSource;
  } else {
    c0_ = *src_cursor_;
    ++src_cursor_;
  }
}

void Lexer::AddLiteralChar(char c) {
  curToken.literal_buffer.push_back(c);
}

void Lexer::AddLiteralCharAdvance() {
  AddLiteralChar(c0_);
  advance();
}

void Lexer::ResetDesc() {
  curToken.number_val = 0.0;
  curToken.literal_buffer.clear();
  curToken.value = Token::UNINITIALIZED;
}

Token::Value Lexer::ScanIdentifierOrKeyword() {
  uint8_t flags = char_lex_flags[c0_];
  AddLiteralChar(c0_);
  AdvanceUntil([this, &flags](char c){
    uint8_t char_flags = char_lex_flags[c];
    if (Terminates(char_flags))  return true;
    flags |= char_flags;
    AddLiteralChar(c);
    return false;
  });
  return !CanBeKeyword(flags) ? 
              Token::IDENTIFIER :
              GetKeywordOrIdentifier(curToken.literal_buffer);
}

}
#endif //LEXER_INL_H