#ifndef SCRIPT_SRC_H
#define SCRIPT_SRC_H

#include <vector>


namespace Kaleidoscope {

struct Location {
  Location(int l, int c)
    : line(l), col(c) {}

  int line;
  int col;
};

class Script {
 public:
  Script(const char* src, int length)
    : src_(src), length_(length) {
    CalculateLineEnds();
  }
  virtual ~Script() = default;

  const char* source() const { return src_; }
  const int   length() const { return length_; }
  const std::vector<int> &line_ends() const {
    return line_ends_;
  }
  Location get_location(int code_pos) const;

 private:
  void CalculateLineEnds() {
    const char *cursor = src_, *end = src_ + length_;
    line_ends_.push_back(0);
    while (cursor != end) {
      if (cursor[0] == '\n') line_ends_.push_back(cursor - src_);
      cursor++;
    }
  }

  const char* src_;
  int length_;
  std::vector<int> line_ends_;
};


} // Kaleidoscope 
#endif // SCRIPT_SRC_H