#include "src/util.h"
#include "stdarg.h"
#include "stdio.h"

namespace Kaleidoscope {

void PrintErrorF(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
}

} // Kaleidoscope