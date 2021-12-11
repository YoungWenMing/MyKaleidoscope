#include "src/util.h"
#include "stdio.h"
#include <string>

namespace Kaleidoscope {

void PrintErrorF(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
}

void VPrintError(const char* format, va_list args) {
  vfprintf(stderr, format, args);
}

} // Kaleidoscope