#include "src/global.h"

void FATAL(const char* info) {
  std::cerr << info << std::endl;
  abort();
}
