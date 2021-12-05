#ifndef UTIL_H
#define UTIL_H

#include <type_traits>
#include <assert.h>
#include "stdarg.h"

namespace Kaleidoscope {

template<typename T, typename U>
inline constexpr bool IsInRange(T val, U low_limit, U high_limit) {
  assert(sizeof(U) <= sizeof(T));
  typedef typename std::make_unsigned<T>::type unsigned_T;
  return static_cast<unsigned_T>(static_cast<unsigned_T>(val) -
                                 static_cast<unsigned_T>(low_limit)) <=
         static_cast<unsigned_T>(static_cast<unsigned_T>(high_limit) -
                                 static_cast<unsigned_T>(low_limit));
}

void PrintErrorF(const char* format, ...);
void VPrintError(const char* format, va_list args);
}
#endif