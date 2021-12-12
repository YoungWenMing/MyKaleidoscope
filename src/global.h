#ifndef GLOBAL_H
#define GLOBAL_H

#include <iostream>
#include <stdlib.h>

typedef unsigned char   uint8_t;
typedef unsigned short  uint16_t;
typedef unsigned int    uint32_t;

void FATAL(const char* info);

#define UNREACHABLE() FATAL("unreachable code.")
#define UNIMPLEMENTED() FATAL("unimplemented logic.")

#if DEBUG
#define DCHECK(condition)   assert(condition)
#else
#define DCHECK(condition)   ((void)0)
#endif
#define CHECK(condition)    assert(condition)

static const uint32_t SMI_MAX_VALUE = 0x7fffffff;

#endif // GLOBAL_H