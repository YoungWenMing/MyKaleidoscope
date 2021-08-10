#ifndef GLOBAL_H
#define GLOBAL_H

#include <iostream>
#include <stdlib.h>

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;

void FATAL(const char* info);

#define UNREACHABLE() FATAL("unreachable code.")

#if DEBUG
#define DCHECK(condition)   assert(condition)
#else
#define DCHECK(condition)   ((void)0)
#endif

#endif // GLOBAL_H