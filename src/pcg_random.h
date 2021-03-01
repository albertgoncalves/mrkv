#ifndef __PCG_RANDOM_H__
#define __PCG_RANDOM_H__

#include <stdint.h>

typedef uint32_t u32;
typedef uint64_t u64;

void set_seed(u64, u64);

u32 get_random_bounded_u32(u32);

#endif
