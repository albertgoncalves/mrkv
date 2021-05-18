#include "pcg_random.h"

#include <math.h>

typedef uint32_t u32;

typedef struct {
    u64 state;
    u64 increment;
} PcgRng;

static PcgRng RNG = {
    .state = 9600629759793949339llu,
    .increment = 15726070495360670683llu,
};

static u32 get_random_u32(void) {
    const u64 state = RNG.state;
    RNG.state = (state * 6364136223846793005llu) + (RNG.increment | 1u);
    const u32 xor_shift = (u32)(((state >> 18u) ^ state) >> 27u);
    const u32 rotate = (u32)(state >> 59u);
    return (xor_shift >> rotate) | (xor_shift << ((-rotate) & 31u));
}

void set_seed(u64 state, u64 increment) {
    RNG.state = 0u;
    RNG.increment = (increment << 1u) | 1u;
    get_random_u32();
    RNG.state += state;
    get_random_u32();
}

u32 get_random_bounded_u32(u32 bound) {
    u32 threshold = (-bound) % bound;
    for (;;) {
        u32 r = get_random_u32();
        if (threshold < r) {
            return r % bound;
        }
    }
}
