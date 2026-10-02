#ifndef GOLDFISH_ALLOC_PROBE_H
#define GOLDFISH_ALLOC_PROBE_H

#include <stddef.h>

// Armadillo's heap allocations routed through a counter, for the test build
// that checks the likelihood kernels allocate nothing event-sized per event.
// That build force-includes this header and points Armadillo at the two
// functions (ARMA_ALIEN_MEM_ALLOC_FUNCTION / ARMA_ALIEN_MEM_FREE_FUNCTION);
// an ordinary build defines them and never calls them.
// The probe build force-includes this header in every translation unit,
// C ones too (the toolchain check compiles one), so it stays valid C.
#ifdef __cplusplus
extern "C" {
#endif
void* goldfish_counted_malloc(size_t bytes);
void goldfish_counted_free(void* memory);
#ifdef __cplusplus
}
#endif

#endif
