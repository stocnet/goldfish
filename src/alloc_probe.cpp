#include <Rcpp.h>
#include <cstdlib>
#include "alloc_probe.h"

// Counts the Armadillo heap allocations of at least `threshold` bytes made
// while counting is on. Only the test build routes Armadillo here (see
// alloc_probe.h); elsewhere the counter stays at zero and
// `alloc_probe_enabled()` says so, so the test that reads it can skip.
namespace {
bool counting = false;
std::size_t threshold = 0;
double n_counted = 0;
}

extern "C" void* goldfish_counted_malloc(std::size_t bytes) {
  if (counting && bytes >= threshold) n_counted += 1;
  return std::malloc(bytes);
}

extern "C" void goldfish_counted_free(void* memory) {
  std::free(memory);
}

//' Allocation probe for the kernel cost guard
//'
//' Test-only. `alloc_probe_enabled()` is TRUE in the build that routes
//' Armadillo's allocations through the counter; `alloc_probe_start()` resets
//' it and counts allocations of at least `threshold_bytes` until
//' `alloc_probe_stop()`, which returns the count.
//' @noRd
// [[Rcpp::export]]
bool alloc_probe_enabled() {
#if defined(ARMA_ALIEN_MEM_ALLOC_FUNCTION)
  return true;
#else
  return false;
#endif
}

//' @noRd
// [[Rcpp::export]]
void alloc_probe_start(double threshold_bytes) {
  threshold = static_cast<std::size_t>(threshold_bytes);
  n_counted = 0;
  counting = true;
}

//' @noRd
// [[Rcpp::export]]
double alloc_probe_stop() {
  counting = false;
  return n_counted;
}
