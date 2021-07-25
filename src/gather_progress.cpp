#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include "gather_progress.hpp"

void gather_progress_render(int processed_events, int total_events) {
    std::stringstream strs;
    strs << "DATA GATHERING! " << "processed events: " << processed_events << ", total events: " << total_events;
    std::string temp_str = strs.str();
    char const* char_type = temp_str.c_str();
    REprintf("\r");
    REprintf("%s", char_type);
    REprintf("\r");
    strs.clear();
    return;
}

void gather_progress_terminate() {
    std::stringstream strs;
    strs << "                                                                                                 ";
    std::string temp_str = strs.str();
    char const* char_type = temp_str.c_str();
    REprintf("\r");
    REprintf("%s", char_type);
    REprintf("\r");
    strs.clear();
    return;
}
