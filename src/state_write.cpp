#include <Rcpp.h>

// Write cells of a state adjacency matrix IN PLACE, bypassing R's
// copy-on-modify.
//
// Why this exists. The preprocessing walk hands the state's adjacency matrix to
// the effect update closures as an argument, which binds it to a second name
// and marks it shared. The state write that follows,
// `state$networks[[key]][sender, receiver] <- replace`, therefore duplicates
// the whole matrix, and the call-then-write cycle repeats every event. On a
// 300-actor fixture that copy is 98 percent of the walk's allocation; at 1899
// actors it is 27.5 MB per event. Inlining does not help, because the sharing
// comes from the closure call rather than from where the write is written.
//
// Why it is safe HERE and nowhere by default. An in-place write is correct
// exactly when nothing else holds a live reference to this matrix expecting it
// not to change. The three places in goldfish that held one were removed first,
// each with its own regression test: `ds_network()` handing back the cached
// imputation override, `init_DyNAM_choice.four()` returning the state matrix as
// its own edgeless cache, and `init_DyNAM_choice.tie()` passing the state
// through an identity transformer on a matrix with no dimnames. The ordinary
// path materializes a fresh matrix per state container, and every effect cache
// is a computed value that allocates. Do not call this on a matrix that reached
// the walk from anywhere else.
//
// The caller guarantees the matrix is REALSXP; a non-double matrix takes the
// ordinary R subassignment instead, so this aborts rather than silently
// reinterpreting the storage.
//
// The same reasoning covers the kind-shaped buffers the preprocessing walk
// maintains -- interaction operands, constraint atoms, the support mask --
// which are vectors and scalars as often as matrices, and logical as often as
// double.
// `set_entries()` is that writer: one linear index instead of a (row, col)
// pair, so it serves every shape, and REALSXP or LGLSXP, so it serves the mask.
// Its aliasing precondition is the same one and is met the same way: each
// buffer is materialized fresh at seeding and lives in exactly one environment
// binding.

namespace {

// Bounds-check one 1-based linear index against a buffer length, aborting with
// the caller's name rather than writing past the end.
inline R_xlen_t checked_offset(int index, R_xlen_t length, const char* who) {
  if (index < 1 || static_cast<R_xlen_t>(index) > length) {
    Rcpp::stop("%s() index out of bounds.", who);
  }
  return static_cast<R_xlen_t>(index) - 1;
}

}  // namespace

// Write entries of a kind-shaped buffer IN PLACE, addressed by 1-based linear
// index. Double and logical buffers only, and `values` must already be the
// buffer's own type: coercing here would allocate the copy this exists to
// avoid, and silently changing a buffer's storage type is worse than aborting.
//
// [[Rcpp::export]]
void set_entries(SEXP buffer, Rcpp::IntegerVector at, SEXP values) {
  const int type = TYPEOF(buffer);
  if (type != REALSXP && type != LGLSXP) {
    Rcpp::stop("set_entries() needs a double or logical buffer.");
  }
  if (TYPEOF(values) != type) {
    Rcpp::stop("set_entries() needs values of the buffer's own type.");
  }
  const R_xlen_t n = at.size();
  if (XLENGTH(values) != n) {
    Rcpp::stop("set_entries() needs `at` and `values` of one length.");
  }
  const R_xlen_t length = XLENGTH(buffer);
  if (type == REALSXP) {
    double* data = REAL(buffer);
    const double* from = REAL(values);
    for (R_xlen_t k = 0; k < n; ++k) {
      data[checked_offset(at[k], length, "set_entries")] = from[k];
    }
    return;
  }
  int* data = LOGICAL(buffer);
  const int* from = LOGICAL(values);
  for (R_xlen_t k = 0; k < n; ++k) {
    data[checked_offset(at[k], length, "set_entries")] = from[k];
  }
}

// [[Rcpp::export]]
void set_matrix_cells(
    SEXP matrix,
    Rcpp::IntegerVector rows,
    Rcpp::IntegerVector cols,
    Rcpp::NumericVector values
) {
  if (TYPEOF(matrix) != REALSXP) {
    Rcpp::stop("set_matrix_cells() needs a double matrix.");
  }
  SEXP dim = Rf_getAttrib(matrix, R_DimSymbol);
  if (dim == R_NilValue || LENGTH(dim) != 2) {
    Rcpp::stop("set_matrix_cells() needs a two-dimensional matrix.");
  }
  const int n_rows = INTEGER(dim)[0];
  const int n_cols = INTEGER(dim)[1];
  const R_xlen_t n = rows.size();
  if (cols.size() != n || values.size() != n) {
    Rcpp::stop("set_matrix_cells() needs rows, cols and values of one length.");
  }
  double* data = REAL(matrix);
  for (R_xlen_t k = 0; k < n; ++k) {
    // One-based on the R side, as a subassignment would be.
    const int i = rows[k];
    const int j = cols[k];
    if (i < 1 || i > n_rows || j < 1 || j > n_cols) {
      Rcpp::stop("set_matrix_cells() index out of bounds.");
    }
    data[static_cast<R_xlen_t>(j - 1) * n_rows + (i - 1)] = values[k];
  }
}
