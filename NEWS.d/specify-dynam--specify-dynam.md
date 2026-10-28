* Added `specify_dynam()`, proposed as the successor of `make_specification()`
  * Reads a formula written for goldfish, an MRQAP in `{infernet}`, or `{ergm}`
  * Rewrites each term as its goldfish effect, e.g. `nodematch("x")` as `same(x)`
  * Drops and names a term goldfish cannot fit, so the rest still fits
  * With `.data`, returns the `goldfishSpec` that `make_specification()` returns
  * Without a formula, gives a table of every effect for the network
  * `formula()` of chosen rows gives their specification
* Added `tidy()` for a `goldfishSpec`, one row for each term and its status
