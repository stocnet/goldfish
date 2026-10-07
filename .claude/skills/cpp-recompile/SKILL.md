---
name: cpp-recompile
description: Regenerate the Rcpp R<->C++ bindings and force a fresh recompile after editing goldfish's src/ C++ (Rcpp/RcppArmadillo). Runs Rcpp::compileAttributes(), shows the RcppExports interface diff to verify a signature/arity change, then a clean, optimized (-O2) rebuild through devtools::load_all(recompile = TRUE, debug = FALSE) so tests never run against a stale or unoptimized .o/.so. Use after ANY change to src/*.cpp / src/*.h, before testing, and before timing or benchmarking a compiled backend.
---

# cpp-recompile

Any change under `src/` (Rcpp/RcppArmadillo) needs two distinct things before you
test: the R↔C++ **binding** must be regenerated if an exported signature changed,
and the compiled **objects** (`.o`/`.so`) must be rebuilt. Skipping either means
the coefficient and C++ golden baselines can silently pass on the wrong thing — a
mismatched interface (wrong arity) or a stale `.so`.

## Do document() / load_all(recompile = TRUE) already do this?

Mostly yes — and that is worth understanding so this skill isn't cargo-culting:

- `pkgbuild::compile_dll()` (which **both** `devtools::load_all()` and
  `devtools::document()` call) runs `Rcpp::compileAttributes()` by default for a
  package that `LinkingTo: Rcpp` — goldfish does. So `load_all(recompile = TRUE)`
  and `document()` *do* regenerate `R/RcppExports.R` / `src/RcppExports.cpp` for
  you, then build.
- So the explicit `Rcpp::compileAttributes()` here is **not** because the others
  skip it. It is for **verification and hygiene**:
  1. It regenerates the bindings as a **separate, inspectable step**, so you can
     `git diff` `RcppExports.*` and *see* the interface delta — confirm an
     intended change (e.g. "arity 17→18" when you add a C++ arg) and catch an
     unintended one *before* compiling and testing.
  2. `RcppExports.R` / `RcppExports.cpp` are **generated but committed** files; an
     explicit regen makes sure they are current and staged, rather than trusting a
     `load_all` side effect you might not notice.
  3. It keeps the two failure modes separable when debugging: an **arity /
     "unused arguments" / wrong-`.Call`** error ⇒ bindings not regenerated;
     a result that ignores your C++ change ⇒ `.so` not rebuilt.

`compileAttributes()` only writes files (codegen); it never compiles.
`load_all(recompile = TRUE)` forces the actual build **and** reloads the running
session. You want both.

## Run

From the package root:

```bash
# 1. Regenerate the R<->C++ bindings from the current [[Rcpp::export]] signatures.
Rscript -e 'Rcpp::compileAttributes(".")'

# 2. Inspect the interface delta — the reason for the explicit call.
git diff --stat -- R/RcppExports.R src/RcppExports.cpp
git diff -- R/RcppExports.R src/RcppExports.cpp
```

Read the diff before continuing:

- **No diff** → no exported signature changed (you only touched a body/internal
  helper). Fine — proceed to the rebuild.
- **A diff you expected** (a new/changed `[[Rcpp::export]]` arg, an arity bump) →
  good, the binding now matches the C++. These files are committed with the change.
- **A diff you did NOT expect** → you changed an exported signature without meaning
  to. Fix the C++ before going further.

```bash
# 3. Force a fresh, OPTIMIZED build + reload the session (re-runs
#    compileAttributes internally — harmless; step 1 already gave you the diff).
PKG_CPPFLAGS="-O2" Rscript -e '
  pkgbuild::clean_dll(".")
  suppressMessages(devtools::load_all(".", recompile = TRUE, debug = FALSE))
  cat("recompiled + reloaded OK\n")'

# 4. Confirm the optimization level reached the compiler. No output here
#    means the objects carry debug info, i.e. a debug (-O0) build.
dwarfdump src/REM_default.o 2>/dev/null | grep -m1 DW_AT_producer ||
  echo "optimized build (no debug info)"
```

## Why the build is forced to `-O2`

`devtools::load_all()` does not build the way `R CMD INSTALL` does. It goes
through `pkgbuild::compile_dll()`, which **replaces** R's compiler flags
(`CXX17FLAGS = -g -O2`) with its own:

| `debug` | Flags pkgbuild sets | Optimization |
|---|---|---|
| `TRUE` (the default) | `-UNDEBUG -Wall -pedantic -g -O0` | none, explicitly |
| `FALSE` | `-Wall -pedantic` | none, by omission |

So `debug = FALSE` alone is not enough: it removes `-O0` and puts nothing in
its place, and the compiler's own default is `-O0`. `PKG_CPPFLAGS` is the one
variable neither pkgbuild nor the package's `Makevars` sets, so an `-O2` given
there reaches the compile line. All three pieces are needed:

- `PKG_CPPFLAGS="-O2"` supplies the optimization level.
- `debug = FALSE` keeps pkgbuild from appending `-O0`, which would win by
  coming later on the command line.
- `pkgbuild::clean_dll()` removes the old objects. `recompile = TRUE` alone
  rebuilt only the files `make` considered out of date, leaving the rest at
  their previous optimization level.

The flag can also live in a personal `~/.R/Makevars` as `CPPFLAGS += -O2`,
which makes the environment variable unnecessary on that machine (verified
2026-09-30: `-DNDEBUG -O2` on the compile line with `debug = FALSE` and no
variable set). `CPPFLAGS` is the right name there: it is the user's own
variable, where `PKG_CPPFLAGS` belongs to the package being built. Write
`+=`, never `=`, so the line adds to what the file already sets. It replaces
only the first of the three pieces. `debug = FALSE` and `clean_dll()` are
still required, because with `debug = TRUE` pkgbuild's `-O0` comes later on
the command line and wins. The command above uses `PKG_CPPFLAGS` from the
environment instead, because a `~/.R/Makevars` that assigns `CPPFLAGS`
overrides a `CPPFLAGS` environment variable, and it works on a machine
without the line. cmdstan is untouched by either: it builds through its own
`make/local`.

Measured on one REM fit (Social_Evolution, 84 actors, 439 intervals,
`~ 1 + inertia + recip`), identical log-likelihood on both builds:

| Backend | Unoptimized | `-O2` |
|---|---|---|
| `cpp` | 1.47 s | 0.80 s |
| `gather` | 9.45 s | 3.80 s |
| `r` | 2.35 s | 2.38 s |

Unoptimized, the `gather` backend is slower than the R engine. A timing or a
benchmark taken on such a build says nothing about the compiled backends.

Two things follow:

- **A later plain `load_all()` keeps the optimized objects** as long as no
  file under `src/` changed. After a `src/` edit it rebuilds only the changed
  files, unoptimized, and the build becomes mixed. Run this skill again
  instead.
- **For a debugger or AddressSanitizer session you want the opposite**: run
  `devtools::load_all(".", recompile = TRUE)` after `pkgbuild::clean_dll(".")`
  to get `-g -O0`, and rebuild with this skill before timing anything.

## Verify

A recompile is only trusted once the compiled tests confirm it against a fresh
build — otherwise you may be reading a cached `.so`:

- Targeted: run the affected file, e.g.
  `NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-cpp_interface.R")'`.
- Full floor before a commit: use the **not-cran-test** skill and confirm the
  coefficient / C++ golden baselines are **PASS, not SKIP** (a stale-`.so` run
  can pass the golden tests on old objects — that is exactly what this guards).

## Never

- Never hand-edit `R/RcppExports.R` or `src/RcppExports.cpp` — they are generated.
  A PreToolUse hook blocks editing them; regenerate via step 1 instead (the hook
  only blocks the Edit/Write tools, so `compileAttributes()` run through the shell
  is unaffected).
- Never test after a `src/` change without recompiling — the C++ golden and
  coefficient baselines will silently pass on the old `.o`/`.so`.
- Never record a timing of a compiled backend without checking the build's
  optimization level first (step 4).
