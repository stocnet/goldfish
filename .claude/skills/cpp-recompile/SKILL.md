---
name: cpp-recompile
description: Regenerate the Rcpp R<->C++ bindings and force a fresh recompile after editing goldfish's src/ C++ (Rcpp/RcppArmadillo). Runs Rcpp::compileAttributes(), shows the RcppExports interface diff to verify a signature/arity change, then devtools::load_all(recompile = TRUE) so tests never run against a stale .o/.so. Use after ANY change to src/*.cpp / src/*.h, before testing.
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
# 3. Force a fresh build + reload the session (re-runs compileAttributes
#    internally — harmless; step 1 already gave you the diff).
Rscript -e 'suppressMessages(devtools::load_all(".", recompile = TRUE)); cat("recompiled + reloaded OK\n")'
```

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
