#!/usr/bin/env bash
# Mechanical hygiene checks on a landed commit range, in the order every
# conformance report has run them by hand. Read-only; prints, never edits.
#
#   bash hygiene-check.sh <base> <head> [<change-name>]
#
# <base> is the commit BEFORE the change's first commit (the branch point or
# the predecessor's landing commit); <head> is the last commit in scope.
# With a change name it also runs the OpenSpec placement pre-flight and
# validate, counts task ticks, and lists task notes that record a deferral.
#
# Everything reported here is "introduced by the range": pre-existing debt in
# the same files is not the change's gap (note it with an owner instead).

set -u
BASE=${1:?base commit}
HEAD_=${2:?head commit}
CHANGE=${3:-}
RANGE="$BASE..$HEAD_"

say() { printf '\n## %s\n' "$1"; }

# Added lines of the range with file:line, for R, src and tests. Format:
# <file>\t<line>\t<text>
added_lines() {
  git diff -U0 --no-color "$RANGE" -- R/ src/ tests/testthat/ \
    | awk '
      /^\+\+\+ / { f = substr($0, 5); sub(/^b\//, "", f); next }
      /^@@/      { match($0, /\+[0-9]+/); n = substr($0, RSTART + 1, RLENGTH - 1) + 0; next }
      /^\+/      { if (f != "/dev/null") printf "%s\t%d\t%s\n", f, n, substr($0, 2); n++; next }
    '
}
ADDED=$(added_lines)

say "1. Added lines over 80 characters (R/, src/, tests/; generated files excluded)"
printf '%s\n' "$ADDED" \
  | grep -v $'^R/RcppExports.R\t' | grep -v $'^src/RcppExports.cpp\t' | grep -v $'^tests/testthat/_snaps/' \
  | awk -F'\t' 'length($3) > 80 { printf "%s:%s (%d) %s\n", $1, $2, length($3), substr($3, 1, 60) "..." }' \
  | { grep . || echo "none"; }
echo "(test_that() titles and string literals cannot be reflowed by air; code lines can.)"

say "2. OpenSpec / ADR references in added comment lines (R/, src/, tests/)"
# Decision ids, task numbers (this change's or any other change's: a
# kebab-case change name followed by a task number), ADR ids, artifact file
# names, and every change directory name under openspec/changes/.
PAT='\bD[0-9]+[a-z]?\b|\btask [0-9]+(\.[0-9]+)?[a-z]?|[a-z]+(-[a-z]+)+ (task )?[0-9]+\.[0-9]+[a-z]?\b|ADR-[0-9]+|design\.md|proposal\.md|spec\.md|tasks\.md|progress\.md|openspec/'
if [ -n "$CHANGE" ]; then PAT="$PAT|$CHANGE"; fi
NAMES=$(ls -d openspec/changes/*/ openspec/changes/archive/*/ 2>/dev/null | sed -E 's#.*/([^/]+)/$#\1#; s#^[0-9-]{11}##' | grep -vE '^(_claims|archive)$' | sort -u | paste -sd'|' -)
if [ -n "$NAMES" ]; then PAT="$PAT|\b($NAMES)\b"; fi
printf '%s\n' "$ADDED" \
  | awk -F'\t' '$3 ~ /^[[:space:]]*(#|\/\/|\*)/ || $3 ~ /(#|\/\/)[^"]*$/' \
  | grep -E "$PAT" \
  | awk -F'\t' '{ printf "%s:%s %s\n", $1, $2, $3 }' \
  | { grep . || echo "none"; }
echo "-- the same references in added documentation lines (README, *.md under tests/ and inst/):"
git diff -U0 --no-color "$RANGE" -- 'tests/**/*.md' 'tests/**/README*' 'inst/**/*.md' 'inst/**/README*' \
  | awk '
      /^\+\+\+ / { f = substr($0, 5); sub(/^b\//, "", f); next }
      /^@@/      { match($0, /\+[0-9]+/); n = substr($0, RSTART + 1, RLENGTH - 1) + 0; next }
      /^\+/      { if (f != "/dev/null") printf "%s:%d %s\n", f, n, substr($0, 2); n++; next }
    ' | grep -E "$PAT" | { grep . || echo "none"; }

say "3. camelCase candidates among identifiers the range introduced (R/, tests/)"
echo "(assignments and formals on added lines. A pre-existing camelCase internal that an added line merely re-touches is legacy, not introduced; check git blame. .lintr allowlist: goldfish|DyNAM|REM|^[A-Z][A-Z0-9_]*\$)"
printf '%s\n' "$ADDED" \
  | grep -v $'^R/RcppExports.R\t' \
  | awk -F'\t' '$1 ~ /\.[Rr]$/ {
      line = $3
      # assignments: name <- / name =  (not ==), and formals name =
      while (match(line, /[A-Za-z_.][A-Za-z0-9_.]*[[:space:]]*(<-|=[^=])/)) {
        tok = substr(line, RSTART, RLENGTH); sub(/[[:space:]]*(<-|=.?)$/, "", tok)
        if (tok ~ /[a-z][A-Z]/ && tok !~ /goldfish|DyNAM|REM/ && tok !~ /^[A-Z][A-Z0-9_]*$/ && tok !~ /^(stringsAsFactors|check\.names|USE\.NAMES|SIMPLIFY|MoreArgs|row\.names|useNames|keepNA|allowNA|fixed|perl|ignore\.case|na\.rm|drop|envir|inherits|simplify)$/) printf "%s:%s %s\n", $1, $2, tok
        line = substr(line, RSTART + RLENGTH)
      }
    }' | sort -u | { grep . || echo "none"; }

say "4a. Frozen 1e-6 baselines touched in the range (must be empty; a move happens only on the trunk under ADR-0021)"
git diff --stat "$RANGE" -- tests/testthat/_baselines | { grep . || echo "none (byte-identical)"; }

say "4b. Snapshots, fixtures and the cost budget touched in the range"
echo "(an ADDED file is a new reference; a MODIFIED one is a moved number or output that needs a stated reason in the commit or progress.md)"
git diff --name-status "$RANGE" -- tests/testthat/_snaps tests/testthat/_fixtures tests/testthat/_budgets | { grep . || echo "none"; }

say "5. Compiled core and its generated bindings"
SRC_CHANGED=$(git diff --name-only "$RANGE" -- src/ | grep -v RcppExports || true)
EXP_CHANGED=$(git diff --name-only "$RANGE" -- R/RcppExports.R src/RcppExports.cpp || true)
if [ -z "$SRC_CHANGED" ]; then echo "src/ unchanged"; else printf 'src/ changed:\n%s\n' "$SRC_CHANGED"; fi
if [ -z "$EXP_CHANGED" ]; then echo "RcppExports.* unchanged"; else printf 'RcppExports.* changed:\n%s\n' "$EXP_CHANGED"; fi
if [ -n "$SRC_CHANGED" ]; then
  echo "-> confirm Rcpp::compileAttributes() on HEAD produces no further delta (cpp-recompile skill) and that a signature change shows in the RcppExports diff."
fi
if git diff "$RANGE" -- src/ | grep -E '^\+.*(Rcpp::export|\[\[Rcpp::export\]\])' >/dev/null && [ -z "$EXP_CHANGED" ]; then
  echo "WARNING: an export attribute was added but RcppExports.* did not change."
fi

say "6. Documentation drift (working copy)"
git status --short man/ NAMESPACE | { grep . || echo "man/ and NAMESPACE clean in the working copy"; }
ROXY=$(git diff "$RANGE" -- R/ | grep -cE '^\+.*@(export|param|return|inheritParams|rdname|noRd)' || true)
MAN=$(git diff --name-only "$RANGE" -- man/ NAMESPACE | wc -l | tr -d ' ')
echo "roxygen tag lines added: $ROXY; man/NAMESPACE files changed in range: $MAN"
if [ "$ROXY" -gt 0 ] && [ "$MAN" -eq 0 ]; then
  echo "-> roxygen changed but no man/NAMESPACE commit: run devtools::document() on HEAD and check for drift (a @noRd-only change is fine)."
fi

say "7. Branch fold policy (feature branch writes NEWS.d, never Version or NEWS.md)"
git diff --name-only "$RANGE" -- DESCRIPTION NEWS.md | { grep . && echo "-> Version/NEWS.md edited in range; allowed only on the integration branch" || echo "DESCRIPTION and NEWS.md untouched"; }
git diff --name-only --diff-filter=A "$RANGE" -- NEWS.d/ | { grep . || echo "no NEWS.d fragment added (fine when nothing user-visible moved; otherwise a gap)"; }

say "8. Forbidden constructs added (cross-package :::, magrittr pipe, baseline edits)"
printf '%s\n' "$ADDED" | awk -F'\t' '$1 ~ /^R\// && $3 ~ /[A-Za-z0-9.]+:::/ { printf "%s:%s %s\n", $1, $2, $3 }' | { grep . || echo "no pkg::: in R/"; }
printf '%s\n' "$ADDED" | awk -F'\t' '$3 ~ /%>%/ { printf "%s:%s %s\n", $1, $2, $3 }' | { grep . || echo "no %>%"; }

say "9. Comment lines removed (each must leave with the functionality it explained)"
REMOVED=$(git diff -U0 --no-color "$RANGE" -- R/ src/ tests/testthat/ | grep -E '^-[[:space:]]*(#|//)' | grep -v '^---' | grep -vE "^-[[:space:]]*#'" | wc -l | tr -d ' ')
ROXY_REMOVED=$(git diff -U0 --no-color "$RANGE" -- R/ | grep -E "^-[[:space:]]*#'" | wc -l | tr -d ' ')
echo "plain comment lines removed: $REMOVED; roxygen lines removed: $ROXY_REMOVED"
echo "(list them with: git diff -U0 $RANGE -- R/ src/ tests/testthat/ | grep -E '^-\\s*(#|//)' | grep -v '^---')"

if [ -n "$CHANGE" ]; then
  DIR="openspec/changes/$CHANGE"
  say "10. OpenSpec artifacts for $CHANGE"
  if [ ! -d "$DIR" ]; then
    echo "$DIR not found (archived? look under openspec/changes/archive/)"
  else
    grep -E '^(status|depends-on):' "$DIR/proposal.md" 2>/dev/null || echo "proposal.md carries no status:/depends-on: line"
    OPEN=$(grep -c '^- \[ \]' "$DIR/tasks.md" 2>/dev/null || true)
    DONE=$(grep -c '^- \[x\]' "$DIR/tasks.md" 2>/dev/null || true)
    echo "tasks.md: $DONE ticked, $OPEN open"
    echo "-- task notes recording a deferral, deviation or descope (each needs a matching delta or an owner; ADR-0062):"
    grep -nEi 'defer|descope|deviation|not yet|left for|follow-up|out of scope|later change|successor|pre-existing' "$DIR/tasks.md" \
      | cut -c1-160 | { grep . || echo "none"; }
    if [ -x .plan/opsx-spec-placement-check.sh ]; then
      echo "-- placement pre-flight:"; bash .plan/opsx-spec-placement-check.sh "$CHANGE" 2>&1 | tail -5
    fi
    if command -v openspec >/dev/null 2>&1; then
      echo "-- openspec validate --strict:"; openspec validate "$CHANGE" --strict 2>&1 | tail -5
    fi
    echo "-- proposal Impact claims to verify against the range (src/, exports, dependencies):"
    grep -nEi 'no `?src/?`? change|src/|export|Depends|Imports|NAMESPACE' "$DIR/proposal.md" | cut -c1-160 | { grep . || echo "none stated"; }
  fi
fi

say "Commits in range"
git log --oneline "$RANGE" | cat
