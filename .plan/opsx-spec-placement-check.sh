#!/usr/bin/env bash
# Spec-delta placement pre-flight for /opsx:archive. Invoked as an explicit
# archive step (see openspec/config.yaml `rules.archive`) BEFORE the delta specs
# are folded into openspec/specs/**.
#
# Why this exists: `openspec validate` checks SHALL wording and scenario
# structure, but NOT `##` section placement. A `## MODIFIED Requirements` block
# naming a requirement that does not exist in the living spec validates cleanly
# and then lands as an ADD at archive -- leaving the old wording in place beside
# the new one, silently. The inverse is just as quiet: an `## ADDED` block
# naming a requirement the living spec already has duplicates it. Both are
# archive-time data loss that no other gate sees.
#
# Renames are the reason a naive version of this check gets ignored. A
# `## RENAMED Requirements` block moves a requirement's name, so afterwards
# neither the FROM name (gone from the living spec) nor a MODIFIED block using
# the TO name resolves the way a plain existence test expects. A MODIFIED or
# REMOVED block naming either side of a declared rename is therefore satisfied
# when either side is in the living spec -- correct before the archive (FROM
# still there) and after it (TO now there).
#
# The rename block ITSELF is stricter, and the reason is the 2026-09-07 fold.
# `openspec archive` resolves a `## RENAMED` block by its FROM header and
# aborts the entire archive -- changing nothing -- when that header is not in
# the living spec. An earlier version of this script accepted either side here
# too, so `class-naming-scheme` passed the pre-flight and then killed the
# archive: an implementation task had already applied the rename to the living
# spec by hand, leaving FROM gone and TO present. The check advertised as the
# fold's safety net missed the one failure mode that actually stops a fold.
# Before the archive, FROM resolving is therefore the only thing that counts,
# and a rename already applied is a defect to fix rather than a harmless no-op.
# After the archive FROM is gone by construction, so the `archive/<date>-<name>`
# re-run accepts the TO side instead and still does not cry wolf.
#
# Usage: bash .plan/opsx-spec-placement-check.sh <change-name>
#        bash .plan/opsx-spec-placement-check.sh archive/<date>-<name>
# Exit 0 = placement is sound, 1 = problems found (listed), 2 = bad invocation.

name="${1:-}"
if [ -z "$name" ]; then
  echo "usage: bash .plan/opsx-spec-placement-check.sh <change-name>" >&2
  exit 2
fi

root="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
change_dir="$root/openspec/changes/$name/specs"
living_dir="$root/openspec/specs"

if [ ! -d "$change_dir" ]; then
  echo "placement-check: no specs/ under openspec/changes/$name" >&2
  exit 2
fi

# An `archive/<date>-<name>` argument names a change whose deltas have ALREADY
# been folded into the living spec. Its `## ADDED` requirements are therefore
# expected to be there -- that is what the archive did -- so the duplicate test
# is meaningless post-fold and would report every ADD as a problem. The
# MODIFIED / REMOVED / RENAMED tests stay live, because each is written to hold
# on both sides of the archive. This mode exists so the check can be re-run on a
# known-good archived change to prove it still behaves.
post_archive=0
case "$name" in
  archive/*) post_archive=1 ;;
esac

# Emit "<KIND>\t<requirement name>" for one delta file. Section state carries
# across requirement headers; RENAMED blocks use `- FROM:`/`- TO:` bullets
# holding a backticked header rather than a header of their own.
parse_delta() {
  awk '
    /^## +ADDED/    { kind = "ADDED";    next }
    /^## +MODIFIED/ { kind = "MODIFIED"; next }
    /^## +REMOVED/  { kind = "REMOVED";  next }
    /^## +RENAMED/  { kind = "RENAMED";  next }
    /^## /          { kind = "";         next }
    kind == "RENAMED" && /^- *(FROM|TO):/ {
      side = ($0 ~ /^- *FROM:/) ? "RENAMED_FROM" : "RENAMED_TO"
      name = $0
      sub(/^- *(FROM|TO): *`?### Requirement: */, "", name)
      sub(/`? *$/, "", name)
      print side "\t" name
      next
    }
    kind != "" && /^### Requirement: / {
      name = $0
      sub(/^### Requirement: */, "", name)
      sub(/ *$/, "", name)
      print kind "\t" name
    }
  ' "$1"
}

# Exact header match, so "Per-event scores primitive" does not satisfy a lookup
# for "Per-event scores primitive extended".
in_living() {
  [ -f "$LIVING" ] && grep -qxF "### Requirement: $1" "$LIVING"
}

# Is `$1` one side of a rename this delta declares, whose other side (or itself)
# is in the living spec? FROM/TO bullets pair by position, which is how they are
# written.
rename_satisfied() {
  local want="$1" i=0 from to
  while IFS= read -r from; do
    i=$((i + 1))
    to="$(printf '%s\n' "$RENAMED_TO" | sed -n "${i}p")"
    if [ "$want" = "$from" ] || [ "$want" = "$to" ]; then
      if in_living "$from" || in_living "$to"; then
        return 0
      fi
    fi
  done <<EOF
$RENAMED_FROM
EOF
  return 1
}

# Does the rename declaring `$1` as its FROM side actually have a source to
# move? Stricter than `rename_satisfied` on purpose -- see the note at the top:
# `openspec archive` needs FROM, so before the fold nothing else will do.
rename_from_resolves() {
  local want="$1" i=0 from to
  while IFS= read -r from; do
    i=$((i + 1))
    [ "$want" = "$from" ] || continue
    to="$(printf '%s\n' "$RENAMED_TO" | sed -n "${i}p")"
    if [ "$post_archive" -eq 1 ]; then
      in_living "$from" || in_living "$to"
      return $?
    fi
    in_living "$from"
    return $?
  done <<EOF
$RENAMED_FROM
EOF
  return 1
}

problems=0
checked=0

for delta in "$change_dir"/*/spec.md; do
  [ -f "$delta" ] || continue
  capability="$(basename "$(dirname "$delta")")"
  LIVING="$living_dir/$capability/spec.md"
  parsed="$(parse_delta "$delta")"
  RENAMED_FROM="$(printf '%s\n' "$parsed" |
    awk -F'\t' '$1 == "RENAMED_FROM" { print $2 }')"
  RENAMED_TO="$(printf '%s\n' "$parsed" |
    awk -F'\t' '$1 == "RENAMED_TO" { print $2 }')"

  while IFS=$'\t' read -r kind req; do
    [ -n "$kind" ] || continue
    checked=$((checked + 1))
    case "$kind" in
      MODIFIED | REMOVED)
        if ! in_living "$req" && ! rename_satisfied "$req"; then
          echo "MISPLACED  $capability :: ## $kind '$req'"
          echo "           not in $LIVING -- it lands as an ADD at archive,"
          echo "           leaving the old wording in place. Use ## ADDED, fix"
          echo "           the name, or declare the rename in ## RENAMED."
          problems=$((problems + 1))
        fi
        ;;
      ADDED)
        if [ "$post_archive" -eq 0 ] && in_living "$req"; then
          echo "DUPLICATE  $capability :: ## ADDED '$req'"
          echo "           already in $LIVING -- ADDING it duplicates the"
          echo "           requirement. Use ## MODIFIED."
          problems=$((problems + 1))
        fi
        ;;
      # Checked from the FROM side only: that is the side `openspec archive`
      # resolves, so testing the TO side would add nothing and would report one
      # broken rename twice.
      RENAMED_FROM)
        if ! rename_from_resolves "$req"; then
          echo "DANGLING   $capability :: ## RENAMED FROM '$req'"
          echo "           not in $LIVING. \`openspec archive\` resolves a"
          echo "           rename by its FROM header and aborts the whole"
          echo "           archive when it is missing. If the rename was"
          echo "           already applied to the living spec by hand, drop"
          echo "           the block as satisfied and say so; otherwise fix"
          echo "           the FROM name to match the living spec."
          problems=$((problems + 1))
        fi
        ;;
    esac
  done <<EOF
$parsed
EOF
done

if [ "$problems" -eq 0 ]; then
  echo "placement-check: OK ($checked requirement placements, $name)"
  exit 0
fi
echo "placement-check: $problems problem(s) found in $name"
exit 1
