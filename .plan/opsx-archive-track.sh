#!/usr/bin/env bash
# Archive-tracking helper for /opsx:archive. Invoked as the FIRST step of the
# archive flow (see openspec/config.yaml `rules.archive`), BEFORE the change dir
# is moved to archive/, so its progress.md is still at
# openspec/changes/<name>/progress.md. Records the archived change in
# .plan/goldfish_versions.csv and snapshots its progress.md to
# .plan/progress-<name>-openspecs.md.
#
# Personal working-notes automation — every target is gitignored. No longer a
# PreToolUse hook (that matched command substrings and fired on unrelated Bash
# calls); it is now a plain script the archive skill calls explicitly with the
# change name, so nothing runs unless an archive is actually happening.
#
# Usage: bash .plan/opsx-archive-track.sh <change-name>
# Never fails the caller (always exit 0); no `set -e`/pipefail.

name="${1:-}"
if [ -z "$name" ]; then
  echo "opsx-archive-track: no change name given; nothing recorded" >&2
  exit 0
fi

root="${CLAUDE_PROJECT_DIR:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
plan="$root/.plan"
csv="$plan/goldfish_versions.csv"
progress="$root/openspec/changes/$name/progress.md"
mkdir -p "$plan"

# Snapshot the progress log while the change dir is still in place (pre-move).
if [ -f "$progress" ]; then
  cp "$progress" "$plan/progress-$name-openspecs.md"
else
  echo "opsx-archive-track: $progress not found; skipped progress snapshot" >&2
fi

# Append a version row, idempotent on the change name.
if [ -f "$csv" ] && grep -q "change '$name'" "$csv" 2>/dev/null; then
  exit 0
fi
ver="$(grep -E '^Version:' "$root/DESCRIPTION" 2>/dev/null | head -1 \
  | sed 's/^Version:[[:space:]]*//')"
[ -n "$ver" ] || ver="unknown"
hash="$(git -C "$root" rev-parse --short HEAD 2>/dev/null || echo unknown)"
day="$(date +%Y-%m-%d)"
desc="Archived OpenSpec change '$name' on $day (version $ver); see openspec/changes/archive/."
printf '%s,local,stocnet/goldfish,%s,%s,archive,FALSE,%s\n' \
  "$ver" "$hash" "$hash" "$desc" >> "$csv"
exit 0
