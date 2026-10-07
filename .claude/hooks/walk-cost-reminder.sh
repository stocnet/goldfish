#!/usr/bin/env bash
# PostToolUse reminder for edits to per-event code. Points the session at
# .claude/rules/walk-cost.md and names the constructs most often reintroduced.
# Fires once per file per session: the rules do not change between edits and a
# reminder repeated on every edit is one that stops being read.

set -u

INPUT=$(cat)
FP=$(printf '%s' "$INPUT" | jq -r '.tool_input.file_path // ""' 2>/dev/null)
SESSION=$(printf '%s' "$INPUT" | jq -r '.session_id // "nosession"' 2>/dev/null)
[ -z "$FP" ] && exit 0

case "$FP" in
  */R/preprocess_*.R | */R/model_preprocess*.R | */R/support_mask*.R | \
    */R/broadcast_kind.R | */R/walk_handle.R | */R/simulate_*.R | \
    */R/process_state_evaluators.R | */R/event_streams.R | */R/state_at.R | \
    */R/cpp_interface.R | */R/estimation_core.R | */R/functions_effects_*.R) ;;
  *) exit 0 ;;
esac

SEEN_DIR="${TMPDIR:-/tmp}/goldfish-walk-cost-$SESSION"
mkdir -p "$SEEN_DIR" 2>/dev/null || exit 0
MARK="$SEEN_DIR/$(printf '%s' "$FP" | tr '/' '_')"
[ -e "$MARK" ] && exit 0
: >"$MARK"

# Constructors on the lines this edit wrote. A hit is a prompt to check whether
# the line runs once per event, not a verdict: most hits in these files are
# setup code.
NEW=$(printf '%s' "$INPUT" | jq -r '.tool_input.new_string // .tool_input.content // ""' 2>/dev/null)
HITS=$(printf '%s' "$NEW" |
  grep -oE '\b(data\.frame|as\.data\.frame|array|matrix|rbind|cbind|append|support_to_grid)\(|<<-|\bassign\(' |
  sort -u | tr '\n' ' ')

MSG="$(basename "$FP") holds per-event code. Follow .claude/rules/walk-cost.md: store initial value + flat updates + pointer at the broadcast kind; read rows and cells by linear index on the stored vector, never by building the grid; write state in place through write_entries(); allocate nothing per event; resolve lookups before the walk. The 1e-6 baselines cannot see a slower or larger walk."
if [ -n "$HITS" ]; then
  MSG="$MSG This edit wrote: $HITS- confirm none of them runs once per event."
fi

jq -nc --arg m "$MSG" \
  '{hookSpecificOutput: {hookEventName: "PostToolUse", additionalContext: $m}}'
exit 0
