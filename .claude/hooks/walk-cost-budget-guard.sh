#!/usr/bin/env bash
# PreToolUse guard for the walk cost budget. An edit to the recorded values is
# a decision about how much the walk may cost, so it is put to the person at
# the keyboard instead of being made silently alongside a code change.

set -u

FP=$(jq -r '.tool_input.file_path // ""' 2>/dev/null)
case "$FP" in
  */tests/testthat/_budgets/walk_cost.csv) ;;
  *) exit 0 ;;
esac

jq -nc '{
  hookSpecificOutput: {
    hookEventName: "PreToolUse",
    permissionDecision: "ask",
    permissionDecisionReason: "Editing the walk cost budget. Raising a recorded value or a limit accepts a slower or larger walk and needs the maintainer to approve it; find what made the walk more expensive first (.claude/rules/walk-cost.md). Lowering a value after an improvement is fine."
  }
}'
exit 0
