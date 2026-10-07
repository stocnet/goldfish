# Report template

File: `.plan/develop/<change>[-<scope>]-conformance.md`. Dense tables, line
references at `<head>`, every claim with its evidence. Abbreviate files that
recur (`pj` = `R/preprocess_joint.R`) in a key line under the header.

```markdown
# <change> [, <scope>] — conformance

Branch `<branch>`, HEAD `<head>`, base `<base>`, checked <date>, session
<id>. Commits in scope: `<hash>` (<what>), ... Read-only; the only write is
this file; ablations in a detached worktree, tree clean before and after.
Line references are to `<head>`. Keys: `pj` = `R/preprocess_joint.R`, ...

## 1. Requirements, scenarios and task statements

Status key: **S** satisfied, **P** partially, **N** not satisfied, **O** out
of this scope (listed because the delta states it), **D** deviation recorded
in tasks.md / progress.md.

### <spec delta> — <MODIFIED|ADDED> "<requirement title>"

| Statement | Status | Code | Test |
| --- | --- | --- | --- |
| <requirement or scenario, condensed> | S | `file:line` | `test-file.R:line` |

### Tasks

| Task | Status | Evidence |
| --- | --- | --- |
| 1.1 <statement, condensed> | S | `file:line`; commit `<hash>` |

### Specific checks

| Check | Result |
| --- | --- |
| Proposal Impact claims (src/, exports, dependencies) | ... |
| Storage promised vs stored (`str()`/`obj_size()` on <fixture>) | ... |
| Same body at several sites agrees | ... |
| Golden provenance (freeze is an ancestor of the flip; one commit) | ... |

## 2. Per-event cost

Scanner hits: <n>, of which <k> on the per-event path. For each per-event
hit: `file:line`, rule, what it costs on the large fixture, and whether the
design's cost section allows it.

| Measured in `$WT` | Before (<tip> or recorded) | After (<head>) | Design predicted | Journal claims |
| --- | --- | --- | --- | --- |
| walk alloc_mb / size_mb (budget fixture) | ... | ... | ... | ... |
| <bench::mark of one step / fit per backend> | ... | ... | ... | ... |

Cost guard: `test-file.R:line`; recorded failing in progress.md at <where>;
ablation reproduces the red (section 3).

## 3. Gaps

1. **blocks** — <what>. `file:line`. Fails <spec clause / rule N / task
   text>. Reproduced: <how>. Owner: **this change** (task <n>).
2. **should-fix** — ...
3. **note** — <pre-existing / out of scope>, verified pre-existing at
   `<commit>`. Owner: <change or role>.

## 4. Data

- Suite (`NOT_CRAN=true`, `$WT` at `<head>`): `FAIL 0 | WARN n | SKIP n |
  PASS n`; failed rows: none. Baseline/golden files: <n>, skipped 0,
  failed 0 — PASS not SKIP.
- Byte-identity: `git diff <base> <head> -- tests/testthat/_baselines`
  empty.
- Ablation: <fix files> reverted to `<pre-fix>` with the detector at
  `<head>`: `test-file.R` FAIL <n> ("<failing test title>"); restored, tree
  clean.
- Fixtures named by the tasks: <file> PASS n / FAIL 0 each.
- Reproduced claim: <progress.md says X; measured Y>.
- Hygiene: <summary of the hygiene-check output>. Introduced by the range:
  <list or none>. Pre-existing in the touched files: <list with owner, or
  none>.
- Evidence files beside this report: the suite log, `primary-status-before
  .txt` / `primary-status-after.txt` (`git status --porcelain` and `git
  worktree list` of the shared checkout), `rcppexports-diff.txt` when
  `src/` is in the range, the ablation and measurement scripts with their
  logs.

## 5. Verdict

CONFORMANT | NOT CONFORMANT — <at most one sentence: the blocking gap, or
"both halves clean">.

Actions for the coordinator:
- <gap n>: <what to do, who>.
- <gap m>: ...
```

The verdict line is one line; the argument for it lives in sections 1 to
4. A report with no gaps still lists what was verified under section 1 and
section 4, so a reader knows what was confirmed, not only what was wrong.
