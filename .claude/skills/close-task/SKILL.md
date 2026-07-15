---
name: close-task
description: Close out an OpenSpec task in goldfish — document() drift check, NOT_CRAN verification (baselines PASS not SKIP), a conventional commit with the Co-Authored-By trailer, and the tasks.md / progress.md bookkeeping. Use after finishing one OpenSpec task, before moving to the next.
disable-model-invocation: true
---

# close-task

The repeatable close-out for a single OpenSpec task in this repo. It encodes the
disciplines from `openspec/config.yaml` and `CLAUDE.md` so every task lands the
same way: documented, green against the frozen baselines, committed atomically,
and logged. Run it once per completed task.

Take the change name and a one-line description of what the task did as context
(infer the change from `openspec list --json` if only one is active).

## 1. Document + drift check

Only if the task touched roxygen comments, `@export`/S3 methods, or a function
signature (the `document() drift check` hook flags this):

```bash
NOT_CRAN=true Rscript -e 'suppressMessages(devtools::document())'
git status --short man/ NAMESPACE
```

- Any `man/` or `NAMESPACE` change **must** be committed *with* the code change,
  never separately. If `document()` produced a diff you did not expect, stop and
  reconcile it before committing.

## 2. Verify (baselines PASS, not SKIP)

Invoke the **`not-cran-test`** skill (or run its command). The task is verified
only when **FAIL = 0**, **ERROR = 0**, and the coefficient/global/cpp-golden
baseline files report **passed > 0, skipped == 0**. A baseline that SKIPped means
`NOT_CRAN=true` was not in effect — the 1e-6 floor did not run, so the task is
**not** done. The ~19 pre-existing dissolve/imputation warnings are expected.

## 3. Commit (atomic, conventional, code only)

One focused commit per task so any step can be reverted with tests green at each
commit. Stage **only package code** — never the gitignored working artifacts:

```bash
# stage code (adjust to what the task changed)
git add R/ tests/ src/ man/ NAMESPACE DESCRIPTION NEWS.md
# NEVER: openspec/  .claude/  .plan/   (gitignored working state)
git status --short   # confirm no openspec/ or stray files are staged
```

Commit message — conventional prefix + the OpenSpec task id, and the trailer:

```
<verb>: <description> (<task-id>)

<body: what changed and why; the NOT_CRAN pass line;
 any deliberate behaviour change or test update>

Co-Authored-By: Claude Opus 4.8 <noreply@anthropic.com>
```

- `<verb>`: `feat` (new capability), `refactor` (behaviour-preserving),
  `test`, `fix`, `chore`, `style`.
- Do **not** push unless explicitly asked.
- Leave the pre-existing untracked/modified files that are not yours
  (`.github/CONTRIBUTING.md`, `CLAUDE.md`, `myeff.html`) untouched.

## 4. Bookkeeping (local-only, not staged)

Both live under the gitignored `openspec/` tree — update them, never `git add`
them:

- **`tasks.md`**: flip the task `- [ ]` → `- [x]` and append a short completion
  note (what landed, key decisions, the `PASS N / FAIL 0 / SKIP 0` line, the
  commit hash).
- **`progress.md`**: append/extend the current session section — tasks done +
  commit hashes, findings, full-suite state, and "next".

## Done

Report: the commit hash, the suite totals, whether `document()` drifted, and the
next task. If it was the last task of a phase milestone, remember the phase bump
(DESCRIPTION version + NEWS.md) is its own task (group 8.x), not this one.