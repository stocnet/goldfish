---
name: conformance-inspector
description: Post-landing conformance inspection of a goldfish OpenSpec change (or one task group of it) against its own proposal, design, spec deltas and tasks, the living spec, and the per-event cost rules in .claude/rules/walk-cost.md. Use it whenever a change or group is reported landed, ticked, "done", or "awaiting fold" and anyone asks whether the code actually matches what was designed; before an archive or trunk fold; when a coordinator needs a fresh agent to check a unit; or when someone suspects the implementation drifted from the design, deferred work behind ticked boxes, or reintroduced a known pitfall (a dense grid or data frame built per event, a vector grown per event, a copy on write, a second walk). Prefer this over a plain code review or the spec-conformance agent alone whenever the question is "does the landed change conform", not "is this diff buggy".
---

# conformance-inspector

A landed change is checked in two halves, and a unit is conformant only when
both are clean:

- **Documents against code.** Every requirement, scenario and task statement
  in the change's artifacts has a code site and a test, and says what the code
  does. Drift between them is the finding.
- **Data against both.** The fixtures, the frozen baselines, an ablation of
  each detector, and a cost measurement. These catch the class where the
  artifacts and the code agree and are both wrong: three of the four findings
  in the first group of `preprocess-one-walk` were visible in no artifact and
  came out of fixtures failing.

Two facts about this repo shape the whole procedure. Ticked boxes prove
nothing: two changes archived at 31/31 and 33/33 with their deferrals written
fourteen times in a gitignored journal and the synced spec left asserting
behavior the code never had (ADR-0062). And the frozen 1e-6 baselines cannot
see cost: a walk that stores one grid per event, allocates a data frame per
event, or copies the state matrix on every write returns the same numbers.
The inspector therefore reads the artifacts for deferrals hidden in task
notes, and hunts the per-event code for the pitfalls the baselines are blind
to, with a measurement rather than an opinion.

The inspector is read-only. The only files it writes are its report and the
evidence beside it. It never edits source, tests or change artifacts, never
commits, never regenerates a baseline, and performs every ablation in a
detached worktree so the shared working copy is never touched (another
session may be applying in it).

## Inputs

Paths below of the form `scripts/...` and `references/...` are relative to
this skill's directory (`.claude/skills/conformance-inspector/` in the repo);
run the scripts from the repository root with that prefix.

- The change name; if given, the group or task range in scope. Everything
  outside that scope that the deltas state is listed as **O** (out of scope),
  never as a gap.
- The commit range: from the branch point or the predecessor's landing commit
  (`<base>`, the commit *before* the first in scope) to the last commit in
  scope (`<head>`). Find them with `git log --oneline --grep=<change>`, the
  hashes in `tasks.md` tick notes, and `progress.md`; the previous
  `.plan/develop/<change>-*-conformance.md`, when one exists, names the tip
  it checked.
- Where the report goes: `.plan/develop/<change>[-<scope>]-conformance.md`
  (local, gitignored; create the directory if missing), with the scratch
  scripts, logs and snapshots beside it. Prior reports there are the house
  format; read one before writing.

## Procedure

Do the slow things first, in the background, and read while they run.

### 0. Set up the worktree and start the suite

Snapshot the shared checkout first, so the report can show it was left
alone: save `git status --porcelain` and `git worktree list` to the
evidence folder now and again at the end.

```bash
WT=$(mktemp -d)/wt && git worktree add --detach "$WT" <head>
git -C "$WT" rev-parse HEAD   # must print <head>; a tool-made worktree may sit at a stale commit
```

Run the `not-cran-test` skill's command inside `$WT` and keep its log. The
first `load_all()` in a fresh worktree compiles the C++; if `src/` is in
the range, run the `cpp-recompile` skill's steps there first so nothing is
tested against a stale object, and save `git -C "$WT" diff --stat --
R/RcppExports.R src/RcppExports.cpp` after `compileAttributes()` as the
bindings evidence (Rcpp prints "updated." whenever it rewrites the file;
only a non-empty diff means the committed bindings were stale). If you are
a subagent, a background job cannot wake you when it finishes: run the
suite in the foreground with a long timeout, or poll its log with a
`Monitor` until-loop, rather than waiting on a notification. Every
ablation and every measurement happens in `$WT`; remove it at the end
(`git worktree remove --force "$WT"`) and confirm the shared checkout's
status matches the opening snapshot.

### 1. Read the artifacts, in this order

1. `proposal.md`: scope, Impact (every claim like "no `src/` change" or
   "no exported row moves" is a checkable statement), `status:`,
   `depends-on:`.
2. `design.md`: each decision; the **Storage and per-event cost** section
   (its predicted sizes and per-event allocations are the oracle for step 3);
   Non-goals and deferred items.
3. `specs/**/spec.md`: each requirement, its `SHALL NOT` storage clause,
   each `#### Scenario:`. For a `## MODIFIED` block, read the living
   requirement it modifies too, and grep the other living capabilities for
   a restatement of the same rule: a change that alters a formula in one
   spec often leaves a sibling spec asserting the old one.
4. `tasks.md`: each task statement *and its tick note*. A note saying
   deferred, descoped, deviation, left for, follow-up, out of scope, or
   "the test uses X instead" is a claim that an artifact was not met as
   written; find the matching delta or owner, or it is a gap.
5. `progress.md`: the measurements, counts and deviations the applier
   recorded. These are claims to reproduce, not evidence. A deviation that
   lives only here is invisible to a clone, since `progress.md` is
   gitignored and `tasks.md` is tracked.
6. `.claude/rules/walk-cost.md`, and any ADR the proposal or design cites
   when the vault is available.

### 2. Half one: documents against code

Build the task table (template in `references/report-template.md`): one row
per requirement, scenario and task statement, with a status, the code site
as `file:line` at `<head>`, and the test that exercises it. A
verification-only task (suite run, validate, NEWS fragment) cites its log
or commit instead of a code line. Status key: **S** satisfied, **P**
partially, **N** not satisfied, **O** out of this scope, **D** deviation
recorded in the artifacts.

What to look for beyond "is there code":

- **Faithful, not adjacent.** The scenario's `THEN` and the test's
  assertion say the same thing: the same tolerance, the same error text,
  the same object compared. A relational parity test against a live
  oracle that routes through the same code is vacuous; one against a
  frozen golden is not. When goldens were frozen, confirm the freeze is an
  ancestor of the flip (`git merge-base --is-ancestor`) and the file has
  one commit.
- **Storage promised, storage delivered.** When a design or spec says
  "two factor vectors", "one entry per flip", "at its broadcast kind",
  "no dyad-shaped object", inspect the preprocessed object on a fixture
  (`str()`, `lobstr::obj_size()`, `dim()` of the stored field) rather than
  reading the code's intent. The first support-mask change stored one
  dense matrix per event under a requirement saying it would not. Also
  check the *empty* case: a field the design says is absent or `NULL` when
  nothing changed, stored instead as a zero-length buffer, can switch on a
  consumer's expensive branch (see step 3).
- **Reverse drift.** New exported surface, arguments, helpers or behavior
  with no requirement, design decision or task describing it.
- **Scope honored.** A Non-goal quietly done, or a guard the design kept
  that the code dropped.
- **The three copies rule.** When the same body exists in more than one
  loop or substrate, diff them: a fix applied to one site is a gap in the
  other.

### 3. The cost half: per-event pitfalls

This is the part a test cannot do. Run the scanner on the range:

```bash
python3 scripts/scan-per-event-constructs.py <base> <head>   # from this skill's directory
```

Its default file list is the rule file's `paths:`; add with `--paths` any
file in the range whose functions run per stored row or per dependent
event even though the list omits it (`R/availability_effect.R`,
`R/model_estimate.R` validation loops). Every hit names its enclosing
function. For each, trace that function to either setup (runs once per
walk or fit: a finding only if the design said otherwise) or the per-event
path: the walk step, an effect update closure, `compute_step.*`, a gather
loop, a kernel's event loop, the simulation step, or anything called from
them. Only per-event hits are findings, and every per-event hit carries its
rule number in the report (1 one value per event; 2 grid built to read a
row or cell; 3 copy on write through a second reference; 4 allocation per
event; 5 lookup by name per event; 6 second pass or second walk); a hit
that is not per event is marked "setup". Quantify each finding with the
reference costs in `walk-cost.md`: one logical grid is 13.8 MB and one
double grid 27.5 MB at 1899 actors, an `n2` row per dependent event was
69 MB over 3000 events at 1500 actors. The scanner's patterns and the
decision procedure are in `references/per-event-pitfalls.md`.

The scanner sees constructs, not semantics. The costs it cannot see are
found by **measuring the no-op case**: a constraint that excludes nothing,
a mask with zero flips, a composition with zero changes must cost the same
as the unconstrained path at every layer the change touched, so run the
constrained and unconstrained fits back to back on each backend
(`bench::mark()`, fixed Newton iterations, a 300-actor fixture is enough
to separate a grid) and the walk and the simulation step likewise. Any
allocation gap is per-event overhead of the new code path, whatever the
code looks like. A `2 x 0` buffer that turned on the R engine's per-event
axis filter (+17 percent allocation) was found this way and by nothing
else.

Then measure the claimed improvements, in `$WT`, against the design's
prediction:

- Walk touched: `Rscript tests/testthat/_budgets/record-walk-cost.R` and
  compare with `tests/testthat/_budgets/walk_cost.csv`; the budget test
  itself runs in the suite. A recorded value raised in the range needs the
  commit to say what the extra cost buys.
- Estimation or likelihood touched: `bench::mark()` one contribution step
  or one fit per backend on an optimized build, allocations as well as
  time.
- A copy suspected: `tracemem()` on the buffer across a multi-event walk.
  It reports on **stdout**; a test capturing messages cannot fail.
- The cost guard the task rules require: confirm it exists, that
  `progress.md` records it failing before the fix, and reproduce the red by
  ablation (step 4). A guard written on a static fixture proves the static
  case only: re-run its measurement on a dynamic variant (a mask that flips
  every event, a composition that changes) before accepting a task's
  "reads no row per event" as general.
- Compare the measured delta with the design's predicted delta and with
  what `progress.md` claims. A prediction refuted and not recorded is a
  gap; a prediction never made for a change that adds per-event code is a
  gap in the design, named as such.

### 4. Half two: data

- **Suite.** Read the verdict off the numbers, never the reporter's prose:
  the `FAIL n` on the `[ FAIL n | WARN n | SKIP n | PASS n ]` line, or
  `sum(as.data.frame(res)$failed)`. `failed = 0` with warnings is a pass;
  the summary reporter's Warnings list reads exactly like a Failed list.
- **Baselines PASS, not SKIP.** The coefficient, global and C++ golden
  files report `passed > 0, skipped == 0`. A SKIP means `NOT_CRAN` was not
  set and nothing about the 1e-6 floor was verified.
- **Byte-identity.** `git diff <base> <head> -- tests/testthat/_baselines`
  is empty; quote the command in the report. A behavior-preserving change
  that moved a baseline is not conformant whatever its tests say.
- **Ablation of each detector, site by site.** In `$WT`: revert one site
  the fix added (`git checkout <pre-fix-commit> -- <file>`, or a one-line
  edit), keep the detector at `<head>`, run that test file, see it red
  with the failure the journal recorded; `git checkout <head> -- .` and
  confirm the tree is clean. Do this once per branch or call site the fix
  touched, not once for the whole fix: three of four counter branches in
  one change survived single-site ablation with every test green while the
  whole-fix ablation was red. A detector that stays green under ablation
  is vacuous and the task that wrote it is **N**.
- **The fixtures the tasks name.** Run them; a task that names a fixture
  that does not exist under any name is **P** at best.
- **One claimed number reproduced** from `progress.md` independently
  (a size, a count, a timing on the same fixture). Every number in the
  report must agree with the saved log it came from; recount before
  writing a headline figure.

### 5. Hygiene

```bash
bash scripts/hygiene-check.sh <base> <head> <change>   # from this skill's directory
```

It lists added lines over 80 characters, OpenSpec and ADR references in
added comments and in added documentation (`README`, `*.md` under `tests/`
and `inst/`), camelCase candidates, baseline and snapshot changes, `src/`
against `RcppExports.*`, `man/` and `NAMESPACE` drift, the branch fold
policy (a feature branch writes `NEWS.d/`, never `Version` or `NEWS.md`),
`pkg:::` and `%>%`, removed comment lines, task notes that record a
deferral, and the placement pre-flight and `openspec validate`. A reference
to *another* change's task ("process-simulation 2.11c") is as much a
dangling pointer as one to this change's. Everything it prints is about
lines the range *introduced*; debt that predates the range in the same
file is a note with an owner, not the change's gap, and when there is none
the report says so. A removed comment is fine only when the functionality
it explained left with it; a rewritten comment that tracks changed
functionality is allowed.

### 6. Write the report

Use `references/report-template.md`. Each gap, notes included, carries a
severity, the `file:line`, the artifact clause or rule it fails, and an
owner:

| Severity | Meaning |
| --- | --- |
| **blocks** | The unit is not conformant: a requirement or scenario unmet, a per-event cost rule broken, a vacuous or missing detector, a moved baseline, a ticked task whose note says it was not done. The change goes back to apply. |
| **should-fix** | Owned by this change before archive: tests that assert less than the task text, a naming or lint breach the range introduced, a comment now false, a proposal claim now wrong, a deviation recorded only in `progress.md`. |
| **note** | Pre-existing, out of scope, or owned elsewhere. Say whose. Recorded so the coordinator can fold it into that change's `tasks.md`. |

End with the verdict as a single line, CONFORMANT or NOT CONFORMANT plus
the one blocking gap, followed by a short list of the actions the
coordinator must take. The reasoning belongs in the sections above, not in
the verdict. The coordinator acts on it (new tasks, a fold into another
change, a tick reverted); the inspector does not.

## Judgment rules learned the hard way

- The constrained CollegeMsg cell varies about 18 percent run to run, so a
  3-rep median cannot resolve 3 percent. Attribute a suspected regression
  by direct tip-to-tip measurement in `$WT`, previous tip against `<head>`,
  back to back. Timings taken while other R jobs run are noise; say so
  rather than conclude from them.
- `Rprof` drops most samples on this macOS: read proportions from it, never
  totals. `Rprofmem()` by function is reliable for allocation attribution.
  `object.size()` counts a shared object twice; `lobstr::obj_size()` once.
- `tasks.md` is git-tracked in this repo; `progress.md` is not. A deferral
  that lives only in `progress.md` is invisible to a clone.
- A design decision deferring work to a named successor is **O** or **D**,
  not a gap; a deferral with no delta and no owner is a gap.
- `test_that()` titles over 80 characters are cosmetic; `.lintr` flags them,
  `air` cannot wrap them. Say so and move on.
- "The task says X, the code does Y, parity holds on the toy" is still **P**:
  record what is asserted and what is not.
- A commit in the range that touches files outside the change (a deleted
  directory, another change's artifacts) is a note for whoever folds the
  branch, with the recoverable commit named.
- Never stash. Never edit the working copy for an ablation. Never regenerate
  `tests/testthat/_baselines/`.
