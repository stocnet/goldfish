# NEWS.d — changelog fragments for parallel branches (vault ADR-0040)

Feature branches do not edit `NEWS.md` and do not bump the `DESCRIPTION`
Version — both are folds owned by the integration branch, performed once
when the branch merges. A user-facing change on a branch is recorded here
instead, as one fragment file per change:

```
<change-or-branch>--<slug>.md
e.g. two-sided-coordination--estimator-surface.md
     feature-hmm--latent-modes-rate.md
```

The prefix (OpenSpec change name, or branch name for ad-hoc work) keeps
fragments from two parallel branches from ever colliding on a filename.

## Writing a fragment

One markdown bullet (or a few), exactly as it should appear in `NEWS.md` —
user-facing wording, American English, backticks around code:

```markdown
* `estimate_dynamu()` estimates the five two-sided coordination
  mechanisms (#NNN).
```

A fragment is never edited after the branch is shared; refine wording by
replacing the file in the same branch before merge.

## Folding (integration branch, at merge — see `openspec/config.yaml`
`rules.merge`)

1. Bump the Version in `DESCRIPTION`; add the matching heading to
   `NEWS.md`.
2. Insert fragment contents under it in filename order; edit for flow.
3. Delete the consumed fragments in the same commit.

An empty `NEWS.d/` at any other time is the normal state. The directory is
tracked (fragments must travel between clones) and excluded from the
package tarball via `.Rbuildignore`.
