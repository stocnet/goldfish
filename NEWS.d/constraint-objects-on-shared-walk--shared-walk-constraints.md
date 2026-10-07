* Fixed constrained rates using a stale risk set between layer changes.
* Improved `support_constraint` on a layer no formula term reads:
  * the preprocessing walk maintains it, with no separate walk;
  * the walk handle and `simulate()` no longer refuse it.
* Improved `support_constraint` checks, now run during preprocessing.
* Updated estimation to refuse a constrained object from an earlier version.
