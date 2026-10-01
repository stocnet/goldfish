* Fixed a global-only `support_constraint` corrupting R's `TRUE` in the walk.
* Fixed fits where a constraint empties the risk set for an interval.
* Improved `support_constraint` on REM and coordination models:
  * alter-, ego- and global-kind constraints keep two vectors, not a grid.
* Improved the preprocessed object, which stores each availability once:
  * composition changes are kept only as update buffers;
  * no raw presence is stashed on the support mask.
* Updated estimation to refuse an object whose composition is only a list.
