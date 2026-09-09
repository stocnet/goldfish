# the merged walk names window effects rather than crashing

    Code
      suppressWarnings(preprocess_joint(single_process_joint(spec)))
    Condition
      Error in `build_merged_blocks()`:
      ! The merged walk does not yet support window effects.
      x Windowed object: "calls_2".
      i Preprocess through `compute_statistics()` for now; the merged walk gains window effects when the derived objects and their expiry streams enter its shared registry and schedule.

