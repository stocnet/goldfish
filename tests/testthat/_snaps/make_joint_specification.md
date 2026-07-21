# a join referencing no panel layer is rejected as separable

    Code
      make_joint_specification(calls_spec, emails_spec, data = data)
    Condition
      Error in `make_joint_specification()`:
      ! A joint specification must reference a panel-observed layer.
      x None of the composed formulas read a "panel" layer, so the processes are exactly separable.
      i Estimate each specification on its own with `estimate_dynam()` or `estimate_rem()`.

# processes on different node sets are rejected

    Code
      make_joint_specification(calls_spec, twomode_spec, data = data)
    Condition
      Error in `make_joint_specification()`:
      ! All composed processes must share one node set.
      i Mixed node sets are not supported in this version.

# a DyNAM-i process cannot be composed

    Code
      make_joint_specification(calls_spec, emails_spec, data = data)
    Condition
      Error in `make_joint_specification()`:
      ! DyNAM-i processes cannot be composed into a joint specification.
      i Only "DyNAM" and "REM" processes are supported.

# two specifications modeling the same focal layer are rejected

    Code
      make_joint_specification(calls_a, calls_b, data = data)
    Condition
      Error in `make_joint_specification()`:
      ! Each joined specification must model a distinct focal layer.
      x Layer "calls" is modeled by more than one specification.
      i A layer's flavors must all be carried by a single specification; read a layer as a covariate to couple processes.

# the joint specification prints per-layer sections (all separable)

    Code
      print(js)
    Message
      -- <joint_specification.goldfish> ----------------------------------------------
      2 processes over one shared data object · 6 formulas · 0 coupled · 6 separable
      
      Layer "calls" — Model "DyNAM"
      Flavor "creation"
      * Rate [fid 1, separable]: `~1 + indeg`
      * Choice [fid 2, separable]: `~inertia + tie(friendship)`
        Derived: `~!tie(calls)`
      Flavor "dissolution"
      * Rate [fid 3, separable]: `~1 + indeg`
      * Choice [fid 4, separable]: `~inertia + tie(friendship)`
        Derived: `~tie(calls)`
      
      Layer "emails" — Model "DyNAM"
      * Rate [fid 5, separable]: `~1 + indeg`
      * Choice [fid 6, separable]: `~inertia`
      
      i Estimate a multivariate specification with
      `estimate_dynes()`.

# the print marks coupled fids against a modeled panel layer

    Code
      print(js)
    Message
      -- <joint_specification.goldfish> ----------------------------------------------
      2 processes over one shared data object · 2 formulas · 2 coupled · 0 separable
      
      Layer "friendship" — Model "DyNAM"
      * Choice [fid 1, coupled]: `~inertia`
      
      Layer "calls" — Model "DyNAM"
      * Choice [fid 2, coupled]: `~inertia`
        Support: `~!tie(friendship)`
      
      i Estimate a multivariate specification with
      `estimate_dynes()`.

