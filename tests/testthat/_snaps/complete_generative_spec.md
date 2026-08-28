# the completion warning snapshot names layer/family/default

    Code
      invisible(complete_generative_spec(js, consumer = "estimate_dynes"))
    Condition
      Warning:
      ! Layer "calls" has no choice sub-model; completing it with a uniform choice over the support-legal alternatives.
      i The default adds no free parameter; it is auto-supplied for the estimate_dynes generative surface.
      Warning:
      ! Layer "emails" has no rate sub-model; completing it with a pinned intercept-only rate (zero free parameters).
      i The default adds no free parameter; it is auto-supplied for the estimate_dynes generative surface.

# a mixed ordered+timed composition aborts at join time

    Code
      make_joint_specification(timed, ordered, data = data)
    Condition
      Error in `make_joint_specification()`:
      ! A joint specification cannot mix timed and ordered processes.
      x It carries both a waiting-time rate (sub_model = "rate") and an ordered rate (sub_model = "rate_ordered").
      i Compose processes of one regime -- all timed (waiting-time rates) or all ordered.

# completion is scoped by type to a joint specification

    Code
      complete_generative_spec(spec)
    Condition
      Error:
      ! `joint_spec` must be a <goldfishJointSpec>.
      i Generative completion is scoped to the joint (generative) surface; the single-process path keeps rate-only / choice-only specifications unchanged.

# a modeled panel layer missing a whole flavor aborts

    Code
      complete_generative_spec(js, wave_times = c(0, 2, 5))
    Condition
      Error:
      ! A modeled panel layer must model all of its flavors.
      x Layer "friendship" carries flavor "dissolution" modeled in neither `rate` nor `choice`.
      i A modeled panel layer's augmented path must place events of every flavor its wave-diff produces; model "dissolution" or drop it from the panel layer.

