# walk_replay_to refuses to move backward

    Code
      walk_replay_to(handle, 4L)
    Condition
      Error:
      ! `walk_replay_to()` cannot move the replay backward.
      x Rows up to 7 are applied; asked to stop before row 4.

# a candidate whose info differs is refused naming info

    Code
      bind_candidate_data(rq_joint_spec(), candidate)
    Condition
      Error:
      ! The candidate sequence does not match the specification's data in info.
      i A candidate may differ from the observed data only in its ties.

# a candidate whose nodes differ is refused naming nodes

    Code
      bind_candidate_data(rq_joint_spec(), candidate)
    Condition
      Error:
      ! The candidate sequence does not match the specification's data in nodes.
      i A candidate may differ from the observed data only in its ties.

# a candidate with another composition is refused naming it

    Code
      bind_candidate_data(spec, candidate)
    Condition
      Error:
      ! The candidate sequence does not match the specification's data in composition.
      i `estimate_dynes()` may have snapped the composition to the panel waves, so a fit's specification and sequences carry a composition the authored data does not.
      i Pair `fit$spec` with the fit's sequences, or the authored specification with the original data.

# sub-model positions off the free layout abort naming the mismatch

    Code
      assert_theta_layout(spec, shifted)
    Condition
      Error:
      ! The sub-models' theta_index positions do not match the specification's free coefficient layout.
      x No sub-model claims free position 4.
      x The layout has no free slot at position 7.
      i Number each sub-model's free effects in `coef_layout()` order.

# an unknown actor label aborts naming the query

    Code
      rq_pair(spec, theta, "calls", "N2", "N9")
    Condition
      Error in `rq_pair()`:
      ! Cannot answer the rate query "calls: N2 -> N9".
      x The receiver "N9" is not a node of the layer.

# an unknown process aborts naming the query

    Code
      rq_pair(spec, theta, "friendship", "N1", "N2")
    Condition
      Error in `rq_pair()`:
      ! Cannot answer the rate query "friendship: N1 -> N2".
      x The specification carries no process "friendship".

# an ordered or coordination process aborts naming its regime

    Code
      rq_pair(ordered, list(), "calls", "N1", "N2")
    Condition
      Error in `rq_pair()`:
      ! Cannot answer the rate query "calls: N1 -> N2".
      x Its rate is ordered; only a timed rate gives an intensity.

---

    Code
      rq_pair(coordination, list(), "calls", "N1", "N2")
    Condition
      Error in `rq_pair()`:
      ! Cannot answer the rate query "calls: N1 -> N2".
      x Its choice is coordinated; a coordination process has no single sender's rate to draw a waiting time at.

# a position outside the candidate's events aborts naming it

    Code
      query_rate_at_position(spec, rq_exo_theta(), queries)
    Condition
      Error:
      ! Cannot answer the rate query "calls: N1 -> N2".
      x Its position `after = 7` is not one of the candidate's 0:6 events.

