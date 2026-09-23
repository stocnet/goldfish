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

