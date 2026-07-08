# the row-reduction message reads correctly [plain]

    Code
      inform_dyadic_sender_reduction(atom_labels = c("tie(net)", "alter(x)"),
      atom_kinds = c(0L, 1L))
    Message
      i `support_constraint`: the dyadic atoms `tie(net)` and `alter(x)` are consumed on the sender axis by row-reduction — a sender is at risk iff it has at least one allowed, present receiver.
      i For a cheaper sender-axis formulation, use an ego-kind atom, e.g. `~ tie(net)` becomes `~ outdeg(net) > 0`.
      ! That reformulation is equivalent only under static receiver composition: `outdeg()` counts ties to absent receivers, the row-reduction does not.

