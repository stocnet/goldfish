* Internal: `support_constraint` atoms are maintained on the single merged
  preprocessing walk as `role = "constraint"` plan effects, replacing their
  separate private atom walk for constraints the shared walk covers;
  preprocessed statistics and masks are byte-identical.
