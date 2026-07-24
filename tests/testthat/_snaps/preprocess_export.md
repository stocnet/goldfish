# gather_model_data() is soft-deprecated onto compute_statistics()

    Code
      invisible(gather_model_data(depNetwork ~ inertia(networkState), data = dataTest))
    Condition
      Warning:
      `gather_model_data()` was deprecated in goldfish 2.0.0.
      i Please use compute_statistics(output = "gather") instead.

