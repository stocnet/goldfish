# as_category on a numeric attribute aborts

    Code
      suppressWarnings(estimate_dynam(contact ~ alter(income), sub_model = "choice",
      data = as_goldfish(fixture), preprocessing_only = TRUE, control_preprocessing = set_preprocessing_opt(
        impute = c(income = "as_category"))))
    Condition
      Error in `validate_imputation_policy()`:
      ! The as-category policy applies only to factor or character attributes.
      x "income" is a numeric attribute.
      i Impute a numeric attribute with the default summary policy.

# a policy naming an unread attribute aborts

    Code
      suppressWarnings(estimate_dynam(contact ~ same(party), sub_model = "choice",
      data = as_goldfish(fixture), preprocessing_only = TRUE, control_preprocessing = set_preprocessing_opt(
        impute = c(nowhere = "as_category"))))
    Condition
      Error in `validate_imputation_policy()`:
      ! Imputation policy names attribute "nowhere", which no effect reads.
      i Name only a nodal attribute an effect in the formula uses.

# a reserved-level collision aborts

    Code
      ds_impute_missing(src, party_effects_link(), policy = c(party = "as_category"))
    Condition
      Error in `ds_impute_missing()`:
      ! The as-category reserved level "(missing)" already occurs in attribute "party".
      x Recoding missing values to it would collide with an observed value.
      i Rename the observed level, or impute this attribute with the default summary policy.

# the as_category policy requires stocnet data objects

    Code
      ds_impute_missing(src, party_effects_link(), policy = c(party = "as_category"))
    Condition
      Error in `ds_impute_missing()`:
      ! The as-category imputation policy requires stocnet data objects.
      x It cannot be applied on the legacy environment data path.
      i Build the data with `goldfish_data()` / `as_goldfish()` to use `impute = c(... = "as_category")`.

