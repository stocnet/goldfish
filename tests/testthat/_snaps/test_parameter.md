# what cannot be tested says how to make it testable

    Code
      test_parameter(parameter_fixture(calls ~ inertia + recip))
    Condition
      Error in `test_parameter()`:
      ! This fit holds no coefficient at an imposed value.
      x Every term was estimated, and an estimated coefficient's score is zero at the maximum — there is nothing left to test.
      i Put the term in the model held at the value you want to test: `offset(term, coef = 0)`. Its statistics are preprocessed in the same pass, and the resulting fit is the constrained one this test needs.
      i Testing a term absent from the formula would instead need a preprocessing pass over the whole event sequence, which is why it is not offered.

---

    Code
      test_parameter(parameter_fixture(), effects = "common_receiver")
    Condition
      Error in `test_parameter()`:
      ! `effects` names a term this model does not contain.
      x A term absent from the formula has no statistics, so there is no score to test it with.
      i Put the term in the model held at the value you want to test: `offset(term, coef = 0)`. Its statistics are preprocessed in the same pass, and the resulting fit is the constrained one this test needs.
      i Testing a term absent from the formula would instead need a preprocessing pass over the whole event sequence, which is why it is not offered.
      Caused by error in `test_parameter()`:
      ! `effects` names a term this model does not have.
      x Unknown: "common_receiver".
      i Available: "inertia/calls", "recip/calls [Fx]", and "trans/calls".
      i An effect name selects all of its terms: "inertia", "recip", and "trans".
      i Search them with `model_terms(fit, pattern = )`.

---

    Code
      test_parameter(parameter_fixture(), effects = "inertia/calls")
    Condition
      Error in `test_parameter()`:
      ! `effects` names 1 estimated coefficient: "inertia/calls".
      x This test reads the score left over at a value the formula imposed, and an estimated coefficient's score is zero at the maximum by construction.
      i `test_gof()` tests whether an estimated effect's contribution is spread over the sequence as the model assumes.

---

    Code
      test_parameter(parameter_fixture(return_preprocessed = FALSE))
    Condition
      Error in `test_parameter()`:
      ! This diagnostic needs the preprocessed statistics of the model, which this fit does not carry.
      i Re-estimate with `return_preprocessed = TRUE`, or
      i supply `preprocessed = compute_statistics(..., output = "preprocessed")`.

# print reports the joint test and the per-term rows

    Code
      header(test_parameter(fit))
    Message
      -- <test_parameter> ------------------------------------------------------------
      Model "DyNAM" · sub-model "choice" · backend "cpp"
      Score test of 1 coefficient held at an imposed value, over 439 events.
      Joint: LM = 70.33 on 1 degree of freedom, p = <2e-16
    Output
      

