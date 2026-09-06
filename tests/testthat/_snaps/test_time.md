# both methods need the statistics and say so

    Code
      test_time(fit)
    Condition
      Error in `test_time()`:
      ! This diagnostic needs the preprocessed statistics of the model, which this fit does not carry.
      i Re-estimate with `return_preprocessed = TRUE`, or
      i supply `preprocessed = compute_statistics(..., output = "preprocessed")`.

---

    Code
      test_time(fit, method = "periods")
    Condition
      Error in `test_time()`:
      ! This diagnostic needs the preprocessed statistics of the model, which this fit does not carry.
      i Re-estimate with `return_preprocessed = TRUE`, or
      i supply `preprocessed = compute_statistics(..., output = "preprocessed")`.

# a fixed coefficient is excluded, and naming one has a destination

    Code
      test_time(fit, effects = "recip")
    Condition
      Error in `test_time()`:
      ! `effects` names 1 term held fixed through `offset()`: "recip/calls [Fx]".
      x This test asks whether an estimated coefficient stayed put, and an imposed one never moved to begin with.
      i `test_parameter()` tests a coefficient at the value `offset()` imposed.

# test_time rejects a grouping it cannot use

    Code
      test_time(fit, method = "periods", periods = 1L)
    Condition
      Error in `test_time()`:
      ! `periods` must ask for at least two periods.
      x It asks for 1.

---

    Code
      test_time(fit, method = "periods", periods = c(-1e+09, 1e+09))
    Condition
      Error in `test_time()`:
      ! `periods` defines a single period.
      x There is nothing to compare it against.
      i Supply cut times inside the observation window, or a count of two or more.

---

    Code
      test_time(fit, method = "periods", periods = c("a", "b"))
    Condition
      Error in `test_time()`:
      ! `periods` must be a count, cut times, or a grouping.

# the print reports the effects and the joint test

    Code
      print(test_time(fit))
    Message
      -- <goldfishTimeTest> ----------------------------------------------------------
      Model "DyNAM" · sub-model "choice" · backend "cpp"
      439 intervals, 439 dependent events; 2 effects tested.
      Score test of a "identity" time trend in each coefficient.
    Output
      # A tibble: 2 x 7
        index term          coefficient statistic    df p_value  rank
        <int> <chr>         <chr>           <dbl> <int>   <dbl> <int>
      1     1 inertia/calls inrt             6.28     1 0.0122      2
      2     2 recip/calls   rec              7.43     1 0.00641     1
    Message
      
      Joint test: chi-squared 10.66 on 2 df, p 0.004834.

---

    Code
      print(test_time(fit, method = "periods", periods = 2L))
    Message
      -- <goldfishTimeTest> ----------------------------------------------------------
      Model "DyNAM" · sub-model "choice" · backend "cpp"
      439 intervals, 439 dependent events; 2 effects tested.
      Score test of a coefficient difference across 2 periods.
    Output
      # A tibble: 2 x 7
        index term          coefficient statistic    df p_value  rank
        <int> <chr>         <chr>           <dbl> <int>   <dbl> <int>
      1     1 inertia/calls inrt             3.09     1  0.0787     1
      2     2 recip/calls   rec              1.29     1  0.255      2
    Message
      
      Joint test: chi-squared 3.259 on 2 df, p 0.196.

# the blocked print names the processes and reports no joint test

    Code
      print(test_time(time_container()))
    Message
      i The fit carries the preprocessed statistics it was estimated from (about 84.2 Kb).
      i The fit carries the preprocessed statistics it was estimated from (about 84.2 Kb).
      i The fit carries the preprocessed statistics it was estimated from (about 54.2 Kb).
      i The fit carries the preprocessed statistics it was estimated from (about 57.2 Kb).
      -- <goldfishTimeTest> ----------------------------------------------------------
      Model "DyNAM" · layer "calls" · 2 flavors over 4 processes
      Score test of a "identity" time trend in each coefficient.
    Output
      # A tibble: 6 x 9
        index term        coefficient statistic    df p_value  rank flavor      family
        <int> <chr>       <chr>           <dbl> <int>   <dbl> <int> <chr>       <chr> 
      1     1 Intercept   Intercept     0.0731      1  0.787      2 creation    rate  
      2     2 indeg/calls ideg          0.912       1  0.340      1 creation    rate  
      3     1 trans/calls trans         0.00374     1  0.951      1 creation    choice
      4     1 Intercept   Intercept     0.380       1  0.538      2 dissolution rate  
      5     2 indeg/calls ideg          1.70        1  0.192      1 dissolution rate  
      6     1 trans/calls trans         4.75        1  0.0292     1 dissolution choice

