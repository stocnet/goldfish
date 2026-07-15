# missing / unnamed / short update coverage aborts

    Code
      validate_goldfish_data(no_update)
    Condition
      Error:
      ! info$update is required for every layer.
      x It is missing.
      i Provide a named vector covering layer "calls".

---

    Code
      validate_goldfish_data(unnamed)
    Condition
      Error:
      ! info$update must be named by layer.
      i manynet accepts an unnamed vector; goldfish requires one entry per layer, named.

---

    Code
      validate_goldfish_data(short)
    Condition
      Error:
      ! info$update must cover every layer in ties.
      x No update entry for layer "sms".

# duplicate node labels abort

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! nodes$label must be unique.
      x Duplicated label: "A".

# non-syntactic layer name aborts with a rename suggestion

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! Every layer name must be a syntactic R name.
      x Non-syntactic name: "phone calls".
      i Rename, e.g. "phone calls" to "phone.calls", so it can be used in formulas without backticks.

# unmodeled observation type aborts

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! goldfish models "event" and "panel" layers only.
      x Layer "calls" declares observation = "cross-sectional".

# character time aborts with conversion guidance

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! ties$time has an unsupported class <character>.
      i Convert time to numeric, POSIXct, or Date.

# mixed time axes abort

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! All streams must share a comparable time axis.
      x Mixed axes: ties and changes on "temporal" and "numeric".
      i Integer/numeric wave times are only comparable when every stream uses them; convert to a common POSIXct/Date axis.

# panel focal layer aborts

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! A "panel" layer cannot be the focal (dependent) process.
      x Layer "calls" is declared observation = "panel".
      i goldfish models event-stream dependents; panel-dependent processes are SAOM/RSiena territory.

# partially overlapping mode sets abort

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! sender/receiver mode sets must be identical (one-mode subset) or disjoint (two-mode).
      x On layer "membership" the sender set ("p") and receiver set ("p" and "o") partially overlap.
      i Express such a design as an identical-set one-mode layer plus a `support_constraint()`.

# side-impure ties abort

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! Ties must be side-pure for a declared two-mode/subset layer.
      x On layer "membership", node "3" falls outside the declared sender/receiver mode sets.

# out-of-range node index aborts

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! ties$from must index rows of nodes (1..3).
      x Out-of-range values: 9.

# non-logical active values abort

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! changes active values must be logical.
      x Found type: "integer".

# non-syntactic flavor values abort

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! ties$flavor values must be syntactic R names.
      x Non-syntactic value: "un create".
      i Flavor values appear as formula-list keys.

# side impurity reports every offending node

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! Ties must be side-pure for a declared two-mode/subset layer.
      x On layer "membership", nodes "3" and "4" fall outside the declared sender/receiver mode sets.

# a partial overlap on one layer aborts naming that layer

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! sender/receiver mode sets must be identical (one-mode subset) or disjoint (two-mode).
      x On layer "advice" the sender set ("employee") and receiver set ("employee" and "supervisor") partially overlap.
      i Express such a design as an identical-set one-mode layer plus a `support_constraint()`.

# sender/receiver naming an absent layer aborts

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! sender/receiver must name layers present in ties$layer.
      x Unknown layer: "gossip".

# a list of per-layer mode sets aborts with the vector form

    Code
      validate_goldfish_data(x)
    Condition
      Error:
      ! info$sender and info$receiver must be a character vector, not a list.
      i Name each mode with its layer, repeating the layer name to give it several modes:
        `c(survey = "employees", survey = "supervisor", report = "employees")`
      i A list is rejected by `manynet::make_stocnet()` and `manynet::bind_changes()`, so it would fail later.

