# layers argument errors are reported before any row is rewritten

    Code
      prepare_dynes_data(x, layers = character())
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` must name at least one layer.
      i For example `layers = c(friendship = "mutually_exclusive")`.

---

    Code
      prepare_dynes_data(x, layers = "friendship")
    Condition
      Error in `prepare_dynes_data()`:
      ! Every entry of `layers` must be named by its layer.
      x Every layer needs a declared style, one of "mutually_exclusive", "count", and "none".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "count", friendship = "none"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` names a layer more than once.
      x Repeated layer: "friendship".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "ordinal"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` must use one of "mutually_exclusive", "count", and "none".
      x Not in the vocabulary: "ordinal".

---

    Code
      prepare_dynes_data(x, layers = c(missing = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `layers` names a layer that is not in ties.
      x Absent layer: "missing".
      i The layers of `x` are "friendship", "calls", "messages", and "advice".

---

    Code
      prepare_dynes_data(x, layers = c(messages = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Only panel layers can be prepared.
      x Layer not observed as "panel": "messages".
      i Event layers keep their times; use `add_flavor()` to flavor one.

# form argument errors are reported before any row is rewritten

    Code
      prepare_dynes_data(x, layers = c(friendship = "count"), form = c(friendship = "sideways"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `form` must use one of "states", "per_wave", and "changes".
      x Not in the vocabulary: "sideways".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "count"), form = c(calls = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `form` names a layer that is not in `layers`.
      x Layer not in `layers`: "calls".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "count"), form = c(friendship = "states",
        friendship = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! `form` names a layer more than once.
      x Repeated layer: "friendship".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive"), form = c(
        friendship = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! A binary tie has no amount per wave.
      x Layer "friendship" is declared "mutually_exclusive" with form "per_wave".
      i Declare the form "states" or "changes", or the style "count".

# flavor_names errors are reported before any row is rewritten

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(calls = roles))
    Condition
      Error in `prepare_dynes_data()`:
      ! `flavor_names` names a layer that is not in `layers`.
      x Layer not in `layers`: "calls".

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive", advice = "none"),
      flavor_names = list(advice = roles))
    Condition
      Error in `prepare_dynes_data()`:
      ! Only a "mutually_exclusive" layer has flavors.
      x Layer "advice" is declared "none".

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = c(creation = "formed",
        other = "ended")))
    Condition
      Error in `prepare_dynes_data()`:
      ! The `flavor_names` entry for "friendship" must be named by the roles "creation" and "dissolution".
      i For example `c(creation = "formed", dissolution = "ended")`.

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = c(creation = "not valid",
        dissolution = "ended")))
    Condition
      Error in `prepare_dynes_data()`:
      ! Flavor labels must be syntactic R names.
      x Layer "friendship" has non-syntactic label: "not valid".
      i Flavor labels key `make_specification()` formula lists.

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = c(creation = "same",
        dissolution = "same")))
    Condition
      Error in `prepare_dynes_data()`:
      ! The two flavor labels must differ.
      x Layer "friendship" labels both flavors "same".

---

    Code
      prepare_dynes_data(x, layers = me, flavor_names = list(friendship = roles,
        friendship = roles))
    Condition
      Error in `prepare_dynes_data()`:
      ! `flavor_names` names a layer more than once.
      x Repeated layer: "friendship".

# a flavor for a count layer aborts with the count guidance

    Code
      prepare_dynes_data(x, layers = c(calls = "count"), flavor_names = list(calls = c(
        creation = "signed", dissolution = "ended")))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count layer needs no flavor.
      x `flavor_names` has an entry for the count layer "calls".
      i A count records repeated events of one process.
      i If "calls" counts two event types together, such as signings and endings, split it into one "count" layer per type.

# a dyad repeated at one wave aborts

    Code
      check_rows(x)
    Condition
      Error in `check_rows()`:
      ! A wave must hold one value per dyad.
      x Layer "survey" reports dyad "C-G" 2 times at wave "10".

---

    Code
      check_rows(x, "per_wave")
    Condition
      Error in `check_rows()`:
      ! A wave must hold one value per dyad.
      x Layer "survey" reports dyad "C-G" 2 times at wave "10".

# an NA tie value aborts

    Code
      check_rows(x)
    Condition
      Error in `check_rows()`:
      ! Layer "survey" has a missing tie value.
      x Dyad "C-G" at wave "10" is NA.
      i A wave must report every tie it holds; partially observed waves are not supported.

---

    Code
      check_rows(x, "per_wave")
    Condition
      Error in `check_rows()`:
      ! Layer "survey" has a missing tie value.
      x Dyad "C-G" at wave "10" is NA.
      i A wave must report every tie it holds; partially observed waves are not supported.

# a self-tie on a one-mode layer aborts

    Code
      check_rows(x)
    Condition
      Error in `check_rows()`:
      ! Layer "survey" has a tie from a node to itself.
      x Node "D" at wave "10".
      i A one-mode layer has no diagonal.

# an undirected tie reported both ways warns and counts once

    Code
      out <- check_rows(x)
    Condition
      Warning in `check_rows()`:
      Layer "survey" reports 1 undirected dyad in both orders with the same value.
      i First: "C-G" at wave "10".
      i Each is counted once, not twice.

# an undirected tie reported both ways with two values aborts

    Code
      check_rows(x)
    Condition
      Error in `check_rows()`:
      ! A wave must hold one value per dyad.
      x Layer "survey" reports dyad "C-G" 2 times at wave "10".
      i The two reports of this undirected tie disagree: 1 and 2.

# a mutually exclusive state other than 0 or 1 aborts

    Code
      prepare_dynes_data(x, layers = c(survey = "mutually_exclusive"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" reports a value its style "mutually_exclusive" cannot hold.
      x Dyad "C-G" at wave "10" is 2.
      i A binary tie is present (1) or absent (0); declare the style "count" or "none" for a valued layer.

# a mutually exclusive change outside the update encoding aborts

    Code
      prepare_dynes_data(x, layers = c(survey = "mutually_exclusive"), form = c(
        survey = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" is a change-list that does not suit its style "mutually_exclusive".
      x Dyad "C-G" at wave "10": the value 2 is not an update of a binary tie.
      i Under "replace" a binary change is 0 and 1.

# a mutually exclusive change-list contradicting the style aborts

    Code
      prepare_dynes_data(twice_created, layers = c(survey = "mutually_exclusive"),
      form = c(survey = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" is a change-list that does not suit its style "mutually_exclusive".
      x Dyad "C-G" at wave "20": a creation where the tie is already present.
      i Each event of a "mutually_exclusive" layer changes the tie's state; declare `form = "states"` if the rows are the ties reported at each wave.

---

    Code
      prepare_dynes_data(dissolved_absent, layers = c(survey = "mutually_exclusive"),
      form = c(survey = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" is a change-list that does not suit its style "mutually_exclusive".
      x Dyad "C-G" at wave "10": a dissolution where the tie is absent.
      i Each event of a "mutually_exclusive" layer changes the tie's state; declare `form = "states"` if the rows are the ties reported at each wave.

# a fractional or negative count aborts

    Code
      prepare_dynes_data(x, layers = c(survey = "count"), form = c(survey = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" reports a value its style "count" cannot hold.
      x Dyad "C-G" at wave "10" is 1.5.
      i A count is a whole non-negative number.

---

    Code
      prepare_dynes_data(x, layers = c(survey = "count"), form = c(survey = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" reports a value its style "count" cannot hold.
      x Dyad "C-G" at wave "10" is -1.
      i A count is a whole non-negative number.

# a count change-list that falls aborts in its update semantics

    Code
      prepare_dynes_data(negative, layers = c(survey = "count"), form = c(survey = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" is a change-list that does not suit its style "count".
      x Dyad "C-G" at wave "20": the value -2 is not a whole number of at least 0.
      i A count rises by whole events and never falls.

---

    Code
      prepare_dynes_data(falling, layers = c(survey = "count"), form = c(survey = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" is a change-list that does not suit its style "count".
      x Dyad "C-G" at wave "20": the level falls from 6 to 4.
      i A count level never falls; the level before the first row is folded from the `time = NA` rows.

---

    Code
      prepare_dynes_data(below_start, layers = c(survey = "count"), form = c(survey = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" is a change-list that does not suit its style "count".
      x Dyad "C-G" at wave "10": the level falls from 6 to 4.
      i A count level never falls; the level before the first row is folded from the `time = NA` rows.

# a count total that falls aborts, naming the wave and the dyad

    Code
      prepare_dynes_data(falling, layers = c(survey = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count total cannot fall.
      x Layer "survey": dyad "C-G" falls from 6 to 4 at wave "20".
      i A dyad missing from a wave has a total of 0 there. If the values are amounts since the previous wave, declare `form = "per_wave"`.

---

    Code
      prepare_dynes_data(dropped, layers = c(survey = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count total cannot fall.
      x Layer "survey": dyad "C-G" falls from 6 to 0 at wave "20".
      i A dyad missing from a wave has a total of 0 there. If the values are amounts since the previous wave, declare `form = "per_wave"`.

# a count total missing at a middle wave aborts as a fall to 0

    Code
      prepare_dynes_data(skipped, layers = c(survey = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count total cannot fall.
      x Layer "survey": dyad "C-G" falls from 2 to 0 at wave "20".
      i A dyad missing from a wave has a total of 0 there. If the values are amounts since the previous wave, declare `form = "per_wave"`.

# a declared empty wave on a count states layer aborts as a fall

    Code
      prepare_dynes_data(x, layers = c(survey = "count"), wave_times = c(10, 20, 30))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count total cannot fall.
      x Layer "survey": dyad "C-G" falls from 2 to 0 at wave "20".
      i A dyad missing from a wave has a total of 0 there. If the values are amounts since the previous wave, declare `form = "per_wave"`.

# a mutually exclusive starting level other than 0 or 1 aborts

    Code
      prepare_dynes_data(x, layers = c(survey = "mutually_exclusive"))
    Condition
      Error in `prepare_dynes_data()`:
      ! The state of layer "survey" before its first wave does not suit its style "mutually_exclusive".
      x The `time = NA` rows fold dyad "C-G" to a level of 2.
      i A binary tie is present (1) or absent (0).

# a count total below its starting level aborts at the first wave

    Code
      prepare_dynes_data(x, layers = c(survey = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! A count total cannot fall.
      x Layer "survey": dyad "C-G" falls from 6 to 4 at wave "10".
      i The total before the first wave is folded from the `time = NA` rows.
      i A dyad missing from a wave has a total of 0 there. If the values are amounts since the previous wave, declare `form = "per_wave"`.

# a prepared layer cannot be prepared again

    Code
      prepare_dynes_data(out, layers = c(calls = "count"), form = c(calls = "changes"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "calls" is already prepared.
      x `prepare_dynes_data()` records the layers it rewrote in info$dynes_prepared.
      i Preparing a change-list again would read every stable tie missing from a later wave as a dissolution.

---

    Code
      prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "friendship" is already prepared.
      x `prepare_dynes_data()` records the layers it rewrote in info$dynes_prepared.
      i Preparing a change-list again would read every stable tie missing from a later wave as a dissolution.

# a layer that is already flavored aborts, naming every such layer

    Code
      prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive", advice = "none"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layers "friendship" and "advice" are already flavored.
      x `prepare_dynes_data()` writes the flavor map itself.
      i Pass the layers in unflavored: drop the info$values_equivalence entries.

# a flavored count layer gets the count guidance

    Code
      prepare_dynes_data(x, layers = c(calls = "count"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "calls" is already flavored.
      x `prepare_dynes_data()` writes the flavor map itself.
      i Pass the layer in unflavored: drop the info$values_equivalence entry.
      i A count layer needs no flavor: "calls" records repeated events of one process.
      i If a count layer counts two event types together, such as signings and endings, split it into one "count" layer per type.

# a row off the grid aborts, naming the layer and the time

    Code
      prepare_dynes_data(x, c(survey = "none"), wave_times = c(10, 20))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" has rows off its wave grid.
      x Row time on no wave: "15".
      i Its waves are "10" and "20".

---

    Code
      prepare_dynes_data(x, c(survey = "count"), form = c(survey = "per_wave"),
      wave_times = list(survey = c(10, 20)))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" has rows off its wave grid.
      x Row time on no wave: "15".
      i Its waves are "10" and "20".

# a grid of another time class aborts, naming both classes

    Code
      prepare_dynes_data(at_posix, layers, wave_times = as_date_days(c(10, 30)))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <Date>, but the time column of layer "survey" is <POSIXct>.
      i Give the grid as <POSIXct>: a <Date> counts days and a <POSIXct> seconds, so mixed classes compare different units.

---

    Code
      prepare_dynes_data(at_date, layers, wave_times = list(survey = as_posix_days(c(
        10, 30))))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <POSIXct>, but the time column of layer "survey" is <Date>.
      i Give the grid as <Date>: a <Date> counts days and a <POSIXct> seconds, so mixed classes compare different units.

# a character grid aborts, its hint naming no unit

    Code
      prepare_dynes_data(x, c(survey = "none"), wave_times = grid)
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` must have the time class of the rows it marks.
      x `wave_times` is <character>, but the time column of layer "survey" is <POSIXct>.
      i Convert the grid to <POSIXct> first; a <character> grid is never parsed.

# a character time column aborts at entry, before the grid check

    Code
      prepare_dynes_data(x, c(survey = "none"), wave_times = c(10, 30))
    Condition
      Error in `prepare_dynes_data()`:
      ! ties$time has an unsupported class <character>.
      i Convert time to numeric, POSIXct, or Date.

---

    Code
      prepare_dynes_data(x, c(survey = "none"))
    Condition
      Error in `prepare_dynes_data()`:
      ! ties$time has an unsupported class <character>.
      i Convert time to numeric, POSIXct, or Date.

# a malformed wave_times aborts before any row is rewritten

    Code
      prepare_dynes_data(x, c(friendship = "none"), wave_times = list(advice = c(10,
        30)))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` names a layer that is not in `layers`.
      x Layer not in `layers`: "advice".

---

    Code
      prepare_dynes_data(x, c(friendship = "none", advice = "none"), form = c(advice = "changes"),
      wave_times = list(advice = c(10, 30)))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` has an entry for a "changes" layer.
      x Layer "advice" is declared with form "changes", which has no waves to diff on.
      i Drop the entry, or declare the form "states" or "per_wave".

---

    Code
      prepare_dynes_data(x, c(friendship = "none"), wave_times = list(friendship = c(
        10, 20), c(10, 30)))
    Condition
      Error in `prepare_dynes_data()`:
      ! Every entry of `wave_times` must be named by its layer.
      i For example `wave_times = list(friendship = c(t1, t2))`.

---

    Code
      prepare_dynes_data(x, c(friendship = "none"), wave_times = c(10, NA))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` must hold finite times.
      x The grid of layer "friendship" holds "10" and "NA".

---

    Code
      prepare_dynes_data(x, c(friendship = "none"), wave_times = list(friendship = c(
        10, Inf)))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` must hold finite times.
      x The grid of layer "friendship" holds "10" and "Inf".

---

    Code
      prepare_dynes_data(x, c(friendship = "none", advice = "none"), form = c(
        friendship = "changes", advice = "changes"), wave_times = c(10, 20))
    Condition
      Error in `prepare_dynes_data()`:
      ! `wave_times` is given, but every layer has form "changes".
      i A change-list has no waves to diff on; drop `wave_times`.

# a states or per_wave layer with no wave aborts

    Code
      prepare_dynes_data(x, c(survey = "none"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" has no wave.
      x It has no timed rows and no `wave_times` entry.
      i Declare its waves in `wave_times`, or its form "changes" if its rows are history only.

---

    Code
      prepare_dynes_data(x, c(survey = "count"), form = c(survey = "per_wave"))
    Condition
      Error in `prepare_dynes_data()`:
      ! Layer "survey" has no wave.
      x It has no timed rows and no `wave_times` entry.
      i Declare its waves in `wave_times`, or its form "changes" if its rows are history only.

# a duplicated grid time warns once and is kept once

    Code
      twice <- prepare_quietly(x, layers = c(survey = "none"), wave_times = list(
        survey = c(10, 20, 20, 30)))
    Condition
      Warning in `prepare_dynes_data()`:
      `wave_times` lists a time more than once.
      * "survey" at "20"
      i Each time is kept once; check that no other wave was meant.

# an unchanged middle wave warns, naming the layer and the wave

    Code
      out <- prepare_quietly(x, layers = c(survey = "mutually_exclusive"))
    Condition
      Warning in `prepare_dynes_data()`:
      Some waves wrote no rows: nothing changed there.
      * "survey" at wave "20"
      i `estimate_dynes()` infers its waves from row times, so these waves would drop out and merge two intervals.
      i If this layer is modeled, pass `wave_times` to `estimate_dynes()`.

# a first wave equal to the time = NA rows warns, naming it

    Code
      out <- prepare_quietly(x, layers = c(survey = "none"))
    Condition
      Warning in `prepare_dynes_data()`:
      Some waves wrote no rows: nothing changed there.
      * "survey" at wave "10"
      i `estimate_dynes()` infers its waves from row times, so these waves would drop out and merge two intervals.
      i If this layer is modeled, pass `wave_times` to `estimate_dynes()`.

# empty waves on several layers raise exactly one warning

    Code
      out <- prepare()
    Condition
      Warning in `prepare_dynes_data()`:
      Some waves wrote no rows: nothing changed there.
      * "friendship" at wave "20"
      * "advice" at wave "30"
      i `estimate_dynes()` infers its waves from row times, so these waves would drop out and merge two intervals.
      i If any of these layers is modeled, pass `wave_times` to `estimate_dynes()`.

# a binary layer reports its flavors and its starting state

    Code
      out <- prepare()
    Message
      Prepared 1 layer; rows at the first wave are the starting state, not events.
      * "friendship" ("mutually_exclusive"): 70 formed, 70 ended.
        Starting state: 276 rows at the first wave "2008-09-09".

# each style reports its own totals, one line per layer

    Code
      out <- prepare_dynes_data(x, layers = c(friendship = "mutually_exclusive",
        calls = "count", advice = "none"), form = c(calls = "per_wave", advice = "changes"))
    Message
      Prepared 3 layers; rows at the first wave are the starting state, not events.
      * "friendship" ("mutually_exclusive"): 2 creation, 1 dissolution.
        Starting state: 2 rows at the first wave "10".
      * "calls" ("count"): 3 unit events on 1 dyad.
        Starting state: 1 row at the first wave "10".
      * "advice" ("none"): 1 changed cell.
        Starting state: 1 row at the first wave "10".

# a count reports unit events and changed dyads under either update

    Code
      out <- prepare_dynes_data(x, layers = c(survey = "count"))
    Message
      Prepared 1 layer; rows at the first wave are the starting state, not events.
      * "survey" ("count"): 7 unit events on 1 dyad.
        Starting state: 2 rows at the first wave "10".

---

    Code
      out <- prepare_dynes_data(x, layers = c(survey = "count"))
    Message
      Prepared 1 layer; rows at the first wave are the starting state, not events.
      * "survey" ("count"): 7 unit events on 1 dyad.
        Starting state: 2 rows at the first wave "10".

---

    Code
      out <- prepare_dynes_data(make_wave_fixture(changes, update = "increment"),
      layers = c(survey = "count"), form = c(survey = "changes"))
    Message
      Prepared 1 layer; rows at the first wave are the starting state, not events.
      * "survey" ("count"): 4 unit events on 2 dyads.
        Starting state: 1 row at the first wave "10".

# an increment count with history rows counts its rises from it

    Code
      out <- prepare_dynes_data(x, layers = c(survey = "count"))
    Message
      Prepared 1 layer; rows at the first wave are the starting state, not events.
      * "survey" ("count"): 3 unit events on 1 dyad.
        Starting state: 1 row at the first wave "10".

