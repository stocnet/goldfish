#' International bilateral fisheries treaties (1960-1970)
#'
#' An abbreviated version of the international fisheries agreements dataset,
#' including only bilateral agreements, fewer variables,
#' and ranging only between 1960 and 1970 inclusive.
#' This data set is only meant for testing, and not for inference.
#' It provides an example of an undirected, weighted (by integer/increment)
#' network, with composition change and both monadic and dyadic covariates.
#' Monadic variables include the dates states gain or lose sovereign status,
#' their polity score, and their GDP.
#' Dyadic variables include bilateral fisheries agreements between states,
#' and states' contiguity with one another over time.
#'
#' @name Fisheries_Treaties_6070
#' @docType data
#' @usage data(Fisheries_Treaties_6070)
#' @format The data includes several dataframes:
#' states (154 rows, 4 columns, monadic),
#' sovchanges (62 rows, 3 columns, monadic),
#' regchanges (145 rows, 3 columns, monadic),
#' gdpchanges (979 rows, 3 columns, monadic),
#' bilatchanges (77 rows, 4 columns, dyadic),
#' contigchanges (139 rows, 4 columns, dyadic).
#' See below for variables and formats.
#'
#' \tabular{lll}{
#'    \strong{Object} \tab \strong{Description} \tab \strong{Format} \cr
#'    states$label \tab Node identifier labels \tab character \cr
#'    states$present \tab Node present in dataset \tab boolean \cr
#'    states$regime \tab Placeholder for regime variable \tab numeric (NA) \cr
#'    states$gdp \tab Placeholder for GDP variable \tab numeric (NA) \cr
#'    sovchanges$time \tab Date of state sovereignty update \tab POSIXct \cr
#'    sovchanges$node \tab Node for state sovereignty update \tab integer \cr
#'    sovchanges$replace \tab State sovereignty update \tab boolean \cr
#'    regchanges$time \tab Date of regime update \tab POSIXct \cr
#'    regchanges$node \tab Node for regime update \tab integer \cr
#'    regchanges$replace \tab Regime update \tab integer (-10--10) \cr
#'    gdpchanges$time \tab Date of GDP update \tab POSIXct \cr
#'    gdpchanges$node \tab Node for GDP update \tab integer \cr
#'    gdpchanges$replace \tab GDP update \tab numeric \cr
#'    bilatchanges$time \tab Date of bilateral change \tab POSIXct \cr
#'    bilatchanges$sender \tab First bilateral change node \tab integer \cr
#'    bilatchanges$receiver \tab Second bilateral change node \tab integer \cr
#'    bilatchanges$increment\tab Create or dissolve tie\tab numeric (-1 or 1)\cr
#'    contigchanges$time \tab Date of contiguity change \tab POSIXct \cr
#'    contigchanges$sender \tab First contiguity change node \tab integer \cr
#'    contigchanges$receiver \tab Second contiguity change node \tab integer \cr
#'    contigchanges$replace \tab New contiguity value \tab numeric \cr
#'  }
#'
#' @references
#' Hollway, James, and Johan Koskinen. 2016.
#' Multilevel Embeddedness: The Case of the Global Fisheries Governance Complex.
#' \emph{Social Networks}, 44: 281-94. \doi{10.1016/j.socnet.2015.03.001}.
#'
#' Hollway, James, and Johan H Koskinen. 2016.
#' Multilevel Bilateralism and Multilateralism: States' Bilateral and
#' Multilateral Fisheries Treaties and Their Secretariats.
#' In \emph{Multilevel Network Analysis for the Social Sciences},
#' edited by Emmanuel Lazega and Tom A B Snijders,
#' 315-32. Cham: Springer International Publishing.
#' \doi{10.1007/978-3-319-24520-1_13}.
#'
#' @keywords datasets dynamic political network states fisheries
NULL

#' @rdname Fisheries_Treaties_6070
"bilatchanges"
#' @rdname Fisheries_Treaties_6070
"bilatnet"
#' @rdname Fisheries_Treaties_6070
"contigchanges"
#' @rdname Fisheries_Treaties_6070
"contignet"
#' @rdname Fisheries_Treaties_6070
"gdpchanges"
#' @rdname Fisheries_Treaties_6070
"regchanges"
#' @rdname Fisheries_Treaties_6070
"sovchanges"
#' @rdname Fisheries_Treaties_6070
"states"

#' Fisheries treaties as a single stocnet object
#'
#' The [Fisheries_Treaties_6070] data assembled into one `stocnet` object (a
#' plain list of tibbles, as produced by `manynet::make_stocnet()`), the data
#' shape goldfish consumes directly through the `data` argument of
#' [estimate_dynam()], [estimate_rem()], and [make_specification()]. It carries
#' two undirected layers over a single node set: `treaties`, an **event** layer
#' whose `increment` updates record a treaty signed (`+1`) or ended (`-1`)
#' between two states, and `contiguity`, an **event** layer of `replace` updates.
#' Node-level covariates (`gdp`, `active`/sovereignty, `regime`) arrive as
#' attribute change streams in `changes`. The focal (dependent) layer is
#' `treaties`.
#'
#' **The tie is a treaty count, not a treaty's presence.** States sign with the
#' same partner repeatedly — Russia and the USA sign fourteen times over five
#' years against a single ending — so the `increment` updates accumulate rather
#' than toggling a tie on and off. Signings and endings are therefore
#' `flavor_style = "redundant"`: repeated same-direction events are meaningful,
#' and no support constraint restricts either process.
#'
#' The object ships **unflavored**. To model the two processes separately, stamp
#' the flavors yourself with [add_flavor()] — see the examples. That is a
#' modeling decision about how to read the update values, so it belongs in the
#' analysis rather than baked into the data.
#'
#' @name fisheries_treaties
#' @docType data
#' @usage data(fisheries_treaties)
#' @format A `stocnet` list of five components:
#' \describe{
#'   \item{info}{layer metadata: `name`, `layers` (`"treaties"`,
#'     `"contiguity"`), per-layer `update` (`treaties = "increment"`,
#'     `contiguity = "replace"`), `directed` (both `FALSE`), `observation` (both
#'     `"event"`), `focal = "treaties"`, and the `gdp`/`active`/`regime`
#'     attribute-update metadata.}
#'   \item{nodes}{154 states (`label`, `active`, `regime`, `gdp`).}
#'   \item{ties}{413 rows (`from`, `to`, `weight`, `time`, `layer`,
#'     `order`) stacking the treaty and contiguity history and events; `from`/`to`
#'     index rows of `nodes`. The reserved integer `order` column pins the
#'     original sequence of the contiguity `replace` events, whose same-time
#'     same-dyad collisions would otherwise be ambiguous. There is no `flavor`
#'     column: [add_flavor()] stamps one.}
#'   \item{changes}{1186 rows (`time`, `node`, `var`, `value`) of the gdp,
#'     active, and regime attribute updates.}
#'   \item{global}{`NULL` (no global attribute stream).}
#' }
#'
#' @seealso [Fisheries_Treaties_6070] for the raw objects and the source
#'   citations; [make_specification()] for the flavor-keyed list syntax.
#'
#' @references
#' Hollway, James, and Johan Koskinen. 2016.
#' Multilevel Embeddedness: The Case of the Global Fisheries Governance Complex.
#' \emph{Social Networks}, 44: 281-94. \doi{10.1016/j.socnet.2015.03.001}.
#'
#' @examples
#' # Construction workflow (how the shipped object is built from the raw
#' # objects):
#' data("Fisheries_Treaties_6070")
#' treaties <- manynet::bind_ties(
#'   manynet::rename_nodes(manynet::join_nodes(
#'     manynet::as_stocnet(bilatnet), states # bilatnet is the history matrix
#'   )),
#'   bilatchanges
#' )
#' # `order` pins the sequence of the same-time contiguity replace events, whose
#' # same-dyad collisions would otherwise be an ambiguous tie-break:
#' contigchanges$order <- seq_len(nrow(contigchanges))
#' contiguity <- manynet::bind_ties(manynet::as_stocnet(contignet), contigchanges)
#' fish <- manynet::from_ties(treaties = treaties, contiguity = contiguity)
#' fish <- manynet::add_info(
#'   fish,
#'   name = "Fisheries Treaties",
#'   focal = "treaties",
#'   directed = c(treaties = FALSE, contiguity = FALSE),
#'   observation = c(treaties = "event", contiguity = "event")
#' )
#' fish <- manynet::bind_changes(fish, changes = gdpchanges, var = "gdp")
#' fish <- manynet::bind_changes(fish, sovchanges, var = "active")
#' fish <- manynet::bind_changes(fish, regchanges, var = "regime")
#'
#' # Or load the prebuilt object and name the two processes. Signings and
#' # endings accumulate on a dyad rather than toggling a tie, so they are
#' # `redundant` and neither gets a support constraint:
#' data("fisheries_treaties")
#' fish <- add_flavor(
#'   fisheries_treaties,
#'   layer = "treaties",
#'   values_equivalence = c(signing = 1, ending = -1),
#'   flavor_style = "redundant"
#' )
#'
#' # Model signings alone: ending events still update the network state.
#' spec <- make_specification(
#'   choice = list(signing ~ inertia + trans),
#'   model = "DyNAM",
#'   choice_sub_model = "choice_coordination",
#'   data = fish
#' )
#'
#' @keywords datasets dynamic political network states fisheries
"fisheries_treaties"
