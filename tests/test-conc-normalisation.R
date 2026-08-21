## Tests for the revenue-normalised CoNC^op variant.
##
## Paper 2A keeps the WELFARE-normalised family as the primary definition (all three
## variants over E[W*], so CoNC^ag = CoNC^W + CoNC^op is an accounting identity).
## The revenue-normalised operator ratio
##   CoNC^op_rev = (E[op surplus | dev] - E[op surplus | truthful]) / E[rev | truthful]
## is reported alongside it for the cor:conc-lb cells, which requires operator
## revenue (= payments collected) to be recorded per round.

library(testthat)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Locate the repo root portably (see test-expL3-bilinear-surface.R).
.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  .SIM_DIR <- if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    normalizePath(".", mustWork = TRUE)
  }
}

source(file.path(.SIM_DIR, "R", "sim_credibility.R"))
source(file.path(.SIM_DIR, "R", "sim_helpers.R"))
source(file.path(.SIM_DIR, "R", "sim_operator.R"))
source(file.path(.SIM_DIR, "R", "sim_market.R"))
source(file.path(.SIM_DIR, "R", "sim_integrator.R"))
source(file.path(.SIM_DIR, "R", "sim_expR5.R"))

## ── t2: revenue-normalised CoNC^op variant ───────────────────────────

test_that("compute_conc_variants adds CoNC_op_rev without disturbing the welfare family", {
  ## Hand-computed frames: welfare 10 -> 9, op surplus 0 -> 0.5, revenue 8 -> 8.4.
  truthful <- tibble::tibble(welfare = c(10, 10, 10),
                             net_op_surplus = c(0, 0, 0),
                             revenue = c(8, 8, 8))
  deviated <- tibble::tibble(welfare = c(9, 9, 9),
                             net_op_surplus = c(0.5, 0.5, 0.5),
                             revenue = c(8.4, 8.4, 8.4))
  out <- compute_conc_variants(truthful, deviated)

  ## Welfare family (primary, unchanged).
  expect_equal(unname(out$CoNC_op), 0.05, tolerance = 1e-9)   # 0.5 / 10
  expect_equal(unname(out$CoNC_W),  0.10, tolerance = 1e-9)   # 1   / 10
  expect_equal(unname(out$CoNC_ag), 0.15, tolerance = 1e-9)   # identity
  ## Revenue-normalised operator ratio: 0.5 / 8 = 0.0625.
  expect_equal(unname(out$CoNC_op_rev), 0.0625, tolerance = 1e-9)
  ## Round-trip: the ratio recovers the deviated operator surplus.
  expect_equal(out$CoNC_op_rev * mean(truthful$revenue),
               mean(deviated$net_op_surplus) - mean(truthful$net_op_surplus),
               tolerance = 1e-12)
})

test_that("compute_conc_variants: truthful vs truthful gives zero on every variant", {
  truthful <- tibble::tibble(welfare = c(10, 12), net_op_surplus = c(0, 0),
                             revenue = c(8, 9))
  out <- compute_conc_variants(truthful, truthful)
  expect_equal(unname(out$CoNC_op), 0, tolerance = 1e-12)
  expect_equal(unname(out$CoNC_W), 0, tolerance = 1e-12)
  expect_equal(unname(out$CoNC_ag), 0, tolerance = 1e-12)
  expect_equal(unname(out$CoNC_op_rev), 0, tolerance = 1e-12)
})

test_that("compute_conc_variants returns NA for CoNC_op_rev on pre-change artifacts", {
  ## Stored artifacts predating the revenue column must still yield the
  ## welfare family, with the revenue-normalised ratio reported as NA.
  truthful <- tibble::tibble(welfare = c(10, 10), net_op_surplus = c(0, 0))
  deviated <- tibble::tibble(welfare = c(9, 9), net_op_surplus = c(0.5, 0.5))
  out <- compute_conc_variants(truthful, deviated)
  expect_equal(unname(out$CoNC_op), 0.05, tolerance = 1e-9)
  expect_true(is.na(out$CoNC_op_rev))
})

test_that("expR5_aggregate reports CoNC_op_rev alongside the welfare family", {
  ## Two conditions on one (mechanism, dag, p_post) cell: truthful baseline
  ## (welfare 10, op surplus 0, revenue 8) and ghost (9, 0.5, 8.4).
  mk <- function(op, w, s, rev) tibble::tibble(
    mechanism_lbl = "vcg", operator_lbl = op, dag_type_lbl = "tree",
    p_post_lbl = 0.5, welfare = c(w, w), net_op_surplus = c(s, s),
    revenue = c(rev, rev)
  )
  results <- dplyr::bind_rows(mk("truthful", 10, 0, 8), mk("ghost_bidder", 9, 0.5, 8.4))
  agg <- expR5_aggregate(results)

  expect_true("CoNC_op_rev" %in% names(agg$summary))
  ghost_row <- agg$summary[agg$summary$operator_lbl == "ghost_bidder", ]
  expect_equal(unname(ghost_row$CoNC_op), 0.05, tolerance = 1e-9)
  expect_equal(unname(ghost_row$CoNC_op_rev), 0.0625, tolerance = 1e-9)
})
