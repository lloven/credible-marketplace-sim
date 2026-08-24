## Tests for the CoNC numerators and their normalisations.
##
## eq:conc pins the CoNC^op numerator as the collected-REVENUE delta
## E[rev^delta - rev*] (the accounting-identity sentence reads it as the agents'
## aggregate payment increment E[sum_i p_i^delta - sum_i p_i^*]).  Paper 2A keeps the
## WELFARE-normalised family as the primary definition, so
##   CoNC^op = E[rev^delta - rev*] / E[W*]
##   CoNC^W  = (E[W*] - E[W^delta]) / E[W*]
##   CoNC^ag = CoNC^W + CoNC^op            (agent surplus loss, an identity)
## and the revenue-normalised operator ratio is the SAME numerator over truthful
## revenue,
##   CoNC^op_rev = E[rev^delta - rev*] / E[rev*].
##
## The operator's own extraction surplus (ghost payment increments on the
## still-allocated agents, which ignore the displaced agent's forgone payment) is a
## DIFFERENT measure and is reported separately as
##   extraction^op = E[op surplus^delta - op surplus*] / E[W*].
## All of these need operator revenue (= payments collected) recorded per round.

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

## Hand-computed frames used throughout: welfare 10 -> 9, operator extraction
## surplus 0 -> 0.5, collected revenue 8 -> 8.4.  So
##   revenue delta   = 0.4     extraction delta = 0.5
##   CoNC_op         = 0.4/10  = 0.04     extraction_op = 0.5/10 = 0.05
##   CoNC_op_rev     = 0.4/8   = 0.05     CoNC_W        = 1/10   = 0.10
##   CoNC_ag         = 0.10 + 0.04 = 0.14
.truthful <- tibble::tibble(welfare = c(10, 10, 10),
                            net_op_surplus = c(0, 0, 0),
                            revenue = c(8, 8, 8))
.deviated <- tibble::tibble(welfare = c(9, 9, 9),
                            net_op_surplus = c(0.5, 0.5, 0.5),
                            revenue = c(8.4, 8.4, 8.4))

## ── t2: numerators of the CoNC family ────────────────────────────────

test_that("compute_conc_variants uses the revenue delta for CoNC_op, not extraction", {
  out <- compute_conc_variants(.truthful, .deviated)

  ## eq:conc quantity: revenue delta over truthful welfare.
  expect_equal(unname(out$CoNC_op), 0.04, tolerance = 1e-9)
  ## Operator extraction surplus, over the same denominator: a DIFFERENT measure.
  expect_equal(unname(out$extraction_op), 0.05, tolerance = 1e-9)
  expect_false(isTRUE(all.equal(out$CoNC_op, out$extraction_op)))
  expect_equal(unname(out$CoNC_W), 0.10, tolerance = 1e-9)
  expect_equal(unname(out$CoNC_ag), 0.14, tolerance = 1e-9)
  ## Revenue-normalised operator ratio: same numerator, truthful revenue below.
  expect_equal(unname(out$CoNC_op_rev), 0.05, tolerance = 1e-9)
  ## Round-trip: the ratio recovers the collected-revenue delta.
  expect_equal(out$CoNC_op_rev * mean(.truthful$revenue),
               mean(.deviated$revenue) - mean(.truthful$revenue),
               tolerance = 1e-12)
})

test_that("CoNC_ag is the payment-increment identity, numerically", {
  ## Agent surplus = welfare - payments collected.  Its loss, normalised by
  ## E[W*], must equal CoNC_W + CoNC_op exactly.
  out <- compute_conc_variants(.truthful, .deviated)
  agent_surplus_truth <- mean(.truthful$welfare) - mean(.truthful$revenue)
  agent_surplus_dev   <- mean(.deviated$welfare) - mean(.deviated$revenue)
  expect_equal(unname(out$CoNC_ag),
               (agent_surplus_truth - agent_surplus_dev) / mean(.truthful$welfare),
               tolerance = 1e-12)
  ## The identity fails if the extraction numerator is substituted in.
  expect_false(isTRUE(all.equal(
    out$CoNC_W + out$extraction_op,
    (agent_surplus_truth - agent_surplus_dev) / mean(.truthful$welfare))))
})

test_that("compute_conc_variants: truthful vs truthful gives zero on every variant", {
  truthful <- tibble::tibble(welfare = c(10, 12), net_op_surplus = c(0, 0),
                             revenue = c(8, 9))
  out <- compute_conc_variants(truthful, truthful)
  expect_equal(unname(out$CoNC_op), 0, tolerance = 1e-12)
  expect_equal(unname(out$extraction_op), 0, tolerance = 1e-12)
  expect_equal(unname(out$CoNC_W), 0, tolerance = 1e-12)
  expect_equal(unname(out$CoNC_ag), 0, tolerance = 1e-12)
  expect_equal(unname(out$CoNC_op_rev), 0, tolerance = 1e-12)
})

test_that("compute_conc_variants returns NA for the revenue quantities on pre-change artifacts", {
  ## Stored artifacts predating the revenue column can still yield CoNC_W and the
  ## extraction rate; every quantity with a revenue delta in it is NA, CoNC_ag
  ## included (it is CoNC_W + CoNC_op).
  truthful <- tibble::tibble(welfare = c(10, 10), net_op_surplus = c(0, 0))
  deviated <- tibble::tibble(welfare = c(9, 9), net_op_surplus = c(0.5, 0.5))
  out <- compute_conc_variants(truthful, deviated)
  expect_equal(unname(out$extraction_op), 0.05, tolerance = 1e-9)
  expect_equal(unname(out$CoNC_W), 0.10, tolerance = 1e-9)
  expect_true(is.na(out$CoNC_op))
  expect_true(is.na(out$CoNC_op_rev))
  expect_true(is.na(out$CoNC_ag))
})

test_that("expR5_aggregate pins the same numerators as compute_conc_variants", {
  ## Two conditions on one (mechanism, dag, p_post) cell: truthful baseline
  ## (welfare 10, op surplus 0, revenue 8) and ghost (9, 0.5, 8.4).
  mk <- function(op, w, s, rev) tibble::tibble(
    mechanism_lbl = "vcg", operator_lbl = op, dag_type_lbl = "tree",
    p_post_lbl = 0.5, welfare = c(w, w), net_op_surplus = c(s, s),
    revenue = c(rev, rev)
  )
  results <- dplyr::bind_rows(mk("truthful", 10, 0, 8), mk("ghost_bidder", 9, 0.5, 8.4))
  agg <- expR5_aggregate(results)

  expect_true(all(c("CoNC_op", "CoNC_op_rev", "extraction_op") %in% names(agg$summary)))
  ghost_row <- agg$summary[agg$summary$operator_lbl == "ghost_bidder", ]
  expect_equal(unname(ghost_row$CoNC_op), 0.04, tolerance = 1e-9)
  expect_equal(unname(ghost_row$extraction_op), 0.05, tolerance = 1e-9)
  expect_equal(unname(ghost_row$CoNC_op_rev), 0.05, tolerance = 1e-9)
  expect_equal(unname(ghost_row$CoNC_W), 0.10, tolerance = 1e-9)
  expect_equal(unname(ghost_row$CoNC_ag), 0.14, tolerance = 1e-9)
})
