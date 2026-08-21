## Tests for the ghost-bid amplitude mode (see README, "Ghost-bid amplitude").
##
## Two semantics coexist:
##   "fixed"           ghost_val = 1.1 * v_max  (default; 2B's SDS forward
##                     calibration depends on it, eq:tau-zero-fixed-eps)
##   "state_dependent" ghost_val = 1.1 * realised value of the displaced
##                     marginal task (pre-286d443 semantics; 2A experiments)

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
source(file.path(.SIM_DIR, "R", "sim_expA.R"))
source(file.path(.SIM_DIR, "R", "sim_expE.R"))
source(file.path(.SIM_DIR, "R", "sim_expK.R"))
source(file.path(.SIM_DIR, "R", "sim_expR5.R"))

## Toy allocation: three allocated tasks, the last one (realised value 1.5)
## is the marginal task the ghost bid displaces.
toy_alloc <- function() {
  tibble::tibble(
    task_id        = c("t1", "t2", "t3"),
    agent_id       = 1:3,
    allocated      = c(TRUE, TRUE, TRUE),
    tier           = "edge",
    realised_value = c(3.0, 2.0, 1.5),
    vcg_payment    = c(1.0, 1.0, 1.0)
  )
}

ghost_amplitude <- function(operator) {
  alloc <- toy_alloc()
  res <- apply_operator_strategy(operator, alloc, alloc$vcg_payment,
                                 make_env(), tibble::tibble(agent_id = 1:3))
  res$deviation_amplitude
}

## ── t3: fixed mode (default) ─────────────────────────────────────────

test_that("amplitude_mode = 'fixed' gives the constant 1.1 * v_max", {
  op <- make_operator(type = "ghost_bidder", v_max = 2, amplitude_mode = "fixed")
  expect_equal(ghost_amplitude(op), 2.2, tolerance = 1e-9)
})

test_that("amplitude_mode = 'fixed' has zero across-round variance in a run", {
  res <- run_simulation(
    n_rounds = 10, n_agents = 12, dag_type = "tree", load_level = 1.0,
    operator_type = "ghost_bidder", credibility_type = "none",
    operator_params = list(amplitude_mode = "fixed"),
    value_support = c(1, 2), force_all_active = TRUE, seed = 11
  )
  expect_true(all(abs(res$deviation_amplitude - 2.2) < 1e-9))
  expect_equal(sd(res$deviation_amplitude), 0, tolerance = 1e-12)
})

## ── t4: state-dependent mode ─────────────────────────────────────────

test_that("amplitude_mode = 'state_dependent' gives 1.1 * realised marginal value", {
  op <- make_operator(type = "ghost_bidder", v_max = 2,
                      amplitude_mode = "state_dependent")
  expect_equal(ghost_amplitude(op), 1.1 * 1.5, tolerance = 1e-9)
})

test_that("amplitude_mode = 'state_dependent' varies across rounds in a seeded run", {
  res <- run_simulation(
    n_rounds = 10, n_agents = 12, dag_type = "tree", load_level = 1.0,
    operator_type = "ghost_bidder", credibility_type = "none",
    operator_params = list(amplitude_mode = "state_dependent"),
    value_support = c(1, 2), force_all_active = TRUE, seed = 11
  )
  expect_true(sd(res$deviation_amplitude) > 0)
  ## Amplitude = 1.1 * the displaced task's REALISED value, which is the bid
  ## value net of congestion/latency discounting, so it is positive and capped
  ## by 1.1 * v_max = 2.2 but may fall below 1.1 * v_min.
  expect_true(all(res$deviation_amplitude > 0))
  expect_true(all(res$deviation_amplitude <= 2.2 + 1e-9))
})

## ── t5: default and explicit ghost_value unchanged ───────────────────

test_that("an operator with no amplitude_mode behaves as 'fixed'", {
  expect_equal(ghost_amplitude(make_operator(type = "ghost_bidder", v_max = 2)),
               2.2, tolerance = 1e-9)
  ## Pre-existing operator objects (built before the mode flag existed) must
  ## keep working: no amplitude_mode field at all.
  legacy <- make_operator(type = "ghost_bidder", v_max = 2)
  legacy$amplitude_mode <- NULL
  expect_equal(ghost_amplitude(legacy), 2.2, tolerance = 1e-9)
})

test_that("an explicit ghost_value overrides both modes", {
  for (mode in c("fixed", "state_dependent")) {
    op <- make_operator(type = "ghost_bidder", v_max = 2, ghost_value = 0.7,
                        amplitude_mode = mode)
    expect_equal(ghost_amplitude(op), 0.7, tolerance = 1e-9)
  }
})

## ── 2A experiments run in state-dependent mode ───────────────────────

test_that("expA/expE/expK/expR5 ghost conditions run state-dependent", {
  amp_sd <- function(res) sd(res$deviation_amplitude, na.rm = TRUE)

  a <- expA_run_single(
    tibble::tibble(dag_type = "tree", load_level = 1.0, load_name = "medium",
                   operator_type = "ghost_bidder", epsilon = NA_real_,
                   mu_markup = NA_real_),
    n_rounds = 10, n_agents = 12, seed = 11
  )
  expect_true(amp_sd(a) > 0)

  e <- expE_run_single(
    tibble::tibble(n_agents = 12, dag_type = "tree", credibility = "none"),
    n_rounds = 10, seed = 11
  )
  expect_true(amp_sd(e) > 0)

  k <- expK_run_single(
    tibble::tibble(mechanism = "vcg", operator = "ghost_bidder",
                   credibility = "none", dag_type = "tree", vdist_idx = 1,
                   vdist_label = "U(1,2)"),
    n_rounds = 10, seed = 11, value_dists = list(c(1, 2))
  )
  expect_true(amp_sd(k) > 0)

  r5 <- expR5_run_single(
    tibble::tibble(mechanism = "vcg", operator = "ghost_bidder",
                   dag_type = "tree", p_post = 0.5, condition_id = 1),
    n_rounds = 10, seed = 11
  )
  expect_true(amp_sd(r5) > 0)
})
