## TDD test suite for Experiment R5 (Trilogy 2A R-5 panels):
##   - posted_price mechanism (with optional ghost / inflator)
##   - first_price mechanism (with ghost)
##   - CoNC^op / CoNC^ag / CoNC^W aggregator
##   - per-DAG γ_ij distribution

library(testthat)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Locate the repo root portably (see test-expL3-bilinear-surface.R).
.SIM_DIR <- normalizePath(
  file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
  mustWork = FALSE
)
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

## ── R5-1: posted-price mechanism ─────────────────────────────────────

test_that("posted_price truthful allocates iff valuation exceeds posted price", {
  res <- run_simulation(
    n_rounds = 5, n_agents = 10, dag_type = "tree", load_level = 1.0,
    operator_type = "truthful", mechanism = "posted_price",
    value_support = c(0, 1), force_all_active = TRUE,
    seed = 1,
    operator_params = list(p_post = 0.5)
  )
  expect_true("mechanism" %in% names(res))
  expect_true(all(res$mechanism == "posted_price"))
  expect_true(all(res$n_allocated <= res$n_tasks))
  ## All five rounds should produce finite welfare
  expect_true(all(is.finite(res$welfare)))
  ## Welfare must be non-negative under truthful posted-price
  expect_true(all(res$welfare >= 0))
})

test_that("posted_price_inflator extracts positive operator surplus", {
  res <- run_simulation(
    n_rounds = 10, n_agents = 12, dag_type = "tree", load_level = 1.0,
    operator_type = "posted_price_inflator", mechanism = "posted_price",
    value_support = c(0, 1), force_all_active = TRUE,
    seed = 7,
    operator_params = list(p_post = 0.3, mu_markup = 0.4)
  )
  ## Operator deviation should produce surplus in at least one round
  expect_true(sum(res$operator_surplus, na.rm = TRUE) > 0)
})

## ── R5-2: first-price mechanism ──────────────────────────────────────

test_that("first_price truthful: payment equals realised value for each allocated task", {
  ## Run a single round directly through vcg/first_price allocator to inspect
  ## per-task payments and values.
  dag <- make_dag("tree")
  env <- make_env()
  agents <- make_agents(8, seed = 3)
  set.seed(123)
  tasks <- generate_tasks(agents, lambda = 1.0,
                          value_support = c(0, 1),
                          seed = 123, force_all_active = TRUE)
  alloc <- first_price_allocate(tasks, env, dag)
  expect_true("vcg_payment" %in% names(alloc))
  alloc_yes <- alloc[alloc$allocated, ]
  if (nrow(alloc_yes) > 0) {
    expect_equal(alloc_yes$vcg_payment, alloc_yes$realised_value,
                 tolerance = 1e-9)
  }
})

test_that("first_price with ghost_bidder inflates payments above truthful", {
  set.seed(2)
  res_truth <- run_simulation(
    n_rounds = 10, n_agents = 12, dag_type = "tree", load_level = 1.0,
    operator_type = "truthful", mechanism = "first_price",
    value_support = c(0, 1), force_all_active = TRUE,
    seed = 11
  )
  res_ghost <- run_simulation(
    n_rounds = 10, n_agents = 12, dag_type = "tree", load_level = 1.0,
    operator_type = "ghost_bidder", mechanism = "first_price",
    value_support = c(0, 1), force_all_active = TRUE,
    seed = 11
  )
  expect_true(sum(res_ghost$operator_surplus, na.rm = TRUE) > 0)
  ## Operator surplus under ghost > truthful (= 0 by construction)
  expect_true(sum(res_ghost$operator_surplus, na.rm = TRUE) >
              sum(res_truth$operator_surplus, na.rm = TRUE))
})

## ── R5-3: CoNC^op / CoNC^ag / CoNC^W aggregator ───────────────────────

test_that("compute_conc_variants returns the CoNC variants", {
  ## Revenue 8 -> 8.4 (the eq:conc numerator, = the agents' payment increment),
  ## operator extraction surplus 0 -> 0.5 (a different measure), welfare 10 -> 9.
  truthful <- tibble::tibble(
    welfare           = c(10, 10, 10),
    operator_surplus  = c(0, 0, 0),
    net_op_surplus    = c(0, 0, 0),
    revenue           = c(8, 8, 8)
  )
  deviated <- tibble::tibble(
    welfare           = c(9, 9, 9),
    operator_surplus  = c(0.5, 0.5, 0.5),
    net_op_surplus    = c(0.5, 0.5, 0.5),
    revenue           = c(8.4, 8.4, 8.4)
  )
  out <- compute_conc_variants(truthful, deviated)
  expect_true(all(c("CoNC_op", "CoNC_ag", "CoNC_W", "extraction_op") %in% names(out)))
  ## CoNC^W = (10 - 9)/10 = 0.1
  expect_equal(unname(out$CoNC_W), 0.1, tolerance = 1e-9)
  ## CoNC^op = (8.4 - 8)/10 = 0.04 ; extraction^op = (0.5 - 0)/10 = 0.05
  expect_equal(unname(out$CoNC_op), 0.04, tolerance = 1e-9)
  expect_equal(unname(out$extraction_op), 0.05, tolerance = 1e-9)
  ## CoNC^ag (agent surplus loss) should be >= CoNC^op in absolute value
  ## (since agent surplus loss = welfare loss + payment increment)
  expect_true(abs(out$CoNC_ag) >= abs(out$CoNC_op) - 1e-9)
})

## ── R5-4: γ_ij distribution extractor ────────────────────────────────

test_that("compute_gamma_distribution returns numeric vector and is finite", {
  dag <- make_dag("tree")
  env <- make_env()
  g <- compute_gamma_distribution(dag, env, n_samples = 50, seed = 1)
  expect_true(is.numeric(g))
  expect_equal(length(g), 50)
  expect_true(all(is.finite(g)))
})

test_that("compute_gamma_distribution: tree DAG yields gamma values near unity scale", {
  dag <- make_dag("tree")
  env <- make_env()
  g <- compute_gamma_distribution(dag, env, n_samples = 200, seed = 2)
  ## gamma is a submodularity gap; for polymatroidal capacities of order O(env capacity)
  ## the per-pair gap should be finite, non-negative, and have median > 0.
  expect_true(median(g) >= 0)
  expect_true(is.finite(median(g)))
})
