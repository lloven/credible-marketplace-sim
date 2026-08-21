## Tests for the per-round operator revenue column.
##
## Operator revenue = payments collected on allocated tasks in the round, taken
## AFTER any credibility reversion. It is the denominator of the
## revenue-normalised CoNC^op variant.

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

## ── t1: per-round revenue column ─────────────────────────────────────

test_that("run_market_round reports revenue = sum of payments collected on allocated tasks", {
  dag    <- make_dag("tree")
  env    <- make_env()
  agents <- make_agents(6, seed = 3)
  tasks  <- generate_tasks(agents, lambda = 1.0, deadlines = c(100, 150, 200),
                           value_support = c(1, 2), seed = 3001,
                           force_all_active = TRUE)

  rr <- run_market_round(
    tasks, env, dag,
    operator    = make_operator(type = "truthful"),
    credibility = make_credibility_mechanism(type = "none"),
    round = 1
  )

  ## Independent recomputation from a fresh VCG allocation on the same tasks.
  ref <- vcg_allocate(tasks, env, dag)
  expected <- sum(ref$vcg_payment[ref$allocated], na.rm = TRUE)

  expect_true("revenue" %in% names(rr))
  expect_true(expected > 0)
  expect_equal(rr$revenue, expected, tolerance = 1e-9)
})

test_that("run_simulation records one finite revenue value per round", {
  res <- run_simulation(
    n_rounds = 5, n_agents = 8, dag_type = "tree", load_level = 1.0,
    operator_type = "truthful", credibility_type = "none",
    value_support = c(1, 2), force_all_active = TRUE, seed = 7
  )
  expect_true("revenue" %in% names(res))
  expect_equal(nrow(res), 5)
  expect_true(all(is.finite(res$revenue)))
})

test_that("ghost deviation raises collected revenue above the truthful baseline", {
  common <- list(n_rounds = 5, n_agents = 8, dag_type = "tree", load_level = 1.0,
                 credibility_type = "none", value_support = c(1, 2),
                 force_all_active = TRUE, seed = 7)
  truth <- do.call(run_simulation, c(common, list(operator_type = "truthful")))
  ghost <- do.call(run_simulation, c(common, list(operator_type = "ghost_bidder")))
  expect_true(mean(ghost$revenue) > mean(truth$revenue))
})
