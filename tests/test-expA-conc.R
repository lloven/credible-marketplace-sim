## Tests for the Exp. 1 per-cell CoNC table (audit M-D-01 / M-D-02).
##
## The manuscript reports, for the nine (topology x load) ghost-bidder cells of
## Exp. 1, an operator extraction of 1.74% and CoNC^op = 1.72% against
## CoNC^W = 1.58%.  Before these targets existed no released script produced
## those numbers.  The tests pin (a) the per-cell scoring convention (own
## matched truthful baseline, mean of per-cell ratios) and (b) the headline
## values against the stored raw results.

library(testthat)
library(dplyr)
library(tibble)

`%||%` <- function(a, b) if (is.null(a)) b else a

## Locate the repo root portably (see test-conc-normalisation.R).
.SIM_DIR <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), ".."),
                          mustWork = FALSE)
if (!dir.exists(file.path(.SIM_DIR, "R"))) {
  .SIM_DIR <- if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    normalizePath(".", mustWork = TRUE)
  }
}

source(file.path(.SIM_DIR, "R", "sim_helpers.R"))
source(file.path(.SIM_DIR, "R", "sim_expA.R"))

test_that("per-cell CoNC scoring uses the matched truthful baseline", {
  ## Two cells with deliberately different revenue scales.  If the function
  ## pooled rounds before dividing, the small-revenue cell's ratio would be
  ## swamped by the large one.
  mk <- function(dag, lvl, op, W, rev, surp) {
    tibble(dag_type = dag, load_level = lvl, load_name = as.character(lvl),
           operator_type = op, welfare = W, revenue = rev,
           operator_surplus = surp)
  }
  results <- bind_rows(
    mk("tree", 1.0, "truthful",     100, 10, 0),
    mk("tree", 1.0, "ghost_bidder",  99, 11, 1),
    mk("sp",   1.0, "truthful",     100,  1, 0),
    mk("sp",   1.0, "ghost_bidder",  99,  2, 1)
  )
  bc <- expA_conc_by_cell(results)
  expect_equal(nrow(bc), 2L)
  ## CoNC^op = (rev_dev - rev_truth) / W_truth = 1/100 in BOTH cells
  expect_equal(bc$CoNC_op, c(1 / 100, 1 / 100))
  ## CoNC^op_rev = (rev_dev - rev_truth) / rev_truth: 1/10 vs 1/1
  expect_equal(sort(bc$CoNC_op_rev), c(0.1, 1.0))
  ## CoNC^ag is the identity CoNC^W + CoNC^op
  expect_equal(bc$CoNC_ag, bc$CoNC_W + bc$CoNC_op)
  ## headline is the MEAN of the per-cell ratios, not a ratio of pooled means
  hl <- expA_headline_conc(bc)
  expect_equal(hl$CoNC_op_rev, mean(c(0.1, 1.0)))
})

test_that("Exp. 1 ghost-bidder headline reproduces the reported percentages", {
  store <- file.path(.SIM_DIR, "_targets", "objects", "expA_results_raw")
  skip_if_not(file.exists(store), "expA_results_raw not in the targets store")
  hl <- expA_headline_conc(expA_conc_by_cell(readRDS(store)))
  ghost <- hl %>% filter(operator_type == "ghost_bidder")
  expect_equal(ghost$n_cells, 9L)
  expect_equal(round(100 * ghost$extraction_op, 2), 1.74)
  expect_equal(round(100 * ghost$CoNC_op,       2), 1.72)
  expect_equal(round(100 * ghost$CoNC_W,        2), 1.58)
  expect_equal(round(100 * ghost$CoNC_ag,       2), 3.30)
  ## Extraction exceeds the revenue delta: the ghost displaces a paying agent.
  expect_gt(ghost$extraction_op, ghost$CoNC_op)
})
