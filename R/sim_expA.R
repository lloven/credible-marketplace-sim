suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment A: Welfare loss from strategic operators ───────────
##
## Varies: operator strategy × DAG topology × load level
## Credibility: none (to measure raw welfare loss)
## Now includes ghost_bidder (trilemma demonstration)
## Key metrics: welfare (% of truthful), price distortion, drop rate,
##              agent surplus, operator surplus

expA_design <- function(
  topologies  = c("tree", "sp", "entangled"),
  load_levels = c(low = 0.5, medium = 1.0, high = 1.5),
  operator_types = c("truthful", "misreporter", "inflator",
                     "discriminator", "ghost_bidder"),
  epsilon_values = c(0.1, 0.2, 0.3),
  mu_markup_values = c(0.1, 0.2, 0.3)
) {
  conditions <- list()

  for (topo in topologies) {
    for (load_name in names(load_levels)) {
      load_val <- load_levels[load_name]

      # Truthful baseline
      conditions <- c(conditions, list(tibble(
        dag_type      = topo,
        load_level    = load_val,
        load_name     = load_name,
        operator_type = "truthful",
        epsilon       = NA_real_,
        mu_markup     = NA_real_
      )))

      # Misreporter (vary epsilon)
      for (eps in epsilon_values) {
        conditions <- c(conditions, list(tibble(
          dag_type      = topo,
          load_level    = load_val,
          load_name     = load_name,
          operator_type = "misreporter",
          epsilon       = eps,
          mu_markup     = NA_real_
        )))
      }

      # Inflator (vary mu_markup)
      for (mu in mu_markup_values) {
        conditions <- c(conditions, list(tibble(
          dag_type      = topo,
          load_level    = load_val,
          load_name     = load_name,
          operator_type = "inflator",
          epsilon       = NA_real_,
          mu_markup     = mu
        )))
      }

      # Discriminator
      conditions <- c(conditions, list(tibble(
        dag_type      = topo,
        load_level    = load_val,
        load_name     = load_name,
        operator_type = "discriminator",
        epsilon       = NA_real_,
        mu_markup     = NA_real_
      )))

      # Ghost bidder (trilemma demonstration)
      conditions <- c(conditions, list(tibble(
        dag_type      = topo,
        load_level    = load_val,
        load_name     = load_name,
        operator_type = "ghost_bidder",
        epsilon       = NA_real_,
        mu_markup     = NA_real_
      )))
    }
  }

  bind_rows(conditions) %>%
    mutate(condition_id = row_number())
}


expA_run_single <- function(condition, n_rounds, n_agents, seed) {
  ## Paper 2A's experiments use the state-dependent ghost amplitude; the
  ## fixed rule stays the default elsewhere (see README, "Ghost-bid amplitude").
  op_params <- list(amplitude_mode = "state_dependent")
  if (!is.na(condition$epsilon))  op_params$epsilon  <- condition$epsilon
  if (!is.na(condition$mu_markup)) op_params$mu_markup <- condition$mu_markup

  run_simulation(
    n_rounds       = n_rounds,
    n_agents       = n_agents,
    dag_type       = condition$dag_type,
    load_level     = condition$load_level,
    operator_type  = condition$operator_type,
    operator_params = op_params,
    credibility_type = "none",
    seed           = seed
  )
}


expA_run_all <- function(conditions, n_rounds, n_agents, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expA_run_single(cond, n_rounds, n_agents, seed = s) %>%
        mutate(condition_id = cond$condition_id,
               load_name = cond$load_name)
    })
  })
  results
}


expA_aggregate <- function(results) {
  baselines <- results %>%
    filter(operator_type == "truthful") %>%
    group_by(dag_type, load_level, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, load_level, load_name, operator_type, seed) %>%
    summarise(
      mean_welfare = mean(welfare, na.rm = TRUE),
      mean_drop_rate = mean(drop_rate, na.rm = TRUE),
      mean_op_surplus = mean(operator_surplus, na.rm = TRUE),
      price_volatility = mean(price_sd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(baselines, by = c("dag_type", "load_level", "seed")) %>%
    mutate(
      welfare_ratio = mean_welfare / baseline_welfare,
      welfare_loss  = 1 - welfare_ratio
    ) %>%
    group_by(dag_type, load_name, operator_type) %>%
    summarise(
      welfare_loss_mean = mean(welfare_loss, na.rm = TRUE),
      welfare_loss_sd   = sd(welfare_loss, na.rm = TRUE),
      welfare_loss_ci   = list(boot_ci(welfare_loss)),
      drop_rate_mean    = mean(mean_drop_rate, na.rm = TRUE),
      op_surplus_mean   = mean(mean_op_surplus, na.rm = TRUE),
      price_vol_mean    = mean(price_volatility, na.rm = TRUE),
      n_obs             = n(),
      .groups = "drop"
    ) %>%
    mutate(
      welfare_loss_ci_lo = map_dbl(welfare_loss_ci, ~ .x["lo"]),
      welfare_loss_ci_hi = map_dbl(welfare_loss_ci, ~ .x["hi"])
    ) %>%
    select(-welfare_loss_ci)

  summary
}


## ── Per-cell CoNC variants for Exp. 1 (audit M-D-01 / M-D-02) ────────
##
## `expA_aggregate()` reports welfare loss and operator surplus; it never
## produced the eq:conc family, so the manuscript's per-cell revenue-normalised
## ratio and its 1.74% / 1.72% / 1.58% headline had no producing code.  These
## two functions close that.
##
## Each (topology x load) cell is scored against ITS OWN matched truthful
## baseline before any averaging: truthful revenue spans 0.36 to 24.4 across the
## nine ghost cells, so pooling rounds before dividing would average over
## incomparable denominators (sec:eval-conc).  The headline is therefore the
## mean of the per-cell ratios, not a ratio of pooled means.
##
## Definitions are `compute_conc_variants()` in R/sim_helpers.R: CoNC^op is the
## collected-REVENUE delta over E[W*], and the operator's extraction surplus is
## a different quantity reported alongside it.

expA_conc_by_cell <- function(results) {
  cells <- results %>%
    distinct(dag_type, load_level, load_name)
  ops <- setdiff(unique(results$operator_type), "truthful")

  rows <- list()
  for (i in seq_len(nrow(cells))) {
    dag <- cells$dag_type[i]
    lvl <- cells$load_level[i]
    truthful <- results %>%
      filter(dag_type == dag, load_level == lvl, operator_type == "truthful")
    for (op in ops) {
      deviated <- results %>%
        filter(dag_type == dag, load_level == lvl, operator_type == op)
      if (nrow(deviated) == 0L || nrow(truthful) == 0L) next
      v <- compute_conc_variants(truthful, deviated)
      rows[[length(rows) + 1L]] <- tibble(
        dag_type      = dag,
        load_level    = lvl,
        load_name     = cells$load_name[i],
        operator_type = op,
        extraction_op = v$extraction_op,
        CoNC_op       = v$CoNC_op,
        CoNC_op_rev   = v$CoNC_op_rev,
        CoNC_W        = v$CoNC_W,
        CoNC_ag       = v$CoNC_ag
      )
    }
  }
  bind_rows(rows) %>% arrange(operator_type, dag_type, load_level)
}

expA_headline_conc <- function(conc_by_cell) {
  conc_by_cell %>%
    group_by(operator_type) %>%
    summarise(
      n_cells       = n(),
      extraction_op = mean(extraction_op, na.rm = TRUE),
      CoNC_op       = mean(CoNC_op,       na.rm = TRUE),
      CoNC_op_rev   = mean(CoNC_op_rev,   na.rm = TRUE),
      CoNC_W        = mean(CoNC_W,        na.rm = TRUE),
      CoNC_ag       = mean(CoNC_ag,       na.rm = TRUE),
      .groups = "drop"
    )
}
