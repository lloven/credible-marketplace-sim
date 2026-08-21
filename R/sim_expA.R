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
