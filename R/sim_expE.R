suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment E: Credibility Trilemma Demonstration ─────────────
##
## Directly validates Theorem 1 (credibility trilemma):
##   No static sealed-bid mechanism is simultaneously
##   (i) revenue-optimal, (ii) DSIC, (iii) credible.
##
## Tests:
##   1. Ghost-bid profitability: operator can profitably deviate
##   2. Per-agent undetectability: each agent's outcome is consistent
##      with *some* truthful execution
##   3. Commitment restores credibility: with broadcast or blockchain,
##      ghost-bid is detectable
##
## This is the paper's central theoretical result.

expE_design <- function(
  topologies  = c("tree", "sp", "entangled"),
  n_agents_set = c(10, 25, 50),
  cred_types  = c("none", "broadcast", "blockchain", "exchange")
) {
  conditions <- expand.grid(
    dag_type    = topologies,
    n_agents    = n_agents_set,
    credibility = cred_types,
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(condition_id = row_number())

  conditions
}


expE_run_single <- function(condition, n_rounds, seed) {
  run_simulation(
    n_rounds         = n_rounds,
    n_agents         = condition$n_agents,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = "ghost_bidder",
    ## State-dependent ghost amplitude for the 2A experiments (ruling D2).
    operator_params  = list(amplitude_mode = "state_dependent"),
    credibility_type = condition$credibility,
    seed             = seed
  ) %>%
    mutate(n_agents_cond = condition$n_agents)
}


expE_run_all <- function(conditions, n_rounds, n_seeds) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expE_run_single(cond, n_rounds, seed = s) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expE_aggregate <- function(results) {
  # Key metrics:
  # 1. Ghost-bid profitability: mean operator surplus when no enforcement
  # 2. Detection rate: fraction of rounds where ghost detected
  # 3. Welfare loss: comparing ghost-bid welfare to truthful baseline

  # Truthful baseline (no-enforcement, no ghost)
  no_ghost <- results %>%
    filter(credibility == "exchange") %>%
    group_by(dag_type, n_agents_cond, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, n_agents_cond, credibility, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_op_surplus  = mean(operator_surplus, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      detection_rate   = mean(penalty_applied > 0, na.rm = TRUE),
      mean_penalty     = mean(penalty_applied, na.rm = TRUE),
      mean_drop_rate   = mean(drop_rate, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(no_ghost, by = c("dag_type", "n_agents_cond", "seed")) %>%
    mutate(
      welfare_ratio    = mean_welfare / baseline_welfare,
      # Use NET surplus (after penalties) for profitability assessment
      ghost_profitable = (mean_net_surplus > 0)
    ) %>%
    group_by(dag_type, n_agents_cond, credibility) %>%
    summarise(
      # Profitability of ghost bid: NET surplus (after penalties/reversion)
      ghost_surplus_mean    = mean(mean_net_surplus, na.rm = TRUE),
      ghost_surplus_ci      = list(boot_ci(mean_net_surplus)),
      # Also track gross surplus for comparison
      ghost_gross_mean      = mean(mean_op_surplus, na.rm = TRUE),
      pct_profitable        = mean(ghost_profitable, na.rm = TRUE),

      # Welfare impact
      welfare_ratio_mean    = mean(welfare_ratio, na.rm = TRUE),
      welfare_ratio_ci      = list(boot_ci(welfare_ratio)),

      # Detection (trilemma part 3: commitment restores credibility)
      detection_rate_mean   = mean(detection_rate, na.rm = TRUE),
      net_surplus_mean      = mean(mean_net_surplus, na.rm = TRUE),

      drop_rate_mean        = mean(mean_drop_rate, na.rm = TRUE),
      n_obs                 = n(),
      .groups = "drop"
    ) %>%
    mutate(
      ghost_surplus_ci_lo    = map_dbl(ghost_surplus_ci, ~ .x["lo"]),
      ghost_surplus_ci_hi    = map_dbl(ghost_surplus_ci, ~ .x["hi"]),
      welfare_ratio_ci_lo    = map_dbl(welfare_ratio_ci, ~ .x["lo"]),
      welfare_ratio_ci_hi    = map_dbl(welfare_ratio_ci, ~ .x["hi"])
    ) %>%
    select(-ghost_surplus_ci, -welfare_ratio_ci)

  summary
}
