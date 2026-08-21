suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment K: Revenue-Optimal (Myerson) Mechanism ────────────────
##
## Validates that the credibility trilemma (Theorem 1) extends to the
## revenue-optimal mechanism.  For Uniform(a, b), the Myerson mechanism
## applies a reserve price r = b/2 and sorts by virtual value
## φ(v) = 2v − b.  For Uniform(1, 2), φ is order-preserving and
## reserve = 1.0 (never binds), so Myerson = VCG.  For Uniform(0.5, 2),
## reserve = 1.0 excludes ≈33% of tasks, making Myerson ≠ VCG.
##
## Design:  mechanism × operator × credibility × topology × value_dist
## Key tests:
##   1. Ghost-bid profitable under Myerson + none  (trilemma applies)
##   2. Ghost-bid unprofitable under Myerson + broadcast  (commitment resolves)
##   3. Results match VCG qualitatively across both value distributions

expK_design <- function(
  mechanisms    = c("vcg", "myerson"),
  operators     = c("truthful", "ghost_bidder"),
  cred_types    = c("none", "broadcast"),
  topologies    = c("tree", "sp", "entangled"),
  value_dists   = list(c(1, 2), c(0.5, 2))
) {
  # Create value distribution labels
  vdist_labels <- sapply(value_dists, function(v) paste0("U(", v[1], ",", v[2], ")"))

  conditions <- expand.grid(
    mechanism   = mechanisms,
    operator    = operators,
    credibility = cred_types,
    dag_type    = topologies,
    vdist_idx   = seq_along(value_dists),
    stringsAsFactors = FALSE
  ) %>%
    as_tibble() %>%
    mutate(
      vdist_label  = vdist_labels[vdist_idx],
      condition_id = row_number()
    )

  attr(conditions, "value_dists") <- value_dists
  conditions
}


expK_run_single <- function(condition, n_rounds, seed, value_dists) {
  vdist <- value_dists[[condition$vdist_idx]]
  run_simulation(
    n_rounds         = n_rounds,
    n_agents         = 40,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = condition$operator,
    ## State-dependent ghost amplitude, as used throughout Paper 2A.
    operator_params  = list(amplitude_mode = "state_dependent"),
    credibility_type = condition$credibility,
    mechanism        = condition$mechanism,
    value_support    = vdist,
    seed             = seed
  ) %>%
    mutate(
      mechanism_cond = condition$mechanism,
      vdist_label    = condition$vdist_label
    )
}


expK_run_all <- function(conditions, n_rounds = 100, n_seeds = 5) {
  value_dists <- attr(conditions, "value_dists")
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expK_run_single(cond, n_rounds, seed = s, value_dists = value_dists) %>%
        mutate(condition_id = cond$condition_id)
    })
  })
  results
}


expK_aggregate <- function(results) {
  # Baseline: truthful operator, VCG mechanism, no credibility
  baseline <- results %>%
    filter(operator_type == "truthful", mechanism_cond == "vcg",
           credibility == "none") %>%
    group_by(dag_type, vdist_label, seed) %>%
    summarise(
      baseline_welfare = mean(welfare, na.rm = TRUE),
      .groups = "drop"
    )

  summary <- results %>%
    group_by(dag_type, mechanism_cond, operator_type, credibility,
             vdist_label, seed) %>%
    summarise(
      mean_welfare     = mean(welfare, na.rm = TRUE),
      mean_op_surplus  = mean(operator_surplus, na.rm = TRUE),
      mean_net_surplus = mean(net_op_surplus, na.rm = TRUE),
      detection_rate   = mean(penalty_applied > 0, na.rm = TRUE),
      mean_payment     = mean(
        ifelse(n_allocated > 0,
               (welfare + operator_surplus) / n_allocated,
               NA_real_),
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    left_join(baseline, by = c("dag_type", "vdist_label", "seed")) %>%
    mutate(
      welfare_ratio    = mean_welfare / baseline_welfare,
      ghost_profitable = (mean_net_surplus > 0)
    ) %>%
    group_by(dag_type, mechanism_cond, operator_type, credibility, vdist_label) %>%
    summarise(
      # Ghost-bid profitability
      surplus_mean     = mean(mean_net_surplus, na.rm = TRUE),
      surplus_ci       = list(boot_ci(mean_net_surplus)),
      gross_surplus    = mean(mean_op_surplus, na.rm = TRUE),
      pct_profitable   = mean(ghost_profitable, na.rm = TRUE),

      # Welfare
      welfare_mean     = mean(welfare_ratio, na.rm = TRUE),
      welfare_ci       = list(boot_ci(welfare_ratio)),

      # Detection
      detect_mean      = mean(detection_rate, na.rm = TRUE),

      # Revenue (mean payment per allocated task)
      payment_mean     = mean(mean_payment, na.rm = TRUE),
      payment_ci       = list(boot_ci(mean_payment)),

      n_obs            = n(),
      .groups = "drop"
    ) %>%
    mutate(
      surplus_ci_lo  = map_dbl(surplus_ci, ~ .x["lo"]),
      surplus_ci_hi  = map_dbl(surplus_ci, ~ .x["hi"]),
      welfare_ci_lo  = map_dbl(welfare_ci, ~ .x["lo"]),
      welfare_ci_hi  = map_dbl(welfare_ci, ~ .x["hi"]),
      payment_ci_lo  = map_dbl(payment_ci, ~ .x["lo"]),
      payment_ci_hi  = map_dbl(payment_ci, ~ .x["hi"])
    ) %>%
    select(-surplus_ci, -welfare_ci, -payment_ci)

  summary
}
