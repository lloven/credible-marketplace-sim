suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Experiment R5: mechanism-class robustness panel ──────────────────
##
## Implements the TEAC R-5 reviewer request:
##   (a) posted-price / first-price baseline against VCG
##   (b) agent-side CoNC^ag reporting
##   (c) γ_ij realised distribution per topology
##
## Factorial design:
##   mechanism × operator × dag × p_post × seed
##
## (mechanism, operator) filter:
##   - vcg / myerson / first_price : truthful and ghost_bidder allowed
##   - posted_price                : truthful, ghost_bidder, posted_price_inflator
##   - posted_price_inflator is meaningless without posted_price mechanism

EXPR5_N_AGENTS <- 40
EXPR5_V_BAR    <- 1.0

expR5_design <- function(
  mechanisms    = c("vcg", "posted_price", "first_price"),
  operators     = c("truthful", "ghost_bidder", "posted_price_inflator"),
  topologies    = c("tree", "sp", "entangled"),
  p_post_values = c(0.2, 0.5, 0.8)
) {
  raw <- expand.grid(
    mechanism = mechanisms,
    operator  = operators,
    dag_type  = topologies,
    p_post    = p_post_values,
    stringsAsFactors = FALSE
  )
  ## Filter incompatibilities
  ##  - posted_price_inflator requires mechanism = posted_price
  ##  - non-posted-price mechanisms: p_post is irrelevant; collapse to one
  ##    nominal value to avoid duplicate conditions.
  ok <- !(raw$operator == "posted_price_inflator" &
          raw$mechanism != "posted_price")
  raw <- raw[ok, , drop = FALSE]

  ## For mechanisms != posted_price, p_post is unused: keep only one row
  ## per (mechanism, operator, dag) for those cases.
  is_pp <- raw$mechanism == "posted_price"
  non_pp <- raw[!is_pp, , drop = FALSE]
  non_pp$p_post <- 0.5  # nominal
  non_pp <- unique(non_pp)
  pp     <- raw[is_pp, , drop = FALSE]
  out    <- rbind(non_pp, pp)
  out    <- as_tibble(out)
  out$condition_id <- seq_len(nrow(out))
  out
}


expR5_run_single <- function(condition, n_rounds, seed) {
  ## condition: list-or-tibble-row with mechanism/operator/dag_type/p_post
  ## State-dependent ghost amplitude, as used throughout Paper 2A.
  op_params <- list(amplitude_mode = "state_dependent")
  if (condition$mechanism == "posted_price") {
    op_params$p_post    <- condition$p_post
    op_params$mu_markup <- 0.4
  }

  res <- run_simulation(
    n_rounds         = n_rounds,
    n_agents         = EXPR5_N_AGENTS,
    dag_type         = condition$dag_type,
    load_level       = 1.0,
    operator_type    = condition$operator,
    mechanism        = condition$mechanism,
    operator_params  = op_params,
    value_support    = c(0, EXPR5_V_BAR),
    force_all_active = TRUE,
    seed             = seed
  )
  res %>%
    mutate(
      mechanism_lbl = condition$mechanism,
      operator_lbl  = condition$operator,
      dag_type_lbl  = condition$dag_type,
      p_post_lbl    = condition$p_post,
      condition_id  = condition$condition_id
    )
}


expR5_run_all <- function(conditions, n_rounds = 100, n_seeds = 5) {
  results <- pmap_dfr(conditions, function(...) {
    cond <- tibble(...)
    map_dfr(seq_len(n_seeds), function(s) {
      expR5_run_single(cond, n_rounds, seed = s)
    })
  })
  results
}


expR5_aggregate <- function(results) {
  ## Per (mechanism × dag × operator × p_post) report CoNC^op / CoNC^ag /
  ## CoNC^W / CoNC^op_rev and the operator extraction rate against the matched
  ## truthful baseline. γ_ij mean/std computed separately (topology-only).

  ## Stored artifacts predating the revenue column still aggregate; every
  ## revenue-delta quantity (CoNC^op, CoNC^ag, CoNC^op_rev) comes out NA.
  if (!"revenue" %in% names(results)) results$revenue <- NA_real_

  ## Per-condition mean welfare + mean op surplus across seeds & rounds
  per_cond <- results %>%
    group_by(mechanism_lbl, operator_lbl, dag_type_lbl, p_post_lbl) %>%
    summarise(
      welfare_mean  = mean(welfare, na.rm = TRUE),
      op_surplus    = mean(net_op_surplus, na.rm = TRUE),
      revenue_mean  = mean(revenue, na.rm = TRUE),
      welfare_sd    = sd(welfare, na.rm = TRUE),
      op_surplus_sd = sd(net_op_surplus, na.rm = TRUE),
      n_rounds      = dplyr::n(),
      .groups = "drop"
    )

  ## Identify truthful baseline per (mechanism, dag, p_post)
  baseline <- per_cond %>%
    filter(operator_lbl == "truthful") %>%
    select(mechanism_lbl, dag_type_lbl, p_post_lbl,
           welfare_baseline = welfare_mean,
           op_baseline      = op_surplus,
           rev_baseline     = revenue_mean)

  ## Join + compute CoNC variants
  summary <- per_cond %>%
    left_join(baseline,
              by = c("mechanism_lbl", "dag_type_lbl", "p_post_lbl")) %>%
    mutate(
      ## eq:conc numerator: the collected-revenue delta (= the agents'
      ## aggregate payment increment).  The operator's extraction surplus is a
      ## different measure and is carried alongside; see compute_conc_variants().
      rev_delta = revenue_mean - rev_baseline,
      extraction_op = (op_surplus - op_baseline) / pmax(welfare_baseline, 1e-9),
      CoNC_op = rev_delta / pmax(welfare_baseline, 1e-9),
      CoNC_W  = (welfare_baseline - welfare_mean) / pmax(welfare_baseline, 1e-9),
      CoNC_ag = CoNC_W + CoNC_op,
      ## eq:conc remark: same numerator over truthful operator revenue.
      CoNC_op_rev = rev_delta /
        ifelse(is.finite(rev_baseline) & rev_baseline > 0, rev_baseline, NA_real_)
    )

  ## γ_ij stats per topology
  gamma_stats <- map_dfr(c("tree", "sp", "entangled"), function(top) {
    dag <- make_dag(top)
    env <- make_env()
    g <- compute_gamma_distribution(dag, env, n_samples = 500, seed = 42)
    tibble(
      dag_type_lbl  = top,
      gamma_mean    = mean(g),
      gamma_sd      = sd(g),
      gamma_median  = median(g),
      gamma_q05     = quantile(g, 0.05),
      gamma_q95     = quantile(g, 0.95)
    )
  })

  summary <- summary %>% left_join(gamma_stats, by = "dag_type_lbl")

  list(
    summary     = summary,
    gamma_stats = gamma_stats,
    per_cond    = per_cond
  )
}
