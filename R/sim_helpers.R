suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── DAG topologies ──────────────────────────────────────────────────
## Each topology defines per-tier demand multipliers reflecting
## structural properties of the service-dependency graph.

make_dag <- function(type = c("tree", "sp", "entangled")) {
  type <- match.arg(type)
  switch(type,
    tree = list(
      type = "tree",
      tier_demand = c(device = 1.0, edge = 1.0, cloud = 1.0),
      n_services  = 3,
      critical_path_tiers = c("device", "edge", "cloud")
    ),
    sp = list(
      type = "sp",
      tier_demand = c(device = 0.8, edge = 1.0, cloud = 1.5),
      n_services  = 4,
      critical_path_tiers = c("device", "edge", "cloud")
    ),
    entangled = list(
      type = "entangled",
      tier_demand = c(device = 1.8, edge = 1.2, cloud = 1.0),
      n_services  = 5,
      critical_path_tiers = c("device", "edge", "cloud", "device")
    )
  )
}


## ── Environment ─────────────────────────────────────────────────────

make_env <- function(
  tier_capacities = c(device = 200, edge = 300, cloud = 500),
  tier_latencies  = c(device = 5, edge = 15, cloud = 50)
) {
  list(
    tiers     = names(tier_capacities),
    capacity  = tier_capacities,
    latency   = tier_latencies
  )
}


## ── Agents ──────────────────────────────────────────────────────────

make_agents <- function(n_agents, seed = 1) {
  set.seed(seed)
  tibble(
    agent_id = seq_len(n_agents),
    trust    = rep(0.8, n_agents),
    budget   = runif(n_agents, 5, 15)
  )
}


## ── Tasks ───────────────────────────────────────────────────────────

generate_tasks <- function(agents, lambda = 1.0, deadlines = c(100, 150, 200),
                           value_support = c(1, 2), seed = NULL,
                           force_all_active = FALSE) {
  ## force_all_active = TRUE makes every agent generate exactly one task per
  ## round, bypassing Poisson dropout.  Required by the SDS forward-calibration
  ## experiment where n in the audit-channel formula is the per-round active
  ## bidder count and must equal the agent pool n.
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(agents)
  if (force_all_active) {
    n_tasks <- rep(1L, n)
  } else {
    n_tasks <- rpois(n, lambda)
  }

  tasks <- map2_dfr(agents$agent_id, n_tasks, function(aid, nt) {
    if (nt == 0) return(tibble())
    tibble(
      agent_id = aid,
      task_id  = paste0(aid, "_", seq_len(nt)),
      value    = runif(nt, value_support[1], value_support[2]),
      deadline = sample(deadlines, nt, replace = TRUE),
      lambda_l = 0.005
    )
  })
  tasks
}


## ── Latency model ───────────────────────────────────────────────────

compute_latency <- function(tier, utilisation, env) {
  base <- env$latency[tier]
  rho  <- min(0.99, max(0, utilisation))
  # M/M/1 mean sojourn time: 1/(mu - lambda) = 1/(mu*(1 - rho))
  # Scaled to ms: base_service_time / (1 - rho)
  congestion <- min(500, base / (1 - rho) - base)
  base + congestion
}

compute_critical_path_latency <- function(dag, env, utilisation) {
  tiers <- dag$critical_path_tiers
  sum(vapply(tiers, function(t) compute_latency(t, utilisation[t], env), numeric(1)))
}


## ── Value computation ───────────────────────────────────────────────

latency_discount <- function(latency, lambda_l = 0.005) {
  exp(-lambda_l * latency)
}

task_value <- function(base_value, latency, deadline, lambda_l = 0.005) {
  if (latency > deadline) return(0)
  base_value * latency_discount(latency, lambda_l)
}


## ── Welfare computation ─────────────────────────────────────────────

compute_welfare <- function(task_outcomes, gamma_c = 0.05, utilisation = NULL) {
  total_value <- sum(task_outcomes$realised_value, na.rm = TRUE)
  congestion_penalty <- 0
  if (!is.null(utilisation)) {
    congestion_penalty <- gamma_c * sum(pmax(utilisation - 1, 0)^2)
  }
  total_value - congestion_penalty
}


## ── CoNC variant aggregator ─────────────────────────────────────────
## Given matched truthful + deviated result tibbles (each with columns
## `welfare`, `operator_surplus` or `net_op_surplus`), compute the three
## CoNC variants:
##   CoNC^op = (E[op_surplus | deviated] - E[op_surplus | truthful]) /
##              E[welfare | truthful]      (operator-side extraction rate)
##   CoNC^W  = (E[welfare | truthful] - E[welfare | deviated]) /
##              E[welfare | truthful]      (welfare-side loss rate)
##   CoNC^ag = CoNC^W + CoNC^op            (agent-side surplus loss:
##                                          welfare loss + operator transfer)
##   CoNC^op_rev = (E[op_surplus | deviated] - E[op_surplus | truthful]) /
##              E[revenue | truthful]      (same numerator over truthful
##                                          operator revenue; reported for the
##                                          cor:conc-lb cells, NA when the
##                                          input frames have no revenue column)
## All three are dimensionless; the manuscript Theorem on CoNC predicts
## CoNC^ag >= CoNC^op in absolute value (agents bear both the transfer and
## the DWL).

compute_conc_variants <- function(truthful, deviated) {
  ## Pick the operator-surplus column flexibly.
  op_col <- function(df) {
    if ("net_op_surplus" %in% names(df)) df$net_op_surplus
    else if ("operator_surplus" %in% names(df)) df$operator_surplus
    else stop("compute_conc_variants: no operator_surplus column found")
  }
  W_truth   <- mean(truthful$welfare, na.rm = TRUE)
  W_dev     <- mean(deviated$welfare, na.rm = TRUE)
  op_truth  <- mean(op_col(truthful), na.rm = TRUE)
  op_dev    <- mean(op_col(deviated), na.rm = TRUE)

  ## Revenue-normalised operator ratio (eq:conc remark): same numerator, but
  ## divided by truthful operator revenue instead of truthful welfare. NA when
  ## the frames predate the revenue column (stored artifacts).
  rev_truth <- if ("revenue" %in% names(truthful)) {
    mean(truthful$revenue, na.rm = TRUE)
  } else NA_real_

  if (!is.finite(W_truth) || W_truth <= 0) {
    return(list(CoNC_op = NA_real_, CoNC_ag = NA_real_, CoNC_W = NA_real_,
                CoNC_op_rev = NA_real_))
  }
  CoNC_op <- (op_dev - op_truth) / W_truth
  CoNC_W  <- (W_truth - W_dev) / W_truth
  CoNC_ag <- CoNC_W + CoNC_op
  CoNC_op_rev <- if (is.finite(rev_truth) && rev_truth > 0) {
    (op_dev - op_truth) / rev_truth
  } else NA_real_
  list(CoNC_op = CoNC_op, CoNC_ag = CoNC_ag, CoNC_W = CoNC_W,
       CoNC_op_rev = CoNC_op_rev)
}


## ── γ_ij distribution extractor ──────────────────────────────────────
## Polymatroid submodularity gap γ_ij = f({i}) + f({j}) - f({i, j}).
## We instantiate f as the achievable per-task welfare on the polymatroid:
## allocating a single task of unit value gives f({i}) = realised_value of
## a generic task at empty utilisation. The pair-evaluation f({i, j})
## measures congestion-induced welfare loss when two tasks compete on the
## critical path. γ measures how submodular the welfare function is.
##
## For a generic DAG the gap is bounded by the per-tier capacity slack and
## the critical-path latency; in the manuscript's CoNC lower bound it
## appears as a Θ(1) structural quantity.

compute_gamma_distribution <- function(dag, env, n_samples = 100, seed = 1) {
  set.seed(seed)
  tiers <- dag$critical_path_tiers
  tier_demands <- dag$tier_demand[tiers]

  ## Singleton welfare: a single unit-value task allocated at empty load
  ## yields realised_value = task_value(1, base_latency, deadline, lambda).
  base_latency <- sum(env$latency[tiers])
  deadline <- 200  # mid-range deadline from default {100,150,200}
  lambda_l <- 0.005
  f_single <- task_value(1.0, base_latency, deadline, lambda_l)

  ## Pair welfare: two tasks competing on the same critical path. Capacity
  ## is consumed by both. We compute realised welfare for the joint
  ## allocation drawn from per-sample utilisation: utilisation increases
  ## by 2*tier_demand/capacity, latency inflates accordingly, realised
  ## value drops.
  gamma <- numeric(n_samples)
  for (s in seq_len(n_samples)) {
    ## Random scaling: simulate variability in deadline/value pairs across
    ## an agent population. Both agents in the pair get the same draw.
    v <- runif(1, 0.5, 1.5)
    d <- sample(c(100, 150, 200), 1)
    ## Singleton: utilisation = tier_demand / capacity
    util_single <- tier_demands / env$capacity[tiers]
    lat_single  <- compute_critical_path_latency(dag, env,
                      setNames(util_single, tiers))
    f_i <- task_value(v, lat_single, d, lambda_l)
    f_j <- f_i  # symmetric

    ## Pair: utilisation doubled
    util_pair <- 2 * tier_demands / env$capacity[tiers]
    lat_pair  <- compute_critical_path_latency(dag, env,
                   setNames(util_pair, tiers))
    rv_pair <- task_value(v, lat_pair, d, lambda_l)
    f_ij <- 2 * rv_pair  # two tasks allocated jointly, each yields rv_pair

    gamma[s] <- (f_i + f_j) - f_ij
  }
  gamma
}


## ── Bootstrap confidence interval helper ────────────────────────────

boot_ci <- function(x, n_boot = 1000, conf = 0.95) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(lo = NA_real_, hi = NA_real_))
  means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  alpha <- (1 - conf) / 2
  qs <- quantile(means, c(alpha, 1 - alpha))
  c(lo = unname(qs[1]), hi = unname(qs[2]))
}
