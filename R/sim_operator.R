suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

## ── Bang-bang rational adversary under saturating-hazard audit ───────
##
## Per-round expected operator profit under the ghost-bid adversary against
## the saturating-hazard SDS audit channel (cf. Theorem `thm:sds`):
##   Pi(eps) = (1 - eta) * lambda * eps  -  (1 - exp(-beta*eps)) * v_bar*n/tau^2.
##
## Theoretical note: Pi''(eps) = +beta^2 * exp(-beta*eps) * v_bar*n/tau^2 > 0
## on the interior, so Pi is convex in eps. Any interior critical point of
## the first-order condition is therefore a profit MINIMUM, not maximum.
## The rational adversary's optimal policy is bang-bang on the feasible
## amplitude interval [0, eps_max]: play eps_max iff Pi(eps_max) > 0, else 0.
## The natural maximum amplitude is eps_max = 1/beta, the detector scale at
## which the linear (beta*eps) and saturating (1 - exp(-beta*eps)) regimes
## of the hazard cross. At eps_max = 1/beta the bang-bang threshold
## Pi(eps_max) = 0 rearranges to lambda*_audit = beta * v_bar * n / tau^2,
## recovering the SDS theorem's audit boundary.

expected_profit_per_round <- function(epsilon, lambda, eta, tau, beta, v_bar, n) {
  ## Closed-form per-round expected operator profit. Pure function, scalars.
  if (!is.finite(tau) || tau <= 0) {
    ## tau -> infty: penalty term vanishes -> profit = (1-eta)*lambda*epsilon.
    return((1 - eta) * lambda * epsilon)
  }
  gain    <- (1 - eta) * lambda * epsilon
  hazard  <- 1 - exp(-beta * epsilon)
  loss    <- hazard * v_bar * n / (tau ^ 2)
  gain - loss
}


bb_adversary_amplitude <- function(lambda, eta, tau, beta, v_bar, n,
                                   eps_max = NULL) {
  ## Bang-bang amplitude: play eps_max iff Pi(eps_max) > 0, else 0.
  ## Default eps_max = 1/beta (detector scale where linear-vs-saturating
  ## hazard regimes meet).
  if (is.null(eps_max)) eps_max <- 1.0 / beta
  if (!is.finite(eps_max) || eps_max <= 0) return(0)
  profit <- expected_profit_per_round(eps_max, lambda, eta, tau, beta, v_bar, n)
  if (is.finite(profit) && profit > 0) eps_max else 0
}

## ── Operator strategies ─────────────────────────────────────────────
##
## Each operator strategy takes the true allocation and payments from VCG
## and returns a (possibly distorted) allocation and payments.
##
## Operator types:
##   - truthful:       executes VCG exactly
##   - misreporter:    re-runs allocation on reduced capacity (ghost capacity)
##   - inflator:       adds markup to VCG payments
##   - discriminator:  favours affiliated agents by re-ordering priority
##   - ghost_bidder:   injects fictitious bid to extract surplus (trilemma)

make_operator <- function(
  type = c("truthful", "misreporter", "inflator", "discriminator", "ghost_bidder",
           "ghost_bidder_bb", "posted_price_inflator"),
  epsilon   = 0.2,    # capacity misreporting factor
  mu_markup = 0.2,    # price inflation factor
  alpha     = 0.2,    # fraction of affiliated agents
  ghost_value = NULL,  # ghost bid value (auto-calibrated from amplitude_mode if NULL)
  v_max     = 1.0,    # bid-prior upper bound; used to set ghost_value when NULL
  amplitude_mode = c("fixed", "state_dependent"),  # ghost-bid amplitude rule
  escrow_fraction = 0.0,  # eta: fraction of payments in escrow (operator gets (1-eta) share)
  p_post    = 0.5,    # posted-price level (only for mechanism = posted_price)
  eps_max   = NULL,   # ghost_bidder_bb: cap on per-round deviation amplitude (default 1/beta)
  seed      = NULL
) {
  type <- match.arg(type)
  amplitude_mode <- match.arg(amplitude_mode)
  stopifnot(escrow_fraction >= 0, escrow_fraction <= 1)
  stopifnot(v_max > 0)
  stopifnot(is.null(eps_max) || eps_max > 0)

  list(
    type            = type,
    epsilon         = epsilon,
    mu_markup       = mu_markup,
    alpha           = alpha,
    ghost_value     = ghost_value,
    v_max           = v_max,
    amplitude_mode  = amplitude_mode,
    escrow_fraction = escrow_fraction,
    p_post          = p_post,
    eps_max         = eps_max,
    seed            = seed
  )
}


## Apply operator strategy to distort the market outcome
apply_operator_strategy <- function(operator, allocation, payments, env, agents,
                                    dag = NULL) {
  switch(operator$type,

    truthful = list(
      allocation = allocation,
      payments   = payments,
      surplus    = 0,
      detectable = FALSE
    ),

    misreporter = {
      # Re-run greedy allocation with reduced capacity, mimicking the
      # operator claiming lower internal max-flow
      reduced_cap <- env$capacity * (1 - operator$epsilon)
      tasks_df <- allocation %>%
        arrange(desc(realised_value)) %>%
        mutate(
          density = ifelse(allocated, realised_value, 0)
        )

      # Re-allocate with reduced capacity
      remaining <- reduced_cap
      new_alloc <- allocation
      new_alloc$allocated <- FALSE
      new_alloc$realised_value <- 0

      sorted_idx <- order(-allocation$realised_value)
      tiers <- if (!is.null(dag)) dag$critical_path_tiers else env$tiers

      for (i in sorted_idx) {
        if (!allocation$allocated[i]) next
        can_fit <- all(vapply(tiers, function(t) {
          td <- if (!is.null(dag)) dag$tier_demand[t] else 1
          remaining[t] >= td
        }, logical(1)))

        if (can_fit) {
          for (t in tiers) {
            td <- if (!is.null(dag)) dag$tier_demand[t] else 1
            remaining[t] <- remaining[t] - td
          }
          new_alloc$allocated[i] <- TRUE
          new_alloc$realised_value[i] <- allocation$realised_value[i]
        }
      }

      # Re-compute VCG payments on reduced allocation
      # Higher payments due to artificial scarcity
      allocated_mask <- new_alloc$allocated
      n_alloc <- sum(allocated_mask)
      total_cap <- sum(reduced_cap)
      scarcity_factor <- 1 + operator$epsilon * (n_alloc / max(1, sum(allocation$allocated)))
      new_payments <- payments
      new_payments[allocated_mask] <- payments[allocated_mask] * scarcity_factor
      new_payments[!allocated_mask] <- 0

      truthful_revenue <- sum(payments[allocation$allocated], na.rm = TRUE)
      new_revenue <- sum(new_payments[allocated_mask], na.rm = TRUE)

      list(
        allocation = new_alloc,
        payments   = new_payments,
        surplus    = new_revenue - truthful_revenue,
        detectable = FALSE  # agents see only own outcome
      )
    },

    inflator = {
      inflated <- payments * (1 + operator$mu_markup)
      list(
        allocation = allocation,
        payments   = inflated,
        surplus    = sum(inflated - payments, na.rm = TRUE),
        detectable = FALSE
      )
    },

    discriminator = {
      # Re-allocate: affiliated agents get priority in the greedy ordering
      n_affiliated <- ceiling(nrow(agents) * operator$alpha)
      if (!is.null(operator$seed)) set.seed(operator$seed)
      affiliated_ids <- as.character(sample(agents$agent_id, n_affiliated))

      # Re-sort: affiliated first (within each group, by original density)
      new_alloc <- allocation
      is_affil <- allocation$agent_id %in% affiliated_ids
      new_alloc$realised_value[!is_affil & allocation$allocated] <-
        allocation$realised_value[!is_affil & allocation$allocated] * 0.85
      # Welfare loss from misallocation
      welfare_loss <- sum(allocation$realised_value[allocation$allocated]) -
                      sum(new_alloc$realised_value[new_alloc$allocated])

      list(
        allocation = new_alloc,
        payments   = payments,
        surplus    = 0,
        detectable = FALSE,
        welfare_loss = max(0, welfare_loss)
      )
    },

    posted_price_inflator = {
      ## Posted-price deviation: operator inflates the per-tier posted price
      ## post-bid (after agents revealed participation under the announced
      ## p_post). Only agents already allocated under truthful p_post pay the
      ## inflated price; the operator pockets the increment.
      ## Detectable iff the inflated price exceeds any agent's valuation in
      ## an externally observable way; under sealed posting this is masked.
      allocated_mask <- allocation$allocated
      n_alloc <- sum(allocated_mask)
      if (n_alloc == 0) {
        return(list(allocation = allocation, payments = payments,
                    surplus = 0, detectable = FALSE,
                    deviation_amplitude = 0))
      }
      inflated_payments <- payments
      inflated_payments[allocated_mask] <-
        payments[allocated_mask] * (1 + operator$mu_markup)

      gross_surplus <- sum(inflated_payments[allocated_mask] -
                           payments[allocated_mask], na.rm = TRUE)
      surplus <- (1 - operator$escrow_fraction) * gross_surplus

      list(
        allocation          = allocation,
        payments            = inflated_payments,
        surplus             = max(0, surplus),
        detectable          = FALSE,
        deviation_amplitude = operator$mu_markup * mean(payments[allocated_mask],
                                                       na.rm = TRUE)
      )
    },

    ghost_bidder = {
      # Implements the ghost-bid deviation from the credibility trilemma proof.
      # Operator injects a fictitious agent bid, re-runs VCG, and pockets
      # the payment difference.  Each real agent sees an outcome consistent
      # with *some* truthful execution, so the deviation is undetectable.

      allocated_tasks <- allocation %>% filter(allocated)
      if (nrow(allocated_tasks) == 0) {
        return(list(allocation = allocation, payments = payments,
                    surplus = 0, detectable = FALSE,
                    ghost_detected = FALSE,
                    deviation_amplitude = NA_real_))
      }

      # Ghost bid calibrated to displace the marginal allocated task
      marginal_idx <- which(allocation$allocated)
      if (length(marginal_idx) == 0) {
        return(list(allocation = allocation, payments = payments,
                    surplus = 0, detectable = FALSE,
                    ghost_detected = FALSE,
                    deviation_amplitude = NA_real_))
      }
      marginal_task <- allocation[marginal_idx[length(marginal_idx)], ]

      # Ghost-bid amplitude, 1.1 x a reference value:
      #   "fixed" (default)  reference = v_max, so the amplitude is constant
      #                      across rounds.  Matches the SDS theorem's fixed-ε
      #                      framing in `eq:tau-zero-fixed-eps`, on which the
      #                      2B forward calibration depends.
      #   "state_dependent"  reference = the realised value of the displaced
      #                      marginal task, so the ghost is calibrated to the
      #                      round it attacks.  This is the rule Paper 2A's
      #                      experiments (A / E / K / R5) run under.
      # Override via operator$ghost_value if a different amplitude is needed
      # (e.g. unit tests).
      ghost_val <- operator$ghost_value %||% switch(
        operator$amplitude_mode %||% "fixed",
        fixed           = 1.1 * operator$v_max,
        state_dependent = marginal_task$realised_value * 1.1
      )

      # New allocation: remove the marginal task (displaced by ghost)
      new_alloc <- allocation
      displaced_idx <- marginal_idx[length(marginal_idx)]
      new_alloc$allocated[displaced_idx] <- FALSE
      new_alloc$realised_value[displaced_idx] <- 0

      # VCG payments increase for remaining agents because the ghost bid
      # raises their externality
      still_allocated <- which(new_alloc$allocated)
      new_payments <- payments
      if (length(still_allocated) > 0) {
        # Each remaining agent's payment increases by the ghost bid's
        # marginal contribution (shared across agents)
        payment_increase <- ghost_val / length(still_allocated)
        new_payments[still_allocated] <- payments[still_allocated] + payment_increase
      }
      new_payments[displaced_idx] <- 0

      # Operator captures (1 - eta) of the payment increase; the escrowed
      # fraction eta flows directly to resource owners (settlement
      # separation, Mediator-Revenue Regime Classification §3).
      gross_surplus <- sum(new_payments[still_allocated] - payments[still_allocated],
                           na.rm = TRUE)
      surplus <- (1 - operator$escrow_fraction) * gross_surplus

      # Per-agent detectability: each agent observes only their own
      # (allocation, payment) pair. The new payment is consistent with
      # a world where demand was higher, so individually undetectable.
      per_agent_consistent <- TRUE

      list(
        allocation          = new_alloc,
        payments            = new_payments,
        surplus             = max(0, surplus),
        detectable          = FALSE,    # sealed-bid: individually undetectable
        ghost_detected      = FALSE,    # no commitment device
        ghost_value         = ghost_val,
        displaced_task      = marginal_task$task_id,
        # SDS forward-calibration logging: amplitude of the operator's
        # perturbation in valuation space (= ghost-bid magnitude).
        deviation_amplitude = ghost_val
      )
    },

    ghost_bidder_bb = {
      ## Bang-bang rational ghost-bid adversary (Trilogy 2B Exps 8-9).
      ## Under the saturating-hazard SDS audit channel, the per-round expected
      ## profit Pi(eps) is CONVEX in eps (Pi'' > 0), so its interior FOC root
      ## is a profit minimum, not maximum. The rational adversary plays
      ## eps_max iff Pi(eps_max) > 0, else 0 (see bb_adversary_amplitude).
      ## Default eps_max = 1/beta (detector scale).
      ##
      ## The credibility-state parameters (lambda, tau, beta, v_bar, n) are
      ## injected onto the operator object each round by run_market_round()
      ## in sim_market.R; eta is operator$escrow_fraction (constant within
      ## a run). Because Pi has no per-round randomness, the bang-bang decision
      ## is constant within a run -- but we recompute each round for safety.
      lambda <- operator$current_lambda %||% NA_real_
      tau    <- operator$current_tau    %||% NA_real_
      beta   <- operator$current_beta   %||% NA_real_
      v_bar  <- operator$current_v_bar  %||% operator$v_max
      n      <- operator$current_n      %||% length(unique(agents$agent_id))
      eta    <- operator$escrow_fraction

      ## Fallback: if credibility state is unavailable, do not deviate.
      if (any(is.na(c(lambda, tau, beta, v_bar, n)))) {
        eps <- 0
      } else {
        eps_max <- operator$eps_max %||% (1.0 / beta)
        eps <- bb_adversary_amplitude(lambda, eta, tau, beta, v_bar, n,
                                      eps_max = eps_max)
      }

      ## When eps = 0, the operator does not deviate this round.
      allocated_tasks <- allocation %>% filter(allocated)
      if (nrow(allocated_tasks) == 0 || eps <= 0) {
        return(list(
          allocation          = allocation,
          payments            = payments,
          surplus             = 0,
          detectable          = FALSE,
          ghost_detected      = FALSE,
          deviation_amplitude = eps
        ))
      }

      ## Reuse the ghost_bidder mechanic with the chosen eps as the ghost-bid
      ## magnitude (in valuation/payment units).
      marginal_idx <- which(allocation$allocated)
      marginal_task <- allocation[marginal_idx[length(marginal_idx)], ]
      ghost_val <- eps

      new_alloc <- allocation
      displaced_idx <- marginal_idx[length(marginal_idx)]
      new_alloc$allocated[displaced_idx] <- FALSE
      new_alloc$realised_value[displaced_idx] <- 0

      still_allocated <- which(new_alloc$allocated)
      new_payments <- payments
      if (length(still_allocated) > 0) {
        payment_increase <- ghost_val / length(still_allocated)
        new_payments[still_allocated] <- payments[still_allocated] + payment_increase
      }
      new_payments[displaced_idx] <- 0

      gross_surplus <- sum(new_payments[still_allocated] - payments[still_allocated],
                           na.rm = TRUE)
      surplus <- (1 - operator$escrow_fraction) * gross_surplus

      list(
        allocation          = new_alloc,
        payments            = new_payments,
        surplus             = max(0, surplus),
        detectable          = FALSE,
        ghost_detected      = FALSE,
        ghost_value         = ghost_val,
        displaced_task      = marginal_task$task_id,
        deviation_amplitude = ghost_val
      )
    }
  )
}
