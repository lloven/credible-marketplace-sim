## _targets.R — Trustworthy Marketplace Architecture simulation pipeline
## IEEE TSC companion paper
## Experiments A-L (confirmatory ablation) + M-O (robustness)

library(targets)

tar_option_set(
  packages = c("dplyr", "tidyr", "purrr", "ggplot2", "tibble", "scales",
               "patchwork", "boot"),
  format   = "rds"
)

# Source all R files
tar_source("R")

# ── Shared parameters ────────────────────────────────────────────────
n_agents_default  <- 40
n_rounds          <- 100
n_seeds           <- 5

# ── DAG topologies ───────────────────────────────────────────────────
topologies        <- c("tree", "sp", "entangled")

# ── Load levels ──────────────────────────────────────────────────────
load_levels       <- c(low = 0.5, medium = 1.0, high = 1.5)

# ── Operator strategy parameters ─────────────────────────────────────
epsilon_values    <- c(0.1, 0.2, 0.3)
mu_markup_values  <- c(0.1, 0.2, 0.3)

# ── Integrator competition parameters ────────────────────────────────
k_integrators     <- c(1, 2, 3, 5)


list(
  # ================================================================
  # Experiment A: Welfare loss from strategic operators
  # (now includes ghost_bidder for trilemma demonstration)
  # ================================================================
  tar_target(
    expA_conditions,
    expA_design(topologies, load_levels,
                operator_types = c("truthful", "misreporter",
                                   "inflator", "discriminator",
                                   "ghost_bidder"),
                epsilon_values = epsilon_values,
                mu_markup_values = mu_markup_values)
  ),
  tar_target(
    expA_results_raw,
    expA_run_all(expA_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expA_summary, expA_aggregate(expA_results_raw)),
  # eq:conc family for Exp. 1: per (topology x load) cell against its own
  # matched truthful baseline, then the nine-cell ghost-bidder means that
  # sec:eval-conc reports (audit M-D-01 / M-D-02).
  tar_target(expA_conc, expA_conc_by_cell(expA_results_raw)),
  tar_target(expA_conc_headline, expA_headline_conc(expA_conc)),
  tar_target(expA_fig_welfare, plot_expA_welfare(expA_summary)),
  tar_target(expA_fig_welfare_by_load, plot_expA_welfare_by_load(expA_summary)),
  tar_target(expA_fig_surplus, plot_expA_surplus(expA_summary)),
  tar_target(expA_fig_combined, plot_exp1_combined(expA_summary)),

  # ================================================================
  # Experiment B: Credibility mechanisms comparison
  # (now includes ghost_bidder operator)
  # ================================================================
  tar_target(
    expB_conditions,
    expB_design(topologies, load_levels,
                operator_types = c("misreporter", "inflator", "ghost_bidder"),
                cred_types = c("none", "broadcast", "blockchain",
                               "exchange", "regulatory"))
  ),
  tar_target(
    expB_results_raw,
    expB_run_all(expB_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expB_summary, expB_aggregate(expB_results_raw)),
  tar_target(expB_fig_welfare_recovery,
             plot_expB_welfare_recovery(expB_summary)),
  tar_target(expB_fig_tradeoff, plot_expB_tradeoff(expB_summary)),
  tar_target(expB_fig_detection, plot_expB_detection(expB_summary)),
  tar_target(expB_fig_combined, plot_exp3_combined(expB_summary)),

  # ================================================================
  # Experiment C: Integrator competition (with agent choice)
  # ================================================================
  tar_target(
    expC_conditions,
    expC_design(topologies, load_levels,
                k_values = k_integrators,
                strategies = c("competitive", "collusive"))
  ),
  tar_target(
    expC_results_raw,
    expC_run_all(expC_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expC_summary, expC_aggregate(expC_results_raw)),
  tar_target(expC_fig_welfare, plot_expC_welfare(expC_summary)),
  tar_target(expC_fig_price_markup, plot_expC_price_markup(expC_summary)),
  tar_target(expC_fig_welfare_convergence,
             plot_expC_welfare_convergence(expC_summary)),
  tar_target(expC_fig_welfare_by_load,
             plot_expC_welfare_by_load(expC_summary)),
  tar_target(expC_fig_combined, plot_exp5_combined(expC_summary)),

  # ================================================================
  # Experiment D: Two-tier heterogeneous trust (with L2 operator)
  # ================================================================
  tar_target(
    expD_conditions,
    expD_design(topologies, n_agents_set = c(20, 40, 60),
                l1_trust = c("none", "broadcast", "blockchain", "exchange"),
                l2_trust = c("none", "broadcast", "exchange"))
  ),
  tar_target(
    expD_results_raw,
    expD_run_all(expD_conditions, n_rounds, n_seeds)
  ),
  tar_target(expD_summary, expD_aggregate(expD_results_raw)),
  tar_target(expD_fig_heatmap, plot_expD_heatmap(expD_summary)),
  tar_target(expD_fig_frontier, plot_expD_frontier(expD_summary)),
  tar_target(expD_fig_scaling, plot_expD_scaling(expD_summary)),
  tar_target(expD_fig_combined, plot_exp6_combined(expD_summary)),

  # ================================================================
  # Experiment E: Credibility Trilemma Demonstration (Theorem 1)
  # ================================================================
  tar_target(
    expE_conditions,
    expE_design(topologies, n_agents_set = c(10, 30, 50),
                cred_types = c("none", "broadcast", "blockchain", "exchange"))
  ),
  tar_target(
    expE_results_raw,
    expE_run_all(expE_conditions, n_rounds, n_seeds)
  ),
  tar_target(expE_summary, expE_aggregate(expE_results_raw)),
  tar_target(expE_fig_trilemma, plot_expE_trilemma(expE_summary)),
  tar_target(expE_fig_detection, plot_expE_detection_vs_profit(expE_summary)),
  tar_target(expE_fig_scaling, plot_expE_scaling(expE_summary)),
  tar_target(expE_fig_combined, plot_exp2_combined(expE_summary)),

  # ================================================================
  # Experiment F: Domain Separation Validation (Proposition 1)
  # ================================================================
  tar_target(
    expF_conditions,
    expF_design(topologies, load_levels,
                operator_types = c("misreporter", "inflator", "ghost_bidder"),
                fee_modes = c("stake", "separated"))
  ),
  tar_target(
    expF_results_raw,
    expF_run_all(expF_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expF_summary, expF_aggregate(expF_results_raw)),
  tar_target(expF_fig_surplus, plot_expF_surplus(expF_summary)),
  tar_target(expF_fig_welfare, plot_expF_welfare(expF_summary)),
  tar_target(expF_fig_combined, plot_exp4_combined(expF_summary)),

  # ================================================================
  # Experiment G: Sensitivity Analysis (supplementary)
  # ================================================================
  tar_target(expG_conditions, expG_design()),
  tar_target(
    expG_results_raw,
    expG_run_all(expG_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expG_summary, expG_aggregate(expG_results_raw)),
  tar_target(expG_fig_sensitivity, plot_expG_sensitivity(expG_summary)),
  tar_target(expG_fig_detection, plot_expG_detection(expG_summary)),
  tar_target(expG_fig_surplus, plot_expG_surplus(expG_summary)),

  # ================================================================
  # Experiment H: Adaptive (Learning) Operator
  # ================================================================
  tar_target(
    expH_conditions,
    expH_design(topologies,
                n_agents_set = c(10, 25, 50),
                cred_types = c("none", "broadcast", "blockchain", "exchange"))
  ),
  tar_target(
    expH_results_raw,
    expH_run_all(expH_conditions, n_rounds = 200, n_seeds)
  ),
  tar_target(expH_summary, expH_aggregate(expH_results_raw)),
  tar_target(expH_fig_deviation, plot_expH_deviation(expH_summary)),
  tar_target(expH_fig_surplus, plot_expH_surplus(expH_summary)),
  tar_target(expH_fig_converged, plot_expH_converged(expH_summary)),
  tar_target(expH_fig_combined, plot_exp7_combined(expH_summary)),

  # ================================================================
  # Experiment I: Imperfect Broadcast (Credibility Degradation)
  # ================================================================
  tar_target(
    expI_conditions,
    expI_design(topologies,
                p_broadcast_values = c(0.0, 0.3, 0.5, 0.7, 0.9, 0.95, 1.0))
  ),
  tar_target(
    expI_results_raw,
    expI_run_all(expI_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expI_summary, expI_aggregate(expI_results_raw)),
  tar_target(expI_fig_welfare, plot_expI_welfare(expI_summary)),
  tar_target(expI_fig_surplus, plot_expI_surplus(expI_summary)),
  tar_target(expI_fig_combined, plot_exp8_combined(expI_summary)),

  # ================================================================
  # Experiment J: Credibility x Competition Interaction
  # ================================================================
  tar_target(
    expJ_conditions,
    expJ_design(topologies,
                cred_types = c("none", "broadcast", "blockchain", "exchange"),
                k_values = k_integrators)
  ),
  tar_target(
    expJ_results_raw,
    expJ_run_all(expJ_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expJ_summary, expJ_aggregate(expJ_results_raw)),
  tar_target(expJ_fig_synergy, expJ_plot_synergy(expJ_summary)),
  tar_target(expJ_fig_profitability, expJ_plot_profitability(expJ_summary)),
  tar_target(expJ_fig_combined, expJ_plot_combined(expJ_summary)),

  # ================================================================
  # Experiment K: Revenue-Optimal (Myerson) Mechanism (Theorem 1)
  # ================================================================
  tar_target(expK_conditions, expK_design()),
  tar_target(
    expK_results_raw,
    expK_run_all(expK_conditions, n_rounds, n_seeds)
  ),
  tar_target(expK_summary, expK_aggregate(expK_results_raw)),
  tar_target(expK_fig_combined, plot_expK_combined(expK_summary)),

  # ================================================================
  # Experiment L: Domain Separation Knife-Edge (Proposition 1)
  # ================================================================
  tar_target(expL_conditions, expL_design()),
  tar_target(
    expL_results_raw,
    expL_run_all(expL_conditions, n_rounds, n_seeds)
  ),
  tar_target(expL_summary, expL_aggregate(expL_results_raw)),
  tar_target(expL_fig_combined, plot_expL_combined(expL_summary)),

  # ================================================================
  # Experiment M: Strategic Agent Adaptation (robustness)
  # ================================================================
  tar_target(
    expM_conditions,
    expM_design(topologies,
                cred_types  = c("none", "broadcast", "exchange"),
                agent_modes = c("passive", "exit_sensitive"))
  ),
  tar_target(
    expM_results_raw,
    expM_run_all(expM_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expM_summary, expM_aggregate(expM_results_raw)),
  tar_target(expM_fig_combined, plot_expM_combined(expM_summary)),

  # ================================================================
  # Experiment N: Markov Broadcast Channel (robustness)
  # ================================================================
  tar_target(
    expN_conditions,
    expN_design(topologies,
                channel_models    = c("iid", "markov_low", "markov_med", "markov_high"),
                p_stationary_vals = c(0.3, 0.5, 0.7))
  ),
  tar_target(
    expN_results_raw,
    expN_run_all(expN_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expN_summary, expN_aggregate(expN_results_raw)),
  tar_target(expN_fig_combined, plot_expN_combined(expN_summary)),

  # ================================================================
  # Experiment O: Non-Stationary Supply (robustness)
  # ================================================================
  tar_target(
    expO_conditions,
    expO_design(topologies,
                capacity_models = c("static", "cyclic", "shock"),
                cred_types      = c("none", "broadcast", "exchange"))
  ),
  tar_target(
    expO_results_raw,
    expO_run_all(expO_conditions, n_rounds, n_agents_default, n_seeds)
  ),
  tar_target(expO_summary, expO_aggregate(expO_results_raw)),
  tar_target(expO_fig_combined, plot_expO_combined(expO_summary)),

  # ================================================================
  # Experiment L2: SDS Forward Calibration (TEAC RF2/Issue 1)
  # — Exp 9 forward-calibration extension. Adds tau_audit dimension
  #   and ghost_bidder-only sweep on top of the original Exp 9
  #   stake × topology design. See R/sim_expL2.R for design details.
  # ================================================================
  tar_target(expL2_conditions, expL2_design()),
  tar_target(
    expL2_results_raw,
    expL2_run_all(expL2_conditions, n_rounds, n_seeds)
  ),
  tar_target(expL2_summary, expL2_aggregate(expL2_results_raw)),

  # ================================================================
  # Experiment L3: Bilinear (λ, η) Surface (Exp 8 in Trilogy 2B)
  # ================================================================
  tar_target(expL3_conditions, expL3_design()),
  tar_target(
    expL3_results_raw,
    expL3_run_all(expL3_conditions, n_rounds, n_seeds)
  ),
  tar_target(expL3_summary, expL3_aggregate(expL3_results_raw)),
  tar_target(
    expL3_plot,
    save_expL3_plot(expL3_summary, path = "figs/expL3_bilinear_surface.pdf")
  ),

  # ================================================================
  # Experiment L4: 3D credibility-deployable surface (Exp 9 in Trilogy 2B)
  # ================================================================
  tar_target(expL4_conditions, expL4_design()),
  tar_target(
    expL4_results_raw,
    expL4_run_all(expL4_conditions, n_rounds, n_seeds)
  ),
  tar_target(expL4_summary, expL4_aggregate(expL4_results_raw)),
  tar_target(
    expL4_plot,
    save_expL4_plot(expL4_summary, path = "figs/expL4_3d_surface.pdf")
  ),

  # ================================================================
  # Statistical analysis (all experiments)
  # ================================================================
  tar_target(stats_expA, stat_expA(expA_results_raw)),
  tar_target(stats_expB, stat_expB(expB_results_raw)),
  tar_target(stats_expC, stat_expC(expC_results_raw)),
  tar_target(stats_expD, stat_expD(expD_results_raw)),
  tar_target(stats_expE, stat_expE(expE_results_raw)),
  tar_target(stats_expF, stat_expF(expF_results_raw)),
  tar_target(stats_expH, stat_expH(expH_results_raw)),
  tar_target(stats_expI, stat_expI(expI_results_raw)),
  tar_target(stats_expJ, stat_expJ(expJ_results_raw)),
  tar_target(stats_expK, stat_expK(expK_results_raw)),
  tar_target(stats_expL, stat_expL(expL_results_raw)),
  tar_target(stats_expM, stat_expM(expM_results_raw)),
  tar_target(stats_expN, stat_expN(expN_results_raw)),
  tar_target(stats_expO, stat_expO(expO_results_raw)),

  # ================================================================
  # NOTE: Combined figure targets (expX_fig_combined) are defined
  # near each experiment's data and individual-figure targets above.
  # Output files use code letters (expA_combined.pdf, etc.)
  # See README.md for the letter → manuscript number mapping.
  # ================================================================
  NULL  # sentinel — keeps the trailing comma valid
)
