suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

## ── Plots for Experiment R5 (TEAC R-5 panels) ────────────────────────
## (a) CoNC^op + CoNC^ag grouped bars by mechanism × dag
## (b) γ_ij histogram per topology (one facet per topology)
## (c) Realisation-wise vs in-expectation reconciliation

## Arm label. p_post is a genuine design dimension only on the posted-price
## mechanism (it is a nominal placeholder under VCG / first-price), and on the
## posted-price arms CoNC^op moves strongly with it — reversing SIGN on the
## ghost arm between p_post = 0.5 and 0.8 — so those arms are kept separate
## instead of being averaged into a meaningless mean.
.expR5_arm_label <- function(mechanism_lbl, operator_lbl, p_post_lbl) {
  op   <- sub("_bidder$", "", sub("^posted_price_", "", operator_lbl))
  base <- paste(mechanism_lbl, op, sep = " / ")
  ifelse(mechanism_lbl == "posted_price",
         sprintf("%s\np = %.1f", base, p_post_lbl), base)
}

plot_expR5_conc_by_mechanism <- function(summary_obj, bar_width = 0.6,
                                         panel_tag = NULL) {
  ## The CoNC variants are ADDITIVE: CoNC^ag = CoNC^op + CoNC^W (the agents'
  ## payment increment plus the welfare they lose). So we STACK op + W; the
  ## diamond marks the total, the agent-side cost CoNC^ag. CoNC^op here is the
  ## eq:conc quantity — the collected-revenue delta — not the operator's
  ## extraction surplus, and it can be NEGATIVE (a ghost bid that displaces a
  ## paying agent can lower collected revenue while still paying the operator),
  ## hence the zero line and the explicit total marker.
  ## Topology barely matters within an arm (spread < 0.008 on CoNC^ag), so we
  ## pool across DAG topologies and show the spread as an error bar on the
  ## total — freeing the figure to make the mechanism-class comparison and the
  ## op/W decomposition the visual message (contrast Fig 4, where topology IS
  ## the message and gets a shared-axis treatment). Values span three orders of
  ## magnitude, so CoNC^op is printed above each bar: on a linear axis the
  ## VCG / first-price bars are below the resolution, and a log axis cannot
  ## carry either the negative arm or the additive stack.
  pooled <- summary_obj$summary %>%
    filter(operator_lbl != "truthful") %>%
    mutate(arm = .expR5_arm_label(mechanism_lbl, operator_lbl, p_post_lbl)) %>%
    group_by(arm) %>%
    summarise(op = mean(CoNC_op, na.rm = TRUE),
              W  = mean(CoNC_W,  na.rm = TRUE),
              ag = mean(CoNC_ag, na.rm = TRUE),
              ag_sd = sd(CoNC_ag, na.rm = TRUE), .groups = "drop") %>%
    arrange(ag) %>%
    mutate(arm = factor(arm, levels = arm),
           lbl = formatC(op, format = "g", digits = 3),
           lbl_y = pmax(ag, W, 0))

  stack_df <- pooled %>%
    select(arm, op, W) %>%
    pivot_longer(c(op, W), names_to = "component", values_to = "value") %>%
    mutate(component = factor(component, levels = c("W", "op")))

  span <- max(pooled$lbl_y) - min(pooled$op, 0)

  ggplot(stack_df, aes(x = arm, y = value, fill = component)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey40") +
    geom_col(width = bar_width, colour = "white", linewidth = 0.3) +
    geom_errorbar(data = pooled, inherit.aes = FALSE,
                  aes(x = arm, ymin = ag - ag_sd, ymax = ag + ag_sd),
                  width = 0.18, linewidth = 0.4) +
    geom_point(data = pooled, inherit.aes = FALSE,
               aes(x = arm, y = ag), shape = 23, size = 1.9,
               fill = "white", colour = "black", stroke = 0.5) +
    geom_text(data = pooled, inherit.aes = FALSE,
              aes(x = arm, y = lbl_y + 0.05 * span, label = lbl),
              size = 2.7, colour = "grey20") +
    scale_fill_manual(
      values = c(W = "#FDAE61", op = "#D7191C"),
      breaks = c("W", "op"),
      labels = c(expression(CoNC^{W} ~ "(welfare destruction)"),
                 expression(CoNC^{op} ~ "(revenue delta)")),
      name = NULL) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.12))) +
    labs(x = NULL, y = expression(CoNC ~ "(dimensionless)"),
         title = panel_tag %||% "Cost of Non-Credibility by mechanism / operator",
         subtitle = expression("Stacked " * CoNC^{op} * " + " * CoNC^{W} * " = " *
                               CoNC^{ag} * " (diamond); printed values are " *
                               CoNC^{op})) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 7),
          legend.position = "bottom",
          plot.margin = margin(t = 5, r = 8, b = 5, l = 14))
}


## Cliff's delta effect size: P(X>Y) - P(X<Y) for two samples. Honest
## overlap-aware measure of how separated two distributions are
## (+1 = X strictly above Y, 0 = full overlap). Used here to quantify the
## tree -> sp -> entangled shift without a parametric model or a p-value.
cliffs_delta <- function(x, y) {
  gt <- sum(outer(x, y, ">"))
  lt <- sum(outer(x, y, "<"))
  (gt - lt) / (length(x) * length(y))
}

plot_expR5_gamma_distribution <- function(summary_obj) {
  ## All three topologies on ONE shared axis so the rightward shift
  ## (tree < sp < entangled) and the honest overlap are visible at a glance.
  ## Kernel densities (not a parametric fit: gamma_ij is a deterministic
  ## pushforward of the value/deadline draws, so we describe shape without
  ## claiming a distributional family). Mean markers + adjacent-pair Cliff's
  ## delta quantify the shift.
  topos <- c("tree", "sp", "entangled")
  pal   <- c(tree = "#1b9e77", sp = "#7570b3", entangled = "#d95f02")
  all_df <- purrr::map_dfr(topos, function(top) {
    g <- compute_gamma_distribution(make_dag(top), make_env(),
                                    n_samples = 500, seed = 42)
    tibble(dag_type_lbl = factor(top, levels = topos), gamma = g)
  })
  means <- all_df %>% group_by(dag_type_lbl) %>%
    summarise(mu = mean(gamma), .groups = "drop")

  ## Adjacent-pair Cliff's delta (tree->sp, sp->entangled). plotmath label
  ## so the Greek delta and arrows render on the default pdf device.
  gv <- function(t) all_df$gamma[all_df$dag_type_lbl == t]
  d_ts <- cliffs_delta(gv("sp"),        gv("tree"))
  d_se <- cliffs_delta(gv("entangled"), gv("sp"))
  ann <- sprintf(
    "\"Cliff's \" * delta * \": tree\" %%->%% \"sp =\" ~ %.2f * \", sp\" %%->%% \"entangled =\" ~ %.2f",
    d_ts, d_se)

  ggplot(all_df, aes(x = gamma, colour = dag_type_lbl, fill = dag_type_lbl)) +
    geom_density(alpha = 0.25, linewidth = 0.7) +
    geom_vline(data = means, aes(xintercept = mu, colour = dag_type_lbl),
               linetype = "dashed", linewidth = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = pal, name = "Topology") +
    scale_fill_manual(values = pal, name = "Topology") +
    annotate("text", x = Inf, y = Inf, label = ann, parse = TRUE,
             hjust = 1.02, vjust = 1.6, size = 3.1) +
    labs(x = expression(gamma[ij] ~ "(realised submodularity gap)"),
         y = "Density",
         title = expression("Submodularity gap " * gamma[ij] *
                            " shifts with topology: tree < sp < entangled"),
         subtitle = "Dashed lines: per-topology means. Distributions overlap; the ordering is in the central tendency.") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          plot.margin = margin(t = 5, r = 10, b = 5, l = 10))
}


## Helper: ag-ordered mechanism/operator levels (deviations only), so the
## bar panel and the violin panel share an identical x-axis.
.expR5_mechop_levels <- function(summary_obj) {
  summary_obj$summary %>%
    filter(operator_lbl != "truthful") %>%
    mutate(arm = .expR5_arm_label(mechanism_lbl, operator_lbl, p_post_lbl)) %>%
    group_by(arm) %>%
    summarise(ag = mean(CoNC_ag, na.rm = TRUE), .groups = "drop") %>%
    arrange(ag) %>%
    pull(arm)
}

## Panel (b): the REALISATION distribution. Per-round net operator surplus
## across all rounds/seeds/topologies per adversarial condition, as violins
## against the zero line. Shows the trilemma is realisation-wise — the bulk
## of per-round realisations sits above 0, not merely the mean. (A different
## glyph from the bars because it is a different quantity: a distribution of
## realisations, not a decomposed aggregate.)
plot_expR5_surplus_realisation <- function(results_raw, lvl) {
  df <- results_raw %>%
    filter(operator_lbl != "truthful") %>%
    mutate(mech_op = factor(.expR5_arm_label(mechanism_lbl, operator_lbl,
                                             p_post_lbl),
                            levels = lvl))
  ggplot(df, aes(x = mech_op, y = net_op_surplus)) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3,
               colour = "grey50") +
    geom_violin(fill = "#2C7BB6", alpha = 0.35, colour = "#2C7BB6",
                linewidth = 0.4, scale = "width") +
    stat_summary(fun = median, geom = "point", size = 1.4, colour = "#08519c") +
    labs(x = NULL, y = "Per-round operator surplus",
         title = "(b) Realisation distribution",
         subtitle = "Per-round surplus across rounds/seeds/topologies; bulk above 0 = realisation-wise extraction") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

## Combined two-panel figure: (a) decomposed aggregate cost (stacked bars),
## (b) realisation distribution (violins), on a shared mechanism/operator
## x-axis. Replaces the former separate conc-bars and realisation figures.
plot_expR5_conc_combined <- function(summary_obj, results_raw) {
  lvl <- .expR5_mechop_levels(summary_obj)
  p_a <- plot_expR5_conc_by_mechanism(summary_obj, bar_width = 0.5,
                                      panel_tag = "(a) Aggregate cost decomposition")
  p_b <- plot_expR5_surplus_realisation(results_raw, lvl)
  p_a + p_b + patchwork::plot_layout(widths = c(1, 1))
}


save_expR5_plots <- function(summary_obj,
                              out_dir = "figs",
                              prefix  = "expR5") {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  p1 <- plot_expR5_conc_by_mechanism(summary_obj)   # Fig: CoNC stacked bars
  p2 <- plot_expR5_gamma_distribution(summary_obj)  # Fig: gamma_ij densities
  ggsave(file.path(out_dir, paste0(prefix, "_conc_by_mechanism.pdf")),
         p1, width = 8, height = 4.4)
  ggsave(file.path(out_dir, paste0(prefix, "_gamma_distribution.pdf")),
         p2, width = 9, height = 3.8)
  invisible(list(p1 = p1, p2 = p2))
}
