## ── Fig. 2 (2A manuscript): CoNC summary — single-panel closure ──────
##
## Single-panel cross-column figure for Trilogy-2A (Credibility Trilemma).
## Ghost-bid net surplus per round across credibility mechanisms
## (None / Broadcast / Blockchain / Domain Separation), pooled across
## the three DAG topologies (tree / series--parallel / entangled) and
## the three population sizes N in {10, 30, 50} swept by Exp.~2.
##   - data: expE_summary, pooled over (n_agents_cond, dag_type)
##   - error bars: 95% normal-approx CIs using the standard error across
##     the nine (topology, N) cells per credibility level (per-seed
##     aggregation has been collapsed upstream by expE_aggregate)
##   - this pooling matches the §6 qualifier "Exp.~2 sweeps N in
##     {10, 30, 50}" and the headline -7.83 broadcast number in
##     Evaluation §Trilemma Illustration
##
## History: the prior two-panel version included a domain-separation
## knife-edge subplot drawn from Exp L (`expL_summary`). Exp L is the
## domain-separation knife-edge experiment, which belongs to the
## companion paper (Trilogy-2B: Deployable Credibility Surface), not 2A.
## The 2A/2B split on 2026-05-23 left Fig 2's two-panel form misaligned
## with 2A's scope; this single-panel version restores caption/figure
## coherence.
##
## Visual language (unchanged from prior 2A/TSC exemplar):
##   - sans-serif in-figure text
##   - closed-box spines (no Tufte minimal)
##   - light grey gridlines
##   - single-accent palette (vermillion None vs bluish-green mechanisms)
##   - figure dimensions 7.16 in × 2.4 in (ACM TEAC text width fit)

suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

## Locate the repo root portably. Prefers `here::here()` (rprojroot
## discovery); falls back to the current working directory if `here` is
## not installed. Invoke this script from the repo root, or set the
## SIM_DIR environment variable to override.
SIM_DIR <- Sys.getenv("SIM_DIR", "")
if (!nzchar(SIM_DIR)) {
  SIM_DIR <- if (requireNamespace("here", quietly = TRUE)) {
    here::here()
  } else {
    normalizePath(".", mustWork = TRUE)
  }
}
TARGETS_STORE <- file.path(SIM_DIR, "_targets")
OUT_PDF <- file.path(SIM_DIR, "figs", "fig2_conc_summary.pdf")

## ─── Palette (Okabe–Ito subset) ─────────────────────────────────────────
COL_VERMILLION <- "#D55E00"   # baseline / "the gap"
COL_GREEN      <- "#009E73"   # closure mechanisms
COL_GREY       <- "#999999"   # reference lines

## ─── Load data ───────────────────────────────────────────────────────────
## Prefer `targets::tar_read`; fall back to a direct RDS read for cases
## where the targets metadata registry is stale (e.g.\ a partial
## sanitization rewrite) but the object file on disk is fresh.
.load_target <- function(name) {
  out <- tryCatch(
    targets::tar_read_raw(name, store = TARGETS_STORE),
    error = function(e) NULL
  )
  if (is.null(out)) {
    obj_path <- file.path(TARGETS_STORE, "objects", name)
    if (file.exists(obj_path)) {
      out <- readRDS(obj_path)
    }
  }
  if (is.null(out)) {
    stop("Could not locate target '", name, "' in store '", TARGETS_STORE, "'.")
  }
  out
}
expE_summary <- .load_target("expE_summary")

## ─── Closure across mechanisms (single panel) ───────────────────────────
mech_levels <- c("none", "broadcast", "blockchain", "exchange")
mech_labels <- c("None", "Broadcast", "Blockchain", "Domain Sep.")

panel_df <- expE_summary %>%
  group_by(credibility) %>%                            # pool over (N, topology)
  summarise(
    surplus_mean = mean(ghost_surplus_mean),
    surplus_se   = sd(ghost_surplus_mean) / sqrt(n()),  # SE across (N x topology) cells
    .groups = "drop"
  ) %>%
  mutate(
    surplus_lo  = surplus_mean - 1.96 * surplus_se,
    surplus_hi  = surplus_mean + 1.96 * surplus_se,
    credibility = factor(credibility, levels = mech_levels, labels = mech_labels),
    is_baseline = credibility == "None"
  )

fig2 <- ggplot(panel_df,
               aes(x = credibility, y = surplus_mean, fill = is_baseline)) +
  geom_col(width = 0.65, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = surplus_lo, ymax = surplus_hi),
                width = 0.20, linewidth = 0.4) +
  geom_hline(yintercept = 0, colour = COL_GREY,
             linetype = "dashed", linewidth = 0.4) +
  scale_fill_manual(values = c(`TRUE` = COL_VERMILLION,
                               `FALSE` = COL_GREEN),
                    guide = "none") +
  labs(x = NULL,
       y = "Ghost-bid net surplus / round") +
  theme_bw(base_size = 8, base_family = "Helvetica") +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(colour = "grey90", linewidth = 0.3),
    panel.border        = element_rect(colour = "black", fill = NA, linewidth = 0.5),
    axis.text.x         = element_text(size = 8, colour = "black"),
    axis.text.y         = element_text(size = 7, colour = "black"),
    axis.title          = element_text(size = 8),
    plot.margin         = margin(4, 6, 4, 4)
  )

## ─── Save ────────────────────────────────────────────────────────────────
dir.create(dirname(OUT_PDF), showWarnings = FALSE, recursive = TRUE)
ggsave(OUT_PDF, fig2,
       width = 7.16, height = 2.4, units = "in")

cat("Wrote", OUT_PDF, "\n")
cat("Panel data:\n"); print(panel_df)
