## ── Raw-output deposit: _targets store -> exports/*.csv.gz ────────────
##
## Exports the Trilogy-2A raw per-round results and their summaries as
## gzipped CSV, so the manuscript's data-availability statement has an
## in-repo referent that does not require R or the targets cache.
##
## Run after the 2A pipeline (see `make reproduce`), or standalone:
##   Rscript scripts/export_raw.R
##
## Objects are read straight from `_targets/objects/` rather than via
## targets::tar_read(): expR5_* are written into the store by
## scripts/run_expR5_standalone.R and are not registered in the targets
## metadata (E1a 05-e1a-2a-empirics.md §0.6).

store <- "_targets/objects"
outdir <- "exports"
dir.create(outdir, showWarnings = FALSE)

targets_2a <- c(
  "expA_results_raw", "expE_results_raw", "expK_results_raw", "expR5_results_raw",
  "expA_summary", "expE_summary", "expK_summary", "expR5_summary"
)

write_gz <- function(df, name) {
  path <- file.path(outdir, paste0(name, ".csv.gz"))
  con <- gzfile(path, "wt")
  on.exit(close(con))
  utils::write.csv(as.data.frame(df), con, row.names = FALSE)
  cat(sprintf("  %-34s %6d rows x %3d cols\n", basename(path), nrow(df), ncol(df)))
}

for (nm in targets_2a) {
  path <- file.path(store, nm)
  if (!file.exists(path)) {
    stop("missing store object: ", path, " -- run the 2A pipeline first")
  }
  obj <- readRDS(path)
  if (is.data.frame(obj)) {
    write_gz(obj, nm)
  } else if (is.list(obj)) {
    # expR5_summary is a list of tibbles (summary / gamma_stats / per_cond)
    for (part in names(obj)) write_gz(obj[[part]], paste0(nm, "_", part))
  } else {
    stop("unsupported object type for ", nm, ": ", class(obj)[1])
  }
}

sizes <- file.info(list.files(outdir, pattern = "\\.csv\\.gz$", full.names = TRUE))$size
cat(sprintf("exports/: %d files, %.1f MB total\n", length(sizes), sum(sizes) / 1024^2))
