# Reproduction entry points for the Trilogy-2A (TEAC) release.
#
#   make test        run the testthat suite
#   make figures     rebuild the 2A pipeline + every 2A figure
#   make exports     write exports/*.csv.gz from the _targets store
#   make reproduce   all of the above, in order (test -> figures -> exports)
#
# `make reproduce` regenerates every number and figure reported in 2A §5
# from source. Full cost on a MacBook: ~42 min for the targets subset plus
# ~8 min for the R-5 driver.
#
# Scope: the 2A closure only. The 2B targets (expB / expC / expD / expF /
# expJ / expL / expL2 / expL3 / expL4) are deliberately NOT built here --
# the per-round `revenue` column added in c2e5e1b invalidates them, and
# their rebuild is 2B-campaign business.

R := Rscript

# expR5 is not registered in _targets.R; it is driven by a standalone script
# that writes its objects into the store (see scripts/run_expR5_standalone.R).
# Fig. 2 is likewise produced by a script, not a target.
TARGETS_2A := \
	expA_conditions expA_results_raw expA_summary stats_expA \
	expA_fig_welfare expA_fig_welfare_by_load expA_fig_surplus expA_fig_combined \
	expE_conditions expE_results_raw expE_summary stats_expE \
	expE_fig_trilemma expE_fig_detection expE_fig_scaling expE_fig_combined \
	expK_conditions expK_results_raw expK_summary stats_expK expK_fig_combined

TARGETS_2A_R := $(shell printf '"%s",' $(TARGETS_2A) | sed 's/,$$//')

.PHONY: reproduce test figures exports
.NOTPARALLEL:

reproduce: test figures exports

test:
	$(R) -e 'testthat::test_dir("tests")'

figures:
	$(R) -e 'targets::tar_make(names = c($(TARGETS_2A_R)))'
	$(R) scripts/run_expR5_standalone.R
	$(R) scripts/plots_fig2_summary.R

exports:
	$(R) scripts/export_raw.R
