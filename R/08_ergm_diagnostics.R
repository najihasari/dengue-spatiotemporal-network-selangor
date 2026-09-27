# ==========================================================
# 08_ergm_diagnostics.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Assess MCMC convergence and model stability for the final ERGM.
#
# Main checks:
# - MCMC trace plots
# - autocorrelation
# - parameter diagnostics
#
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load final ERGM model
# ---------------------------

model_final <- readRDS(
  "output/models/final_ergm_model.rds"
)


# ---------------------------
# 3. Model summary
# ---------------------------

summary(
  model_final
)


# ---------------------------
# 4. MCMC diagnostics
# ---------------------------

ergm::mcmc.diagnostics(
  model_final
)


# ---------------------------
# 5. Extract MCMC sample
# ---------------------------

mcmc_sample <- model_final$sample


# ---------------------------
# 6. Basic MCMC summary
# ---------------------------

if (!is.null(mcmc_sample)) {

  print(
    summary(
      mcmc_sample
    )
  )

}


# ---------------------------
# 7. Autocorrelation check
# ---------------------------

if (!is.null(mcmc_sample)) {

  print(
    coda::autocorr.diag(
      mcmc_sample
    )
  )

}


# ---------------------------
# 8. Effective sample size
# ---------------------------

if (!is.null(mcmc_sample)) {

  effective_sample_size <- coda::effectiveSize(
    mcmc_sample
  )

  print(
    effective_sample_size
  )

}


# ---------------------------
# 9. Save diagnostic summary
# ---------------------------

if (!is.null(mcmc_sample)) {

  diagnostic_summary <- tibble::tibble(
    parameter = names(
      effective_sample_size
    ),
    effective_sample_size = as.numeric(
      effective_sample_size
    )
  )

  dir.create(
    "output/tables",
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    diagnostic_summary,
    "output/tables/ergm_mcmc_diagnostics.csv"
  )

}


# ==========================================================
# Interpretation notes
#
# Trace plots:
# Chains should fluctuate around a stable mean without
# obvious trends or prolonged drift.
#
# Autocorrelation:
# High autocorrelation indicates that successive MCMC samples
# are strongly dependent.
#
# Effective sample size:
# Larger values indicate more independent information in the
# simulated MCMC sample.
#
# Diagnostics should be considered together rather than relying
# on a single statistic.
#
# A converged model should demonstrate stable chains and
# adequate mixing before substantive interpretation.
# ==========================================================


# ==========================================================
# End of 08_ergm_diagnostics.R
#
# Next:
# R/09_ergm_gof.R
# ==========================================================
