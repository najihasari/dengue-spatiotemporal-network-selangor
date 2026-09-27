# ==========================================================
# 09_ergm_gof.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Evaluate goodness-of-fit of the final ERGM by comparing
# observed network statistics with networks simulated from
# the fitted model.
#
# Main checks:
# - degree distribution
# - geodesic distance distribution
# - edgewise shared partner distribution
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
# 3. Run goodness-of-fit
# ---------------------------

gof_final <- ergm::gof(
  model_final
)


# ---------------------------
# 4. Print GOF results
# ---------------------------

print(
  gof_final
)


# ---------------------------
# 5. Plot GOF
# ---------------------------

plot(
  gof_final
)


# ---------------------------
# 6. Optional targeted GOF
# ---------------------------

# A more explicit GOF specification can be used when needed.
#
# Uncomment if you want to focus on selected network features.

# gof_selected <- ergm::gof(
#   model_final,
#   GOF = ~
#     degree +
#     distance +
#     espartners
# )

# print(
#   gof_selected
# )

# plot(
#   gof_selected
# )


# ---------------------------
# 7. Save GOF object locally
# ---------------------------

dir.create(
  "output/models",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  gof_final,
  "output/models/final_ergm_gof.rds"
)


# ---------------------------
# 8. Optional figure export
# ---------------------------

dir.create(
  "output/figures",
  recursive = TRUE,
  showWarnings = FALSE
)

png(
  filename = "output/figures/final_ergm_gof.png",
  width = 1800,
  height = 1400,
  res = 200
)

plot(
  gof_final
)

dev.off()


# ==========================================================
# Interpretation notes
#
# Goodness-of-fit compares characteristics of the observed
# network with networks simulated from the fitted ERGM.
#
# A satisfactory model should reproduce important structural
# characteristics of the observed network reasonably well.
#
# Degree distribution:
# Assesses whether the model reproduces the observed pattern
# of node connectivity.
#
# Geodesic distance:
# Assesses whether simulated networks reproduce the observed
# shortest-path structure.
#
# Edgewise shared partners:
# Assesses local clustering and shared-neighbour structure.
#
# GOF should not be judged from a single statistic alone.
# Interpretation should consider the overall agreement between
# observed and simulated network characteristics.
# ==========================================================


# ==========================================================
# End of 09_ergm_gof.R
#
# Next:
# R/10_sensitivity_analysis.R
# ==========================================================
