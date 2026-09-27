# ==========================================================
# 07_ergm_analysis.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Fit the final Exponential Random Graph Model (ERGM)
# used to examine factors associated with spatiotemporal
# dengue linkage formation.
#
# Final model terms:
# - edges
# - nodefactor(occupation)
# - nodematch(citizenship, diff = TRUE)
# - nodematch(housing, diff = TRUE)
# - gwdegree(decay = 0.25, fixed = TRUE)
#
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load prepared statnet network
# ---------------------------

nw <- readRDS(
  "data/processed/statnet_network_200m_14d.rds"
)


# ---------------------------
# 3. Inspect network
# ---------------------------

cat(
  "\nNetwork summary\n",
  "Vertices:",
  network::network.size(nw),
  "\n",
  "Edges:",
  network::network.edgecount(nw),
  "\n"
)


# ---------------------------
# 4. Check vertex attributes
# ---------------------------

required_attributes <- c(
  "occupation",
  "citizenship",
  "housing"
)

available_attributes <- network::list.vertex.attributes(nw)

missing_attributes <- setdiff(
  required_attributes,
  available_attributes
)

if (length(missing_attributes) > 0) {

  stop(
    paste(
      "Missing ERGM attributes:",
      paste(
        missing_attributes,
        collapse = ", "
      )
    )
  )
}


# ---------------------------
# 5. Inspect attribute categories
# ---------------------------

cat(
  "\nOccupation categories:\n"
)

print(
  table(
    network::get.vertex.attribute(
      nw,
      "occupation"
    ),
    useNA = "ifany"
  )
)

cat(
  "\nCitizenship categories:\n"
)

print(
  table(
    network::get.vertex.attribute(
      nw,
      "citizenship"
    ),
    useNA = "ifany"
  )
)

cat(
  "\nHousing categories:\n"
)

print(
  table(
    network::get.vertex.attribute(
      nw,
      "housing"
    ),
    useNA = "ifany"
  )
)


# ---------------------------
# 6. Baseline ERGM
# ---------------------------

# The edges-only model represents the baseline tendency
# for edge formation.

model_edges <- ergm::ergm(
  nw ~ edges
)

summary(
  model_edges
)


# ---------------------------
# 7. Attribute model
# ---------------------------

# Adds individual and residential attributes.

model_attributes <- ergm::ergm(
  nw ~
    edges +
    nodefactor("occupation") +
    nodematch(
      "citizenship",
      diff = TRUE
    ) +
    nodematch(
      "housing",
      diff = TRUE
    )
)

summary(
  model_attributes
)


# ---------------------------
# 8. Final ERGM
# ---------------------------

# gwdegree accounts for degree-related network dependence.
#
# The decay parameter is fixed at 0.25 based on the
# final analytical specification.

model_final <- ergm::ergm(

  nw ~

    edges +

    nodefactor(
      "occupation"
    ) +

    nodematch(
      "citizenship",
      diff = TRUE
    ) +

    nodematch(
      "housing",
      diff = TRUE
    ) +

    gwdegree(
      decay = GWDEGREE_DECAY,
      fixed = TRUE
    )
)


# ---------------------------
# 9. Display model results
# ---------------------------

summary(
  model_final
)


# ---------------------------
# 10. Extract coefficients
# ---------------------------

model_coefficients <- summary(
  model_final
)$coefficients


# ---------------------------
# 11. Convert coefficients to table
# ---------------------------

ergm_results <- tibble::tibble(

  term = rownames(
    model_coefficients
  ),

  estimate = model_coefficients[, 1],

  std_error = model_coefficients[, 2],

  z_value = model_coefficients[, 3],

  p_value = model_coefficients[, 4]
)


# ---------------------------
# 12. Calculate odds ratios
# ---------------------------

ergm_results <- ergm_results %>%
  dplyr::mutate(

    odds_ratio = exp(
      estimate
    ),

    lower_95_ci = exp(
      estimate -
        1.96 * std_error
    ),

    upper_95_ci = exp(
      estimate +
        1.96 * std_error
    )
  )


# ---------------------------
# 13. Round results
# ---------------------------

ergm_results_report <- ergm_results %>%
  dplyr::mutate(

    estimate = round(
      estimate,
      3
    ),

    std_error = round(
      std_error,
      3
    ),

    z_value = round(
      z_value,
      3
    ),

    p_value = signif(
      p_value,
      3
    ),

    odds_ratio = round(
      odds_ratio,
      2
    ),

    lower_95_ci = round(
      lower_95_ci,
      2
    ),

    upper_95_ci = round(
      upper_95_ci,
      2
    )
  )


# ---------------------------
# 14. Display reporting table
# ---------------------------

print(
  ergm_results_report
)


# ---------------------------
# 15. Save model locally
# ---------------------------

dir.create(
  "output/models",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  model_final,
  "output/models/final_ergm_model.rds"
)


# ---------------------------
# 16. Save result table
# ---------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  ergm_results_report,
  "output/tables/final_ergm_results.csv"
)


# ==========================================================
# Interpretation notes
#
# ERGM coefficients are expressed on the log-odds scale.
#
# Odds ratios are obtained using:
#
#   exp(coefficient)
#
# OR > 1:
# Higher odds of forming a network linkage.
#
# OR < 1:
# Lower odds of forming a network linkage.
#
# nodefactor("occupation"):
# Estimates differences in linkage propensity by occupation
# category relative to the reference category.
#
# nodematch("citizenship", diff = TRUE):
# Estimates within-category citizenship homophily.
#
# nodematch("housing", diff = TRUE):
# Estimates within-category housing homophily.
#
# gwdegree:
# Captures degree-related structural dependence in the
# network.
#
# IMPORTANT:
# ERGM associations describe network linkage formation and
# should not be interpreted as proof of causal transmission.
# ==========================================================


# ==========================================================
# End of 07_ergm_analysis.R
#
# Next:
# R/08_ergm_diagnostics.R
# ==========================================================
