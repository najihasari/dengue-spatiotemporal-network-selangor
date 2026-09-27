# ==========================================================
# 10_sensitivity_analysis.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Assess the robustness of the spatiotemporal linkage
# definition using alternative spatial and temporal thresholds.
#
# Threshold combinations:
# - Spatial: 100 m, 200 m, 300 m
# - Temporal: 7 d, 14 d, 21 d
#
# Primary definition:
# - 200 m / 14 d
#
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load cleaned case data
# ---------------------------

data_clean <- readr::read_csv(
  "data/processed/dengue_selangor_2023_clean.csv",
  show_col_types = FALSE
)


# ---------------------------
# 3. Define thresholds
# ---------------------------

spatial_thresholds <- c(
  100,
  200,
  300
)

temporal_thresholds <- c(
  7,
  14,
  21
)


# ---------------------------
# 4. Create threshold grid
# ---------------------------

threshold_grid <- expand.grid(
  spatial_m = spatial_thresholds,
  temporal_days = temporal_thresholds
)


# ---------------------------
# 5. Function to construct edges
# ---------------------------

build_spatiotemporal_edges <- function(
  data,
  spatial_threshold,
  temporal_threshold
) {

  pairs <- tidyr::crossing(
    source_index = seq_len(nrow(data)),
    target_index = seq_len(nrow(data))
  ) %>%
    dplyr::filter(
      source_index < target_index
    )

  source_data <- data %>%
    dplyr::mutate(
      source_index = dplyr::row_number()
    ) %>%
    dplyr::transmute(
      source_index,
      source = case_id,
      source_onset = onset_date,
      source_lat = latitude,
      source_lon = longitude
    )

  target_data <- data %>%
    dplyr::mutate(
      target_index = dplyr::row_number()
    ) %>%
    dplyr::transmute(
      target_index,
      target = case_id,
      target_onset = onset_date,
      target_lat = latitude,
      target_lon = longitude
    )

  pairs <- pairs %>%
    dplyr::left_join(
      source_data,
      by = "source_index"
    ) %>%
    dplyr::left_join(
      target_data,
      by = "target_index"
    ) %>%
    dplyr::mutate(
      onset_gap_days = abs(
        as.numeric(
          as.Date(source_onset) -
          as.Date(target_onset)
        )
      )
    ) %>%
    dplyr::filter(
      onset_gap_days <= temporal_threshold
    )

  pairs <- pairs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      distance_m = geosphere::distHaversine(
        c(
          source_lon,
          source_lat
        ),
        c(
          target_lon,
          target_lat
        )
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      distance_m <= spatial_threshold
    )

  pairs %>%
    dplyr::select(
      source,
      target,
      distance_m,
      onset_gap_days
    )
}


# ---------------------------
# 6. Function to summarise network
# ---------------------------

summarise_network <- function(
  data,
  edge_data,
  spatial_threshold,
  temporal_threshold
) {

  g <- igraph::graph_from_data_frame(
    d = edge_data,
    vertices = data,
    directed = FALSE
  )

  degree_values <- igraph::degree(g)

  linked_n <- sum(
    degree_values >= 1
  )

  isolate_n <- sum(
    degree_values == 0
  )

  component_info <- igraph::components(g)

  tibble::tibble(
    spatial_m = spatial_threshold,
    temporal_days = temporal_threshold,
    nodes = igraph::vcount(g),
    edges = igraph::ecount(g),
    linked_cases = linked_n,
    isolates = isolate_n,
    linked_percent = round(
      100 * linked_n / igraph::vcount(g),
      1
    ),
    mean_degree = mean(
      degree_values
    ),
    density = igraph::edge_density(
      g,
      loops = FALSE
    ),
    components = component_info$no,
    largest_component = max(
      component_info$csize
    )
  )
}


# ---------------------------
# 7. Run sensitivity analysis
# ---------------------------

sensitivity_results <- list()

for (
  i in seq_len(
    nrow(threshold_grid)
  )
) {

  spatial_value <- threshold_grid$spatial_m[i]

  temporal_value <- threshold_grid$temporal_days[i]

  cat(
    "\nRunning:",
    spatial_value,
    "m /",
    temporal_value,
    "days\n"
  )

  sensitivity_edges <- build_spatiotemporal_edges(
    data = data_clean,
    spatial_threshold = spatial_value,
    temporal_threshold = temporal_value
  )

  sensitivity_results[[i]] <- summarise_network(
    data = data_clean,
    edge_data = sensitivity_edges,
    spatial_threshold = spatial_value,
    temporal_threshold = temporal_value
  )
}


# ---------------------------
# 8. Combine results
# ---------------------------

sensitivity_summary <- dplyr::bind_rows(
  sensitivity_results
)


# ---------------------------
# 9. Round reporting values
# ---------------------------

sensitivity_summary <- sensitivity_summary %>%
  dplyr::mutate(
    mean_degree = round(
      mean_degree,
      2
    ),
    density = signif(
      density,
      4
    )
  )


# ---------------------------
# 10. Mark primary threshold
# ---------------------------

sensitivity_summary <- sensitivity_summary %>%
  dplyr::mutate(
    primary_definition = dplyr::if_else(
      spatial_m == 200 &
      temporal_days == 14,
      "Yes",
      "No"
    )
  )


# ---------------------------
# 11. Display results
# ---------------------------

print(
  sensitivity_summary
)


# ---------------------------
# 12. Save results
# ---------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  sensitivity_summary,
  "output/tables/sensitivity_analysis_summary.csv"
)


# ==========================================================
# Interpretation notes
#
# Sensitivity analysis evaluates whether the observed network
# structure is highly dependent on a single arbitrary linkage
# threshold.
#
# Spatial thresholds:
# - 100 metres
# - 200 metres
# - 300 metres
#
# Temporal thresholds:
# - 7 days
# - 14 days
# - 21 days
#
# The main analytical definition retained for the study is:
#
#   200 metres / 14 days
#
# Network-level statistics are compared across all nine
# threshold combinations.
#
# This analysis assesses robustness of network construction.
# It does not validate confirmed dengue transmission pathways.
# ==========================================================


# ==========================================================
# End of 10_sensitivity_analysis.R
# ==========================================================
