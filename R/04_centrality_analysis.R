# ==========================================================
# 04_centrality_analysis.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Calculate node-level centrality measures for linked dengue
# cases in the spatiotemporal network.
#
# Main measures:
# - degree centrality
# - betweenness centrality
# - closeness centrality
# - eigenvector centrality
#
# These measures describe different aspects of a node's
# structural position within the network.
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load linked network data
# ---------------------------

nodes <- read_csv(
  "data/processed/network_nodes_with_components.csv",
  show_col_types = FALSE
)

edges <- read_csv(
  "data/processed/spatiotemporal_edges_200m_14d.csv",
  show_col_types = FALSE
)


# ---------------------------
# 3. Construct linked network
# ---------------------------

g <- igraph::graph_from_data_frame(
  d = edges,
  vertices = nodes,
  directed = FALSE
)


# ---------------------------
# 4. Degree centrality
# ---------------------------

degree_centrality <- igraph::degree(
  g,
  mode = "all"
)


# ---------------------------
# 5. Betweenness centrality
# ---------------------------

betweenness_centrality <- igraph::betweenness(
  g,
  directed = FALSE,
  normalized = FALSE,
  weights = NA
)


# ---------------------------
# 6. Closeness centrality
# ---------------------------

# Closeness can be problematic in disconnected networks.
# igraph calculates closeness within the reachable portion
# of the network.
#
# Harmonic centrality is also calculated below because it is
# generally more interpretable for disconnected networks.

closeness_centrality <- igraph::closeness(
  g,
  mode = "all",
  normalized = TRUE,
  weights = NA
)


# ---------------------------
# 7. Harmonic centrality
# ---------------------------

harmonic_centrality <- igraph::harmonic_centrality(
  g,
  mode = "all",
  normalized = TRUE,
  weights = NA
)


# ---------------------------
# 8. Eigenvector centrality
# ---------------------------

eigenvector_result <- igraph::eigen_centrality(
  g,
  directed = FALSE,
  weights = NA,
  scale = TRUE
)

eigenvector_centrality <- eigenvector_result$vector


# ---------------------------
# 9. Combine centrality measures
# ---------------------------

centrality_table <- tibble(
  case_id = names(degree_centrality),
  degree = as.numeric(degree_centrality),
  betweenness = as.numeric(betweenness_centrality),
  closeness = as.numeric(closeness_centrality),
  harmonic = as.numeric(harmonic_centrality),
  eigenvector = as.numeric(eigenvector_centrality)
)


# ---------------------------
# 10. Join node attributes
# ---------------------------

centrality_table <- centrality_table %>%
  left_join(
    nodes %>%
      select(
        case_id,
        district,
        mukim,
        age,
        sex,
        occupation,
        citizenship,
        housing,
        component_id
      ),
    by = "case_id"
  )


# ---------------------------
# 11. Rank nodes
# ---------------------------

centrality_ranked <- centrality_table %>%
  mutate(
    degree_rank = min_rank(desc(degree)),
    betweenness_rank = min_rank(desc(betweenness)),
    closeness_rank = min_rank(desc(closeness)),
    harmonic_rank = min_rank(desc(harmonic)),
    eigenvector_rank = min_rank(desc(eigenvector))
  )


# ---------------------------
# 12. Inspect top nodes
# ---------------------------

top_degree <- centrality_ranked %>%
  arrange(desc(degree)) %>%
  slice_head(n = 20)

top_betweenness <- centrality_ranked %>%
  arrange(desc(betweenness)) %>%
  slice_head(n = 20)

top_eigenvector <- centrality_ranked %>%
  arrange(desc(eigenvector)) %>%
  slice_head(n = 20)


# ---------------------------
# 13. District-level summaries
# ---------------------------

district_centrality_summary <- centrality_table %>%
  group_by(district) %>%
  summarise(
    n_nodes = n(),
    mean_degree = mean(degree, na.rm = TRUE),
    median_degree = median(degree, na.rm = TRUE),
    max_degree = max(degree, na.rm = TRUE),
    mean_betweenness = mean(
      betweenness,
      na.rm = TRUE
    ),
    mean_harmonic = mean(
      harmonic,
      na.rm = TRUE
    ),
    mean_eigenvector = mean(
      eigenvector,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ---------------------------
# 14. Round values for reporting
# ---------------------------

centrality_report <- centrality_ranked %>%
  mutate(
    betweenness = round(betweenness, 3),
    closeness = round(closeness, 5),
    harmonic = round(harmonic, 5),
    eigenvector = round(eigenvector, 5)
  )

district_centrality_summary <- district_centrality_summary %>%
  mutate(
    mean_degree = round(mean_degree, 2),
    median_degree = round(median_degree, 2),
    mean_betweenness = round(
      mean_betweenness,
      3
    ),
    mean_harmonic = round(
      mean_harmonic,
      5
    ),
    mean_eigenvector = round(
      mean_eigenvector,
      5
    )
  )


# ---------------------------
# 15. Save outputs
# ---------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  centrality_report,
  "output/tables/node_centrality_measures.csv"
)

write_csv(
  district_centrality_summary,
  "output/tables/district_centrality_summary.csv"
)

write_csv(
  top_degree,
  "output/tables/top20_degree.csv"
)

write_csv(
  top_betweenness,
  "output/tables/top20_betweenness.csv"
)

write_csv(
  top_eigenvector,
  "output/tables/top20_eigenvector.csv"
)


# ==========================================================
# Interpretation notes
#
# Degree:
# Number of direct spatiotemporal connections involving a case.
#
# Betweenness:
# Extent to which a node lies on shortest paths between other
# nodes and may indicate a bridging structural position.
#
# Closeness:
# Proximity of a node to other reachable nodes through shortest
# paths. Interpretation should be cautious in disconnected
# networks.
#
# Harmonic centrality:
# Alternative to closeness that is better suited to disconnected
# networks.
#
# Eigenvector centrality:
# Higher when a node is connected to other structurally important
# nodes.
#
# Centrality measures describe network structure and should not
# be interpreted as proof that an individual case caused or
# transmitted infection to other cases.
# ==========================================================


# ==========================================================
# End of 04_centrality_analysis.R
#
# Next:
# R/05_export_gephi.R
# ==========================================================
