# ==========================================================
# 03_network_descriptive_analysis.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Calculate descriptive network statistics for the linked-case
# dengue networks.
#
# Main outputs:
# - number of nodes
# - number of edges
# - mean degree
# - maximum degree
# - network density
# - connected components
# - largest component size
# - average component size
# - diameter
# - average path length
#
# Analysis is performed overall and by district.
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load network data
# ---------------------------

nodes_network <- read_csv(
  "data/processed/network_nodes_200m_14d.csv",
  show_col_types = FALSE
)

edges <- read_csv(
  "data/processed/spatiotemporal_edges_200m_14d.csv",
  show_col_types = FALSE
)


# ---------------------------
# 3. Retain linked cases
# ---------------------------

linked_nodes <- nodes_network %>%
  filter(linkage_status == "Linked")


# ---------------------------
# 4. Construct overall linked network
# ---------------------------

g_linked <- igraph::graph_from_data_frame(
  d = edges,
  vertices = linked_nodes,
  directed = FALSE
)


# ---------------------------
# 5. Function for network statistics
# ---------------------------

calculate_network_statistics <- function(g, network_name = "Overall") {

  n_nodes <- igraph::vcount(g)
  n_edges <- igraph::ecount(g)

  degree_values <- igraph::degree(g)

  component_info <- igraph::components(g)

  component_sizes <- component_info$csize

  largest_component_size <- max(component_sizes)

  mean_component_size <- mean(component_sizes)

  network_density <- igraph::edge_density(
    g,
    loops = FALSE
  )

  mean_degree <- mean(degree_values)

  max_degree <- max(degree_values)

  # Diameter and mean path length may be undefined for
  # disconnected networks if calculated on the full graph.
  #
  # Therefore, these are calculated on the largest connected
  # component.

  largest_component_id <- which.max(component_sizes)

  largest_component_vertices <- which(
    component_info$membership == largest_component_id
  )

  g_largest <- igraph::induced_subgraph(
    g,
    vids = largest_component_vertices
  )

  network_diameter <- igraph::diameter(
    g_largest,
    directed = FALSE,
    weights = NA
  )

  average_path_length <- igraph::mean_distance(
    g_largest,
    directed = FALSE,
    weights = NA
  )

  tibble(
    network = network_name,
    nodes = n_nodes,
    edges = n_edges,
    mean_degree = mean_degree,
    max_degree = max_degree,
    density = network_density,
    components = component_info$no,
    largest_component = largest_component_size,
    mean_component_size = mean_component_size,
    diameter = network_diameter,
    average_path_length = average_path_length
  )
}


# ---------------------------
# 6. Overall network statistics
# ---------------------------

overall_statistics <- calculate_network_statistics(
  g_linked,
  network_name = "Selangor"
)

print(overall_statistics)


# ---------------------------
# 7. Prepare district-specific edge lists
# ---------------------------

# District networks are constructed using cases belonging to
# the same district.
#
# Cross-district edges are excluded from district-specific
# descriptive analysis.

district_edges <- edges %>%
  filter(
    source_district == target_district
  ) %>%
  mutate(
    district = source_district
  )


# ---------------------------
# 8. Function for district network
# ---------------------------

analyse_district_network <- function(district_name) {

  district_nodes <- linked_nodes %>%
    filter(district == district_name)

  district_edge_data <- district_edges %>%
    filter(district == district_name)

  if (
    nrow(district_nodes) == 0 ||
    nrow(district_edge_data) == 0
  ) {

    return(
      tibble(
        network = district_name,
        nodes = nrow(district_nodes),
        edges = nrow(district_edge_data),
        mean_degree = NA_real_,
        max_degree = NA_real_,
        density = NA_real_,
        components = NA_integer_,
        largest_component = NA_integer_,
        mean_component_size = NA_real_,
        diameter = NA_real_,
        average_path_length = NA_real_
      )
    )
  }

  g_district <- igraph::graph_from_data_frame(
    d = district_edge_data %>%
      select(
        source,
        target,
        distance_m,
        onset_gap_days
      ),
    vertices = district_nodes,
    directed = FALSE
  )

  calculate_network_statistics(
    g_district,
    network_name = district_name
  )
}


# ---------------------------
# 9. Run analysis for all districts
# ---------------------------

district_statistics <- bind_rows(
  lapply(
    SELANGOR_DISTRICTS,
    analyse_district_network
  )
)

print(district_statistics)


# ---------------------------
# 10. Combine overall and district statistics
# ---------------------------

network_statistics <- bind_rows(
  overall_statistics,
  district_statistics
)


# ---------------------------
# 11. Round values for reporting
# ---------------------------

network_statistics_report <- network_statistics %>%
  mutate(
    mean_degree = round(mean_degree, 2),
    density = signif(density, 4),
    mean_component_size = round(
      mean_component_size,
      2
    ),
    average_path_length = round(
      average_path_length,
      2
    )
  )

print(network_statistics_report)


# ---------------------------
# 12. Degree distribution
# ---------------------------

degree_distribution <- tibble(
  case_id = names(
    igraph::degree(g_linked)
  ),
  degree = as.numeric(
    igraph::degree(g_linked)
  )
) %>%
  count(
    degree,
    name = "frequency"
  ) %>%
  arrange(degree)

print(degree_distribution)


# ---------------------------
# 13. Component membership
# ---------------------------

overall_components <- igraph::components(
  g_linked
)

component_membership <- tibble(
  case_id = names(
    overall_components$membership
  ),
  component_id = as.integer(
    overall_components$membership
  )
)


# ---------------------------
# 14. Component size summary
# ---------------------------

component_size_summary <- tibble(
  component_id = seq_along(
    overall_components$csize
  ),
  component_size = as.integer(
    overall_components$csize
  )
) %>%
  arrange(
    desc(component_size)
  )

print(
  head(
    component_size_summary,
    20
  )
)


# ---------------------------
# 15. Add component ID to node table
# ---------------------------

nodes_with_components <- linked_nodes %>%
  left_join(
    component_membership,
    by = "case_id"
  )


# ---------------------------
# 16. Save outputs
# ---------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  network_statistics_report,
  "output/tables/network_descriptive_statistics.csv"
)

write_csv(
  degree_distribution,
  "output/tables/degree_distribution.csv"
)

write_csv(
  component_size_summary,
  "output/tables/component_size_summary.csv"
)

write_csv(
  nodes_with_components,
  "data/processed/network_nodes_with_components.csv"
)


# ==========================================================
# End of 03_network_descriptive_analysis.R
#
# Next:
# R/04_centrality_analysis.R
# ==========================================================
