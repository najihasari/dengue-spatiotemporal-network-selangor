# ==========================================================
# 06_prepare_statnet_network.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Prepare an undirected statnet network object for ERGM analysis.
#
# Public repository version:
# - Uses cleaned linked-case data
# - Uses anonymised/public-safe identifiers where appropriate
# - Attaches only the attributes required for ERGM
# - Does not contain confidential case-level outputs
#
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load processed data
# ---------------------------

nodes <- readr::read_csv(
  "data/processed/network_nodes_with_components.csv",
  show_col_types = FALSE
) %>%
  dplyr::filter(
    linkage_status == "Linked"
  )

edges <- readr::read_csv(
  "data/processed/spatiotemporal_edges_200m_14d.csv",
  show_col_types = FALSE
)


# ---------------------------
# 3. Retain ERGM variables
# ---------------------------

ergm_nodes <- nodes %>%
  dplyr::select(
    case_id,
    occupation,
    citizenship,
    housing
  )


# ---------------------------
# 4. Create vertex ID list
# ---------------------------

vertex_ids <- ergm_nodes$case_id


# ---------------------------
# 5. Keep valid edges only
# ---------------------------

ergm_edges <- edges %>%
  dplyr::filter(
    source %in% vertex_ids,
    target %in% vertex_ids
  ) %>%
  dplyr::select(
    source,
    target
  )


# ---------------------------
# 6. Canonicalise undirected edges
# ---------------------------

# For an undirected network:
# A-B and B-A represent the same edge.
#
# pmin() / pmax() are used to create a consistent ordering
# before duplicate removal.

ergm_edges <- ergm_edges %>%
  dplyr::mutate(
    node_a = pmin(
      source,
      target
    ),
    node_b = pmax(
      source,
      target
    )
  ) %>%
  dplyr::select(
    source = node_a,
    target = node_b
  ) %>%
  dplyr::distinct()


# ---------------------------
# 7. Remove self-loops
# ---------------------------

ergm_edges <- ergm_edges %>%
  dplyr::filter(
    source != target
  )


# ---------------------------
# 8. Initialise statnet network
# ---------------------------

nw <- network::network.initialize(
  length(vertex_ids),
  directed = FALSE,
  loops = FALSE,
  multiple = FALSE
)


# ---------------------------
# 9. Assign vertex names
# ---------------------------

network::set.vertex.attribute(
  nw,
  "vertex.names",
  vertex_ids
)


# ---------------------------
# 10. Convert edge IDs to vertex positions
# ---------------------------

edge_tail <- match(
  ergm_edges$source,
  vertex_ids
)

edge_head <- match(
  ergm_edges$target,
  vertex_ids
)


# ---------------------------
# 11. Check edge matching
# ---------------------------

if (
  any(is.na(edge_tail)) ||
  any(is.na(edge_head))
) {

  stop(
    "Some edge endpoints could not be matched to vertex IDs."
  )
}


# ---------------------------
# 12. Add edges
# ---------------------------

network::add.edges(
  nw,
  tail = edge_tail,
  head = edge_head
)


# ---------------------------
# 13. Align node attributes
# ---------------------------

node_aligned <- ergm_nodes[
  match(
    vertex_ids,
    ergm_nodes$case_id
  ),
]


# ---------------------------
# 14. Attach occupation
# ---------------------------

network::set.vertex.attribute(
  nw,
  "occupation",
  as.character(
    node_aligned$occupation
  )
)


# ---------------------------
# 15. Attach citizenship
# ---------------------------

network::set.vertex.attribute(
  nw,
  "citizenship",
  as.character(
    node_aligned$citizenship
  )
)


# ---------------------------
# 16. Attach housing
# ---------------------------

network::set.vertex.attribute(
  nw,
  "housing",
  as.character(
    node_aligned$housing
  )
)


# ---------------------------
# 17. Validate network
# ---------------------------

cat(
  "\nStatnet network prepared.\n",
  "Vertices:",
  network::network.size(nw),
  "\n",
  "Edges:",
  network::network.edgecount(nw),
  "\n"
)


# ---------------------------
# 18. Confirm undirected network
# ---------------------------

if (
  network::is.directed(nw)
) {

  stop(
    "Network should be undirected."
  )

} else {

  message(
    "Network correctly specified as undirected."
  )
}


# ---------------------------
# 19. Check ERGM attributes
# ---------------------------

cat(
  "\nOccupation:\n"
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
  "\nCitizenship:\n"
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
  "\nHousing:\n"
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
# 20. Basic network checks
# ---------------------------

if (
  network::network.edgecount(nw) == 0
) {

  stop(
    "Network contains no edges."
  )
}


if (
  network::network.size(nw) == 0
) {

  stop(
    "Network contains no vertices."
  )
}


# ---------------------------
# 21. Save statnet object
# ---------------------------

# This file is intended for local analysis.
# It should remain excluded from the public repository
# if it contains real case-level research data.

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  nw,
  "data/processed/statnet_network_200m_14d.rds"
)


# ==========================================================
# ERGM preparation notes
#
# The statnet network contains:
#
# Structural information:
# - undirected edges
#
# Vertex attributes:
# - occupation
# - citizenship
# - housing
#
# These attributes are used in the final ERGM specification.
#
# The public repository shares the analytical workflow only.
# Original surveillance data are not distributed.
# ==========================================================


# ==========================================================
# End of 06_prepare_statnet_network.R
#
# Next:
# R/07_ergm_analysis.R
# ==========================================================
