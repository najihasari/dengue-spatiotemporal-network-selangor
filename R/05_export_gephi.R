# ==========================================================
# 05_export_gephi.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Prepare node and edge tables for network visualisation in Gephi.
#
# Outputs:
# - Gephi node table
# - Gephi edge table
#
# Notes:
# - The network is undirected.
# - Weight is set to 1 for the primary unweighted network.
# - Residential distance is stored separately as Distance_m.
# - Onset difference is stored separately as Onset_gap_days.
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Load processed network data
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
# 3. Retain linked nodes
# ---------------------------

nodes_gephi <- nodes %>%
  filter(
    linkage_status == "Linked"
  )


# ---------------------------
# 4. Prepare Gephi node table
# ---------------------------

# Gephi requires a unique node identifier.
#
# Longitude and Latitude are included as attributes.
# If geographic layout or GeoLayout is used in Gephi,
# these variables can be mapped to spatial coordinates.

gephi_nodes <- nodes_gephi %>%
  transmute(
    Id = case_id,
    Label = case_id,

    Longitude = longitude,
    Latitude = latitude,

    District = district,
    Mukim = mukim,

    Age = age,
    Sex = sex,
    Occupation = occupation,
    Citizenship = citizenship,
    Housing = housing,

    Degree = degree,
    Component = component_id
  )


# ---------------------------
# 5. Prepare Gephi edge table
# ---------------------------

gephi_edges <- edges %>%
  transmute(
    Source = source,
    Target = target,

    Type = "Undirected",

    Weight = 1,

    Distance_m = round(
      distance_m,
      2
    ),

    Onset_gap_days = onset_gap_days
  )


# ---------------------------
# 6. Validate Gephi node IDs
# ---------------------------

if (anyDuplicated(gephi_nodes$Id) > 0) {

  stop(
    "Duplicate node IDs detected in Gephi node table."
  )

} else {

  message(
    "All Gephi node IDs are unique."
  )
}


# ---------------------------
# 7. Validate edge endpoints
# ---------------------------

missing_source <- setdiff(
  unique(gephi_edges$Source),
  gephi_nodes$Id
)

missing_target <- setdiff(
  unique(gephi_edges$Target),
  gephi_nodes$Id
)

if (
  length(missing_source) > 0 ||
  length(missing_target) > 0
) {

  warning(
    "Some edge endpoints are missing from the Gephi node table."
  )

} else {

  message(
    "All edge endpoints are present in the node table."
  )
}


# ---------------------------
# 8. Check self-loops
# ---------------------------

self_loops <- gephi_edges %>%
  filter(
    Source == Target
  )

if (nrow(self_loops) > 0) {

  warning(
    "Self-loops detected in Gephi edge table."
  )

} else {

  message(
    "No self-loops detected."
  )
}


# ---------------------------
# 9. Check duplicate undirected edges
# ---------------------------

duplicate_gephi_edges <- gephi_edges %>%
  mutate(
    pair_a = pmin(
      Source,
      Target
    ),
    pair_b = pmax(
      Source,
      Target
    )
  ) %>%
  count(
    pair_a,
    pair_b,
    name = "n"
  ) %>%
  filter(
    n > 1
  )

if (nrow(duplicate_gephi_edges) > 0) {

  warning(
    "Duplicate undirected edges detected."
  )

  print(
    duplicate_gephi_edges
  )

} else {

  message(
    "No duplicate undirected edges detected."
  )
}


# ---------------------------
# 10. Create output folder
# ---------------------------

dir.create(
  "output/gephi",
  recursive = TRUE,
  showWarnings = FALSE
)


# ---------------------------
# 11. Export Gephi node table
# ---------------------------

write_csv(
  gephi_nodes,
  "output/gephi/gephi_nodes_200m_14d.csv"
)


# ---------------------------
# 12. Export Gephi edge table
# ---------------------------

write_csv(
  gephi_edges,
  "output/gephi/gephi_edges_200m_14d.csv"
)


# ---------------------------
# 13. Export district-specific files
# ---------------------------

for (
  district_name in SELANGOR_DISTRICTS
) {

  district_nodes <- gephi_nodes %>%
    filter(
      District == district_name
    )

  district_edges <- gephi_edges %>%
    filter(
      Source %in% district_nodes$Id,
      Target %in% district_nodes$Id
    )

  safe_district_name <- district_name %>%
    tolower() %>%
    gsub(
      " ",
      "_",
      .
    )

  write_csv(
    district_nodes,
    paste0(
      "output/gephi/",
      safe_district_name,
      "_nodes.csv"
    )
  )

  write_csv(
    district_edges,
    paste0(
      "output/gephi/",
      safe_district_name,
      "_edges.csv"
    )
  )
}


# ---------------------------
# 14. Gephi import notes
# ---------------------------

cat(
  "\nGephi export completed.\n\n",
  "Import nodes using:\n",
  "  Id = node identifier\n\n",
  "Import edges using:\n",
  "  Source = source node\n",
  "  Target = target node\n",
  "  Type = Undirected\n",
  "  Weight = 1\n\n",
  "Distance_m and Onset_gap_days are edge attributes.\n"
)


# ==========================================================
# Interpretation notes
#
# Weight:
# The primary network is unweighted, therefore every retained
# spatiotemporal linkage has Weight = 1.
#
# Distance_m:
# Actual residential distance between linked cases.
# It is retained as an edge attribute and is NOT automatically
# used as the edge weight.
#
# Onset_gap_days:
# Absolute difference between illness-onset dates of two linked
# dengue cases.
#
# Coordinates:
# Longitude and Latitude are included for geographic or
# spatially informed visualisation.
# ==========================================================


# ==========================================================
# End of 05_export_gephi.R
#
# Next:
# R/06_export_qgis.R
# ==========================================================
