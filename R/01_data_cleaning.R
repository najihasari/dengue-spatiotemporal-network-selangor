# ==========================================================
# 01_data_cleaning.R
# Spatiotemporal Dengue Network Analysis - Selangor, 2023
# ==========================================================
#
# Purpose:
# Prepare the case-level dengue surveillance dataset for
# spatiotemporal network construction.
#
# Main tasks:
# 1. Import data
# 2. Standardise variable names
# 3. Format illness-onset dates
# 4. Recode analytical variables
# 5. Check missing values and duplicates
# 6. Validate geographic coordinates
# 7. Restrict analysis to Selangor districts
# 8. Export a clean analytical dataset
#
# IMPORTANT:
# The original confidential surveillance dataset should NOT
# be uploaded to a public GitHub repository.
# ==========================================================


# ---------------------------
# 1. Load setup
# ---------------------------

source("R/00_setup.R")


# ---------------------------
# 2. Import data
# ---------------------------

# Replace the example file below with the local path to the
# confidential surveillance dataset when running the analysis.
#
# DO NOT commit the original dataset to GitHub.

raw_data <- read_csv(
  "data/raw/dengue_selangor_2023.csv",
  show_col_types = FALSE
)


# ---------------------------
# 3. Inspect imported data
# ---------------------------

glimpse(raw_data)

cat("\nNumber of rows:", nrow(raw_data), "\n")
cat("Number of columns:", ncol(raw_data), "\n")


# ---------------------------
# 4. Standardise variable names
# ---------------------------

# Adjust the variables on the right-hand side to match the
# original column names in the surveillance dataset.
#
# The analytical workflow uses standardised English variable
# names to improve readability and reproducibility.

data <- raw_data %>%
  transmute(
    case_id      = as.character(Notifikasi_No),
    onset_date   = Tarikh_Onset,
    latitude     = as.numeric(Lat),
    longitude    = as.numeric(Lon),
    district     = as.character(Daerah),
    mukim        = as.character(Mukim),
    age          = as.numeric(Umur),
    sex          = as.character(Jantina),
    occupation   = as.character(Kategori_Pekerjaan),
    citizenship  = as.character(Kewarganegaraan),
    housing      = as.character(Jenis_Rumah_2)
  )


# ---------------------------
# 5. Format illness-onset date
# ---------------------------

# Modify the date parsing if the source file uses a different
# date format.

data <- data %>%
  mutate(
    onset_date = as.Date(onset_date)
  )


# ---------------------------
# 6. Restrict study period
# ---------------------------

data <- data %>%
  filter(
    onset_date >= as.Date("2023-01-01"),
    onset_date <= as.Date("2023-12-31")
  )


# ---------------------------
# 7. Clean text variables
# ---------------------------

data <- data %>%
  mutate(
    district = trimws(district),
    mukim = trimws(mukim),
    sex = trimws(sex),
    occupation = trimws(occupation),
    citizenship = trimws(citizenship),
    housing = trimws(housing)
  )


# ---------------------------
# 8. Standardise district names
# ---------------------------

# District names should match the nine districts included in
# the Selangor analysis.

data <- data %>%
  mutate(
    district = case_when(
      district %in% c("Petaling") ~ "Petaling",
      district %in% c("Hulu Langat", "Ulu Langat") ~ "Hulu Langat",
      district %in% c("Klang") ~ "Klang",
      district %in% c("Gombak") ~ "Gombak",
      district %in% c("Sepang") ~ "Sepang",
      district %in% c("Hulu Selangor", "Ulu Selangor") ~ "Hulu Selangor",
      district %in% c("Kuala Langat") ~ "Kuala Langat",
      district %in% c("Kuala Selangor") ~ "Kuala Selangor",
      district %in% c("Sabak Bernam") ~ "Sabak Bernam",
      TRUE ~ district
    )
  )

data <- data %>%
  filter(district %in% SELANGOR_DISTRICTS)


# ---------------------------
# 9. Recode sex
# ---------------------------

data <- data %>%
  mutate(
    sex = case_when(
      tolower(sex) %in% c("male", "lelaki", "m") ~ "Male",
      tolower(sex) %in% c("female", "perempuan", "f") ~ "Female",
      TRUE ~ NA_character_
    )
  )


# ---------------------------
# 10. Recode occupation
# ---------------------------

# Final analytical categories:
# - Employed
# - Student
# - Not working
#
# Edit source labels below if the original dataset uses
# additional or different wording.

data <- data %>%
  mutate(
    occupation = case_when(
      tolower(occupation) %in% c(
        "employed",
        "working",
        "bekerja",
        "pekerja"
      ) ~ "Employed",

      tolower(occupation) %in% c(
        "student",
        "pelajar",
        "murid"
      ) ~ "Student",

      tolower(occupation) %in% c(
        "not working",
        "tidak bekerja",
        "unemployed",
        "retired",
        "pesara",
        "housewife",
        "suri rumah"
      ) ~ "Not working",

      TRUE ~ occupation
    )
  )


# ---------------------------
# 11. Recode citizenship
# ---------------------------

# Final analytical categories:
# - Citizen
# - Non-citizen

data <- data %>%
  mutate(
    citizenship = case_when(
      tolower(citizenship) %in% c(
        "citizen",
        "malaysian",
        "warganegara",
        "warganegara malaysia"
      ) ~ "Citizen",

      tolower(citizenship) %in% c(
        "non-citizen",
        "non citizen",
        "non-malaysian",
        "bukan warganegara",
        "bukan warganegara malaysia"
      ) ~ "Non-citizen",

      TRUE ~ citizenship
    )
  )


# ---------------------------
# 12. Recode housing type
# ---------------------------

# Final analytical categories:
# - High-rise: >= 5 floors
# - Low-rise: < 5 floors
#
# If the original variable is already coded as High-rise /
# Low-rise, this step simply standardises the labels.

data <- data %>%
  mutate(
    housing = case_when(
      tolower(housing) %in% c(
        "high-rise",
        "high rise",
        "highrise",
        "bertingkat tinggi"
      ) ~ "High-rise",

      tolower(housing) %in% c(
        "low-rise",
        "low rise",
        "lowrise",
        "bertingkat rendah"
      ) ~ "Low-rise",

      TRUE ~ housing
    )
  )


# ---------------------------
# 13. Convert analytical variables to factors
# ---------------------------

data <- data %>%
  mutate(
    sex = factor(
      sex,
      levels = c("Male", "Female")
    ),

    occupation = factor(
      occupation,
      levels = c(
        "Employed",
        "Student",
        "Not working"
      )
    ),

    citizenship = factor(
      citizenship,
      levels = c(
        "Citizen",
        "Non-citizen"
      )
    ),

    housing = factor(
      housing,
      levels = c(
        "Low-rise",
        "High-rise"
      )
    ),

    district = factor(
      district,
      levels = SELANGOR_DISTRICTS
    )
  )


# ---------------------------
# 14. Check duplicate case identifiers
# ---------------------------

duplicate_cases <- data %>%
  count(case_id, name = "n") %>%
  filter(n > 1)

if (nrow(duplicate_cases) > 0) {

  message(
    "Warning: duplicate case identifiers detected. ",
    "Review before network construction."
  )

  print(duplicate_cases)

} else {

  message("No duplicate case identifiers detected.")
}


# ---------------------------
# 15. Check missing values
# ---------------------------

missing_summary <- data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_n"
  ) %>%
  arrange(desc(missing_n))

print(missing_summary)


# ---------------------------
# 16. Validate coordinates
# ---------------------------

# Basic geographic-range checks.
#
# These broad limits are intended only to detect obvious
# coordinate-entry errors. They are not used to define the
# study boundary.

data <- data %>%
  mutate(
    coordinate_valid = case_when(
      is.na(latitude) | is.na(longitude) ~ FALSE,
      latitude < -90 | latitude > 90 ~ FALSE,
      longitude < -180 | longitude > 180 ~ FALSE,
      TRUE ~ TRUE
    )
  )

invalid_coordinates <- data %>%
  filter(!coordinate_valid)

cat(
  "\nCases with missing or invalid coordinates:",
  nrow(invalid_coordinates),
  "\n"
)


# ---------------------------
# 17. Check age
# ---------------------------

# Flag implausible values for manual review rather than
# automatically deleting them.

data <- data %>%
  mutate(
    age_valid = case_when(
      is.na(age) ~ FALSE,
      age < 0 ~ FALSE,
      age > 110 ~ FALSE,
      TRUE ~ TRUE
    )
  )

cat(
  "Cases with missing or implausible age:",
  sum(!data$age_valid),
  "\n"
)


# ---------------------------
# 18. Create analysis-ready dataset
# ---------------------------

# The spatial network requires:
# - unique case identifier
# - onset date
# - valid residential coordinates
#
# Other attributes may remain missing depending on the
# specific descriptive or ERGM analysis.

data_clean <- data %>%
  filter(
    !is.na(case_id),
    !is.na(onset_date),
    coordinate_valid
  ) %>%
  select(
    case_id,
    onset_date,
    latitude,
    longitude,
    district,
    mukim,
    age,
    sex,
    occupation,
    citizenship,
    housing
  )


# ---------------------------
# 19. Final validation
# ---------------------------

check_required_columns(data_clean)

cat(
  "\nFinal analysis-ready cases:",
  nrow(data_clean),
  "\n"
)

cat(
  "Unique case identifiers:",
  n_distinct(data_clean$case_id),
  "\n"
)


# ---------------------------
# 20. Summary by district
# ---------------------------

district_summary <- data_clean %>%
  count(district, name = "n_cases") %>%
  arrange(desc(n_cases))

print(district_summary)


# ---------------------------
# 21. Save cleaned dataset
# ---------------------------

# IMPORTANT:
# If this repository is public, ensure that this file is
# excluded using .gitignore because it contains confidential
# case-level coordinates.

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  data_clean,
  "data/processed/dengue_selangor_2023_clean.csv"
)


# ---------------------------
# 22. Save non-sensitive QC summary
# ---------------------------

dir.create(
  "output/tables",
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  missing_summary,
  "output/tables/data_cleaning_missing_summary.csv"
)

write_csv(
  district_summary,
  "output/tables/data_cleaning_district_summary.csv"
)


# ==========================================================
# End of 01_data_cleaning.R
# Next step:
# R/02_construct_spatiotemporal_links.R
# ==========================================================
