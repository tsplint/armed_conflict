# Create final_data_codex in memory from the seven CSV files in data/raw.
# Run from the armed_conflict project. No datasets are saved or overwritten.
library(dplyr)
library(tidyr)
library(purrr)
library(janitor)
library(here)

raw_dir <- here("data", "raw")
analysis_years <- 2000:2019

# 1. Read every dataset and clean its column names before any joins.
read_clean_csv <- function(filename) {
  read.csv(file.path(raw_dir, filename), stringsAsFactors = FALSE) |>
    clean_names()
}

maternal_raw <- read_clean_csv("maternal_mortality.csv")
infant_raw <- read_clean_csv("infant_mortality.csv")
neonatal_raw <- read_clean_csv("neonatal_mortality.csv")
under5_raw <- read_clean_csv("under5_mortality.csv")
disaster_raw <- read_clean_csv("disaster.csv")
conflict_raw <- read_clean_csv("conflict.csv")
covariates_raw <- read_clean_csv("covariates.csv")

# 2. Reshape each mortality dataset to one row per country-year.
# clean_names() changes X2000 to x2000; names_prefix removes that x.
# Keep missing mortality values as NA, including maternal values in 2018-2019.
# Units: maternal per 100,000 live births; other mortality per 1,000.
prepare_mortality <- function(data, value_name) {
  data |>
    select(iso, matches("^x[0-9]{4}$")) |>
    pivot_longer(
      cols = -iso,
      names_to = "year",
      names_prefix = "x",
      names_transform = list(year = as.integer),
      values_to = value_name
    ) |>
    filter(year %in% analysis_years) |>
    arrange(iso, year)
}

maternal <- prepare_mortality(maternal_raw, "maternal_mortality")
infant <- prepare_mortality(infant_raw, "infant_mortality")
neonatal <- prepare_mortality(neonatal_raw, "neonatal_mortality")
under5 <- prepare_mortality(under5_raw, "under5_mortality")

# 3. Retain only ISO, year and the two requested disaster types.
# The source calls droughts "Drought" (not "Draughts").
disaster_events <- disaster_raw |>
  filter(
    year %in% analysis_years,
    disaster_type %in% c("Drought", "Earthquake")
  ) |>
  select(iso, year, disaster_type)

# Multiple events in a country-year become a single indicator for each type.
# Both indicators can equal 1 in the same year.
disaster_observed <- disaster_events |>
  group_by(iso, year) |>
  summarise(
    drought = as.integer(any(disaster_type == "Drought")),
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    .groups = "drop"
  )

# 4. Sum best separately for each conflict within each country-year.
# Apply the 25-death threshold to each conflict_id, then mark a country-year
# as exposed if at least one conflict qualifies. Separate conflicts with
# 15 deaths each do not qualify merely because their combined total is 30.
# The source contains rows with both conflict_id and best missing. Assume
# these are no-record placeholders and give them zero recorded deaths.
# An incomplete death total stays NA rather than being treated as zero.
conflict_by_id <- conflict_raw |>
  mutate(best = if_else(is.na(conflict_id) & is.na(best), 0, best)) |>
  group_by(iso, year, conflict_id) |>
  summarise(deaths = sum(best), .groups = "drop") |>
  mutate(armed_conflict = as.integer(deaths >= 25))

# Collapse to one row per country-year before shifting the year and merging.
# any() returns TRUE if a conflict qualifies, even if another is unknown;
# otherwise an unknown conflict status remains NA.
conflict_annual <- conflict_by_id |>
  group_by(iso, year) |>
  summarise(
    armed_conflict = as.integer(any(armed_conflict == 1L)),
    .groups = "drop"
  ) |>
  arrange(iso, year)

# Shift AFTER aggregation: conflict in 1999 is joined to outcomes in 2000.
# armed_conflict in the final dataset therefore refers to the PREVIOUS year.
conflict <- conflict_annual |>
  mutate(year = as.integer(year) + 1L) |>
  filter(year %in% analysis_years) |>
  select(iso, year, armed_conflict) |>
  mutate(conflict_record_present = TRUE)

# 5. Prepare covariates.
covariates <- covariates_raw |>
  mutate(year = as.integer(year)) |>
  filter(year %in% analysis_years)

# Check join keys before merging so duplicate keys cannot multiply rows.
check_keys <- function(data, keys) {
  stopifnot(!anyNA(data[keys]), anyDuplicated(data[keys]) == 0L)
  invisible(data)
}

datasets_to_merge <- list(
  maternal, infant, neonatal, under5, covariates, disaster_observed, conflict
)
walk(datasets_to_merge, function(data) check_keys(data, c("iso", "year")))

# 6. Use the mortality datasets to define the country-year merge base.
# Keep every ISO/year pair present in any of the four mortality datasets,
# including rows with missing mortality values. Other sources add columns
# but do not add country-year rows absent from the mortality datasets.
panel_keys <- list(maternal, infant, neonatal, under5) |>
  map(function(data) select(data, iso, year)) |>
  bind_rows() |>
  distinct() |>
  arrange(iso, year)

# Complete the disaster indicators for every country-year in this panel.
# Assumption: no matching event in the disaster file means no recorded event.
disaster <- panel_keys |>
  left_join(disaster_observed, by = c("iso", "year")) |>
  mutate(
    drought = replace_na(drought, 0L),
    earthquake = replace_na(earthquake, 0L)
  )

final_data_codex <- list(
  panel_keys, maternal, infant, neonatal, under5, covariates, disaster, conflict
) |>
  reduce(left_join, by = c("iso", "year")) |>
  mutate(
    # Assumption: absent conflict records mean no recorded conflict deaths.
    # Preserve NA when a record exists but its death count is missing.
    armed_conflict = if_else(
      is.na(conflict_record_present), 0L, armed_conflict
    )
  ) |>
  select(-conflict_record_present) |>
  relocate(iso, year) |>
  arrange(iso, year)

# 7. Verify the finished dataset and show the result without writing files.
check_keys(final_data_codex, c("iso", "year"))
stopifnot(
  nrow(final_data_codex) == nrow(panel_keys),
  all(final_data_codex$year %in% analysis_years),
  all(final_data_codex$drought %in% c(0L, 1L)),
  all(final_data_codex$earthquake %in% c(0L, 1L)),
  all(na.omit(final_data_codex$armed_conflict) %in% c(0L, 1L))
)

print(dim(final_data_codex))
print(head(final_data_codex))
print(table(final_data_codex$armed_conflict, useNA = "ifany"))
print(table(final_data_codex$drought, final_data_codex$earthquake))
# Missing values in mortality/covariates are not filled with zero.
print(colSums(is.na(final_data_codex)))

write.csv(
  final_data_codex,
  here("data", "processed", "final_data_codex.csv"),
  row.names = FALSE
)
