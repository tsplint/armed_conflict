# Create final analytical data set
# CHL5233 - Armed Conflict Dataset Week 3. October 5, 2026
# code without codex use
## Tallinn Splinter 

# add packages
library(tidyverse)
library(janitor)
library(countrycode)
library(here)
library(knitr)
library(flextable)
library(sjPlot)


# --------------------------------------
# Prepare world bank mortality data #
#---------------------------------------
matmor0 <- read.csv(here( "data", "raw", "maternal_mortality.csv"), header = TRUE)
infmor0 <- read.csv(here( "data", "raw", "infant_mortality.csv"), header = TRUE)
neomor0 <- read.csv(here("data", "raw", "neonatal_mortality.csv"), header = TRUE)
un5mor0 <- read.csv(here( "data", "raw", "under5_mortality.csv"), header = TRUE)

# check names and if need to convert country code to iso
names(matmor0)
head(matmor0$iso)
names(infmor0)
names(neomor0)
names(un5mor0)

dim(matmor0)
dim(infmor0)
dim(neomor0)
dim(un5mor0)

## function to prepare data - upadate given function
wbfun <- function(dataname, varname) {
  dataname |>
    dplyr::select(iso, X2000:X2019) |>
    pivot_longer(
      cols = starts_with("X"),
      names_to = "year",
      names_prefix = "X",
      values_to = varname
    ) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year)
}

#apply to all variables
matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

#put all data frames into list
wblist <- list(matmor, infmor, neomor, un5mor)

#merge all data frames in list
wblist |> reduce(full_join, by = c('iso', 'year')) -> wbdata

# --------------------------------------
# Prepare Disaster data #
#---------------------------------------
disaster0 <- read.csv(
  here("data", "raw", "disaster.csv"),
  header = TRUE
)
# inspect the data
dim(disaster0)
names(disaster)
head(disaster0)
# clean names and subset years to 2000-2019 with 2 disasters Earthquake and Draught
disaster <- disaster0 |>
  clean_names()

disaster <- disaster |>
  filter(
    year >= 2000,
    year <= 2019,
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type)

head(disaster)
dim(disaster)

# create dummy variable
disaster <- disaster |>
  mutate(
    drought = ifelse(disaster_type == "Drought", 1, 0),
    earthquake = ifelse(disaster_type == "Earthquake", 1, 0)
  )
head(disaster)

# collapse
disaster <- disaster |>
  group_by(year, iso) |>
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  )

#check 
head(disaster)
dim(disaster)



# --------------------------------------------------
# Prepare conflict data
# --------------------------------------------------

conflict0 <- read.csv(
  here("data", "raw", "conflict.csv"),
  header = TRUE
)
dim(conflict0)
names(conflict0)
head(conflict0)

# combine years so no mulitple years
conflict <- conflict0 |>
  group_by(iso, year, conflict_id) |>
  summarise(
    deaths = sum(best, na.rm = TRUE),
    .groups = "drop"
  )
head(conflict)
dim(conflict)

# create binary armed conflict variable based off of > or equal to 25 death cutoff from paper
conflict <- conflict |>
  mutate(
    armed_conflict = ifelse(deaths >= 25, 1, 0)
  ) |>
  group_by(iso, year) |>
  summarise(
    armed_conflict = max(armed_conflict),
    .groups = "drop"
  )
head(conflict)
dim(conflict)
table(conflict$armed_conflict)

# lag data by one year (like paper) and change years
conflict <- conflict |>
  mutate(
    year = year + 1
  ) |>
  filter(year >= 2000, year <= 2019) |>
  rename(
    armed_conflict_lag1 = armed_conflict
  )


#check
head(conflict)
dim(conflict)
table(conflict$armed_conflict_lag1, useNA = "ifany")

# add covariates csv
covariates <- read.csv(
  here("data", "raw", "covariates.csv"),
  header = TRUE
)

dim(covariates)
names(covariates)
head(covariates)

# Merge all data sets
final_data <- wbdata |>
  left_join(covariates, by = c("iso", "year")) |>
  left_join(disaster, by = c("iso", "year")) |>
  left_join(conflict, by = c("iso", "year"))

# check
dim(final_data)
names(final_data)
head(final_data)
# has NA's -- check variables 
table(final_data$earthquake, useNA = "ifany")
table(final_data$drought, useNA = "ifany")

#remove NAs

final_data <- final_data |>
  mutate(
    earthquake = replace_na(earthquake, 0),
    drought = replace_na(drought, 0)
  )
# check NAs are changed to 0
table(final_data$earthquake, useNA = "ifany")
table(final_data$drought, useNA = "ifany")

head(final_data)

# --------------------------------------
# Create CSV of final data file
#---------------------------------------


write.csv(
  final_data,
  here("data", "processed", "final_data.csv"),
  row.names = FALSE
)
# check file exists
file.exists(here("data", "processed", "final_data.csv"))
