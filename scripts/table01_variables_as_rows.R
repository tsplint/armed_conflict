# Table 1: Country-year characteristics by armed conflict
# Variables as rows (mortality outcomes, all covariates), exposure groups as coloumns (overall, No conflict, Armed conflict)
# all years included in table


library(tidyverse)
library(flextable)
library(officer)
library(pagedown)

# load data 

final_data <- read_csv(
  "data/processed/final_data.csv",
  show_col_types = FALSE
)

# define variables (mortality and covariates)

variable_labels <- c(
  matmor = "Maternal mortality",
  infmor = "Infant mortality",
  neomor = "Neonatal mortality",
  un5mor = "Under-five mortality",
  gdp_1000 = "GDP per capita (thousands)",
  oecd= "Organisation for Economic Co-operation and Development",
  pop_dens= "Population density",
  urban = "Urban residence",
  age_dep= "Age-dependency ratio",
  male_edu="Male education",
  temp="Temperature",
  rainfall_1000= "Rainfall",
  earthquake = "Earthquake",
  drought = "Drought"
)

binary_vars <- c("oecd", "earthquake", "drought")

# Create conflict groups

table_data <- final_data |>
  mutate(
    conflict_group = case_when(
      armed_conflict_lag1 == 0 ~ "No conflict",
      armed_conflict_lag1 == 1 ~ "Armed conflict",
      TRUE ~ NA_character_
    )
  )

# Summary functions

# Median (IQR)
summarise_continuous <- function(x) {
  x <- x[!is.na(x)]

  if (length(x) == 0) return("NA")

  sprintf(
    "%.2f (%.2f–%.2f)",
    median(x),
    quantile(x, 0.25),
    quantile(x, 0.75)
  )
}

# n (%) for binary variables coded 1
summarise_binary <- function(x) {
  n_valid <- sum(!is.na(x))
  n_yes <- sum(x == 1, na.rm = TRUE)

  if (n_valid == 0) return("NA")

  sprintf(
    "%d (%.1f%%)",
    n_yes,
    100 * n_yes / n_valid
  )
}

# Missing n (%)
summarise_missing <- function(x) {
  sprintf(
    "%d (%.1f%%)",
    sum(is.na(x)),
    100 * mean(is.na(x))
  )
}

# summarise data

make_summary <- function(data, group_name) {

  map_dfr(names(variable_labels), function(var) {

    x <- data[[var]]

    tibble(
      Group = group_name,
      Variable = variable_labels[[var]],
      Summary = if (var %in% binary_vars) {
        summarise_binary(x)
      } else {
        summarise_continuous(x)
      },
      Missing = summarise_missing(x)
    )
  })
}

summary_long <- bind_rows(
  make_summary(table_data, "Overall"),
  make_summary(
    filter(table_data, conflict_group == "No conflict"),
    "No conflict"
  ),
  make_summary(
    filter(table_data, conflict_group == "Armed conflict"),
    "Armed conflict"
  )
)

# group sizes

group_sizes <- table_data |>
  filter(!is.na(conflict_group)) |>
  group_by(conflict_group) |>
  summarise(
    N = n(),
    Countries = n_distinct(iso),
    .groups = "drop"
  ) |>
  rename(Group = conflict_group) |>
  bind_rows(
    tibble(
      Group = "Overall",
      N = nrow(table_data),
      Countries = n_distinct(table_data$iso)
    )
  ) |>
  mutate(
    Group = factor(
      Group,
      levels = c("Overall", "No conflict", "Armed conflict")
    )
  ) |>
  arrange(Group) |>
  mutate(Group = as.character(Group))

print(group_sizes)

# table with variables as rows
table_rows <- summary_long |>
  mutate(
    Value = paste0(
      Summary,
      "\nMissing: ",
      Missing
    ),
    Group = factor(
      Group,
      levels = c("Overall", "No conflict", "Armed conflict")
    ),
    Variable = factor(
      Variable,
      levels = unname(variable_labels)
    )
  ) |>
  select(Variable, Group, Value) |>
  pivot_wider(
    names_from = Group,
    values_from = Value
  ) |>
  arrange(Variable) |>
  mutate(
    Characteristic = as.character(Variable)
  ) |>
  select(
    Characteristic,
    Overall,
    `No conflict`,
    `Armed conflict`
  )

# Add country-year sample sizes
n_row <- tibble(
  Characteristic = "Country-years, N",
  Overall = format(group_sizes$N[group_sizes$Group == "Overall"],
                   big.mark = ","),
  `No conflict` = format(group_sizes$N[group_sizes$Group == "No conflict"],
                         big.mark = ","),
  `Armed conflict` = format(group_sizes$N[group_sizes$Group == "Armed conflict"],
                           big.mark = ",")
)

# Add unique country numbers
countries_row <- tibble(
  Characteristic = "Unique countries, N",
  Overall = as.character(
    group_sizes$Countries[group_sizes$Group == "Overall"]
  ),
  `No conflict` = as.character(
    group_sizes$Countries[group_sizes$Group == "No conflict"]
  ),
  `Armed conflict` = as.character(
    group_sizes$Countries[group_sizes$Group == "Armed conflict"]
  )
)

# Combine sample sizes and descriptive characteristics
table_rows <- bind_rows(
  n_row,
  countries_row,
  table_rows
)

# View table
print(table_rows, n = Inf, width = Inf)


# Format into table with flextable package

table_1_rows <- flextable(table_rows) |>
  set_header_labels(
    Characteristic = "Variable",
    Overall = "Overall",
    `No conflict` = "No conflict",
    `Armed conflict` = "Armed conflict"
  ) |>
  theme_booktabs() |>
  fontsize(size = 9, part = "all") |>
  bold(part = "header") |>
  bold(i = 1:2, part = "body") |>
  align(
    j = c("Overall", "No conflict", "Armed conflict"),
    align = "center",
    part = "all"
  ) |>
  valign(valign = "center", part = "all") |>
  autofit()

# Add table description/footnote
table_1_rows <- add_footer_lines(
  table_1_rows,
  values = paste(
    "Continuous variables: Median (IQR).",
    "Binary variables: n (%)",
    "Missing data: n (%).",
    "Percentages for observed binary variables exclude missing values.",
    "Conflict exposure refers to the previous year.",
    "Note: countries may contribute to both exposure groups."
  )
)

# View table
table_1_rows


#### Export to pdf ####

dir.create("tables", showWarnings = FALSE)



#PDF set title of table
table_pdf <- set_caption(
  table_1_rows,
  caption = paste(
    "Table 1. Country-year characteristics by",
    "prior-year armed conflict exposure, 2000–2019"
  )
)

# Save as HTML
flextable::save_as_html(
  table_pdf,
  path = "tables/table01_variables_as_rows.html"
)

# Convert HTML to PDF
pagedown::chrome_print(
  input = "tables/table01_variables_as_rows.html",
  output = "tables/table01_variables_as_rows.pdf"
)
