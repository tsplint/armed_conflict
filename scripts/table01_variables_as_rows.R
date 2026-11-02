# Table 1: Country-year characteristics by armed conflict
# Variables as rows (mortality outcomes, all covariates), exposure groups as coloumns (overall, No conflict, Armed conflict)
# all years included in table
# includes Codex's suggested changes to make more visually appealing (see bottom of file for changes made)


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
  matmor = "Maternal mortality (per 100,000 live births)",
  infmor = "Infant mortality (per 1,000 live births)",
  neomor = "Neonatal mortality (per 1,000 live births)",
  un5mor = "Under-five mortality (per 1,000 live births)",
  gdp_1000 = "GDP per capita (US$ thousands)",
  oecd = "OECD membership",
  pop_dens = "Population in high-density areas (%)",
  urban = "Urban residence (%)",
  age_dep = "Age-dependency ratio (per 100 working-age people)",
  male_edu = "Male education (years per capita)",
  temp = "Temperature (°C)",
  rainfall_1000 = "Annual rainfall (1,000 mm/year)",
  earthquake = "Earthquake present",
  drought = "Drought present"
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

# Display the summary type below each variable label.
table_rows <- table_rows |>
  mutate(
    Characteristic = case_when(
      Characteristic %in% unname(variable_labels[binary_vars]) ~
        paste0(Characteristic, "\nn (%)"),
      Characteristic %in% unname(variable_labels) ~
        paste0(Characteristic, "\nMedian (Q1–Q3)"),
      TRUE ~ Characteristic
    )
  )

# View table
print(table_rows, n = Inf, width = Inf)


# Format a publication-style table. Keep summary calculations unchanged.
# Section rows are for presentation only.
section_labels <- c(
  "Mortality outcomes",
  "Socioeconomic and demographic characteristics",
  "Environmental characteristics"
)
section_before <- c(3L, 7L, 13L)
presentation_rows <- map_dfr(seq_len(nrow(table_rows)), function(i) {
  row <- table_rows[i, ]
  if (i %in% section_before) {
    section <- tibble(
      Characteristic = section_labels[match(i, section_before)],
      Overall = "", `No conflict` = "", `Armed conflict` = ""
    )
    bind_rows(section, row)
  } else {
    row
  }
})
section_rows <- which(presentation_rows$Characteristic %in% section_labels)
summary_columns <- c("Overall", "No conflict", "Armed conflict")
navy <- "#20354A"

table_1_rows <- flextable(presentation_rows) |>
  set_header_labels(
    Characteristic = "Characteristic",
    Overall = "Overall",
    `No conflict` = "No conflict",
    `Armed conflict` = "Armed conflict"
  ) |>
  theme_booktabs() |>
  font(fontname = "Arial", part = "all") |>
  fontsize(size = 10, part = "all") |>
  color(color = navy, part = "all") |>
  bg(bg = navy, part = "header") |>
  color(color = "white", part = "header") |>
  bold(part = "header") |>
  bold(i = 1:2, part = "body") |>
  align(j = "Characteristic", align = "left", part = "all") |>
  align(j = summary_columns, align = "center", part = "all") |>
  valign(valign = "center", part = "all") |>
  padding(padding = 5, part = "all") |>
  width(j = "Characteristic", width = 2.8) |>
  width(j = summary_columns, width = 1.4) |>
  set_table_properties(layout = "fixed", width = 1)

# Make summary-type labels smaller and grey beneath the variable names.
for (i in which(grepl("\n", presentation_rows$Characteristic, fixed = TRUE))) {
  label_lines <- strsplit(presentation_rows$Characteristic[i], "\n", fixed = TRUE)[[1]]
  table_1_rows <- compose(
    table_1_rows, i = i, j = "Characteristic",
    value = as_paragraph(
      as_chunk(label_lines[1], props = fp_text(font.size = 10, color = navy)),
      as_chunk("\n"),
      as_chunk(label_lines[2], props = fp_text(font.size = 8, color = navy))
    )
  )
}

# Style the two lines independently, preserving the original values.
for (column in summary_columns) {
  for (i in which(grepl("\nMissing:", presentation_rows[[column]], fixed = TRUE))) {
    cell_lines <- strsplit(presentation_rows[[column]][i], "\n", fixed = TRUE)[[1]]
    table_1_rows <- compose(
      table_1_rows, i = i, j = column,
      value = as_paragraph(
        as_chunk(cell_lines[1], props = fp_text(font.size = 10, color = navy)),
        as_chunk("\n"),
        as_chunk(cell_lines[2], props = fp_text(font.size = 8, color = "#68737D"))
      )
    )
  }
}

for (i in section_rows) {
  table_1_rows <- table_1_rows |>
    merge_at(i = i, j = 1:4, part = "body") |>
    bg(i = i, bg = "#EDF1F5", part = "body") |>
    bold(i = i, part = "body") |>
    align(i = i, align = "left", part = "body") |>
    padding(i = i, padding.top = 7, padding.bottom = 7, part = "body")
}

# Separate statistical definitions from variable descriptions.
table_1_rows <- table_1_rows |>
  add_footer_lines(values = c(
    "Binary summaries count observations coded 1. Missing: n (%) of country-years in each column; binary percentages exclude missing values.",
    "Exposure: armed conflict in the previous year (lagged by one year). All available country-years from 2000–2019 are pooled. Countries may contribute to both exposure groups.",
    "GDP: current US dollars, expressed in thousands. OECD: Organisation for Economic Co-operation and Development membership.",
    "High-density areas: percentage of the population living at >1,000 people/km². Urban residence: percentage living in urban areas. Age dependency: people aged <15 or >64 per 100 people aged 15–64.",
    "Male education: age-standardised years of education per capita. Temperature and rainfall: population-weighted annual means; rainfall is expressed in thousands of millimetres per year. Earthquake and drought: 1 = present, 0 = absent."
  )) |>
  fontsize(size = 8, part = "footer") |>
  color(color = "#59636D", part = "footer") |>
  align(align = "left", part = "footer") |>
  padding(padding.top = 4, padding.bottom = 2, part = "footer")

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
  output = "tables/table01_variables_as_rows.pdf",
  options = list(
    paperWidth = 8.27,
    paperHeight = 11.69,
    marginTop = 0.4,
    marginBottom = 0.4,
    marginLeft = 0.4,
    marginRight = 0.4,
    printBackground = TRUE,
    scale = 0.9
  )
)


## codex's changes - make section headers, improve spacing by decreasing font size and using bold headers,
##make missing data grey not black, shorten labels and add descriptions ot footnotes
## and suggests a navy style header 
