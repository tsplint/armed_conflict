### Mulitple Imputation of Armed Conflict Data ###
## Week 8 assignment CHL5233
## Note: GDP was log transformed prior to imputation 

# load packages
library(tidyverse)
library(mice)
library(lme4)
library(knitr)
library(kableExtra)
library(pagedown)


# Load data
final_data <- read_csv(
  "data/processed/final_data.csv",
  show_col_types = FALSE
)

# Log transform GDP
final_data$log_gdp <- log(final_data$gdp_1000)

# convert ISO to numeric and select variables for imputation
midata <- final_data |>
  mutate(iso_num = as.numeric(as.factor(iso))) |>
  select(iso_num, year,
    matmor, un5mor, infmor, neomor,
    armed_conflict_lag1, log_gdp,
    oecd, pop_dens, urban, age_dep,
    male_edu, temp, rainfall_1000,
    earthquake, drought)

# Check and visualize missing data
summary(midata)
colSums(is.na(midata))



### mice initialization dry run

mice0 <- mice(
  midata,
  seed = 100,
  m = 5,
  maxit = 0,
  print= F
)

# Get methods and predictor matrix
meth <- mice0$method
pred <- mice0$predictorMatrix


# Edit meth
meth[c("urban", "male_edu", "temp",
       "rainfall_1000", "log_gdp", "pop_dens",
       "matmor", "infmor", "neomor", "un5mor")] <- "2l.lmer"


# Edit pred

pred[c("urban", "male_edu", "temp",
       "rainfall_1000", "log_gdp", "pop_dens",
       "matmor", "infmor", "neomor", "un5mor"),
     "iso_num"] <- -2

# Check settings
meth
pred



### Run Multiple Imputations  (m=10, max=20)
set.seed(100)

mice.multi.out <- mice(
  midata,
  seed = 100,
  m = 10,
  maxit = 20,
  method = meth,
  predictorMatrix = pred
)

# Check convergence
plot(mice.multi.out)


# save MI 
save(
  mice.multi.out,
  file = "outputs/mice_out.RData"
)


### Fit MI regression models

# Maternal mortality
fit.mi.matmor <- with(
  mice.multi.out,
  lm(matmor ~ -1 + armed_conflict_lag1 +
       log_gdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp +
       rainfall_1000 + earthquake + drought +
       as.factor(iso_num) + as.factor(year))
)

out.mi.matmor <- pool(fit.mi.matmor)


# Under-5 mortality
fit.mi.un5mor <- with(
  mice.multi.out,
  lm(un5mor ~ -1 + armed_conflict_lag1 +
       log_gdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp +
       rainfall_1000 + earthquake + drought +
       as.factor(iso_num) + as.factor(year))
)

out.mi.un5mor <- pool(fit.mi.un5mor)


# Infant mortality
fit.mi.infmor <- with(
  mice.multi.out,
  lm(infmor ~ -1 + armed_conflict_lag1 +
       log_gdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp +
       rainfall_1000 + earthquake + drought +
       as.factor(iso_num) + as.factor(year))
)

out.mi.infmor <- pool(fit.mi.infmor)


# Neonatal mortality
fit.mi.neomor <- with(
  mice.multi.out,
  lm(neomor ~ -1 + armed_conflict_lag1 +
       log_gdp + oecd + pop_dens + urban +
       age_dep + male_edu + temp +
       rainfall_1000 + earthquake + drought +
       as.factor(iso_num) + as.factor(year))
)

out.mi.neomor <- pool(fit.mi.neomor)


# view pooled results

summary(out.mi.matmor, conf.int = TRUE)
summary(out.mi.un5mor, conf.int = TRUE)
summary(out.mi.infmor, conf.int = TRUE)
summary(out.mi.neomor, conf.int = TRUE)


### regression models complete cases for table comparasion


matmormod <- lm(
  matmor ~ -1 + armed_conflict_lag1 +
    log_gdp + oecd + pop_dens + urban +
    age_dep + male_edu + temp +
    rainfall_1000 + earthquake + drought +
    as.factor(iso) + as.factor(year),
  data = final_data,
  na.action = na.omit
)

un5mormod <- lm(
  un5mor ~ -1 + armed_conflict_lag1 +
    log_gdp + oecd + pop_dens + urban +
    age_dep + male_edu + temp +
    rainfall_1000 + earthquake + drought +
    as.factor(iso) + as.factor(year),
  data = final_data,
  na.action = na.omit
)

infmormod <- lm(
  infmor ~ -1 + armed_conflict_lag1 +
    log_gdp + oecd + pop_dens + urban +
    age_dep + male_edu + temp +
    rainfall_1000 + earthquake + drought +
    as.factor(iso) + as.factor(year),
  data = final_data,
  na.action = na.omit
)

neomormod <- lm(
  neomor ~ -1 + armed_conflict_lag1 +
    log_gdp + oecd + pop_dens + urban +
    age_dep + male_edu + temp +
    rainfall_1000 + earthquake + drought +
    as.factor(iso) + as.factor(year),
  data = final_data,
  na.action = na.omit
)

# Check cc sample sizes
nobs(matmormod)
nobs(un5mormod)
nobs(infmormod)
nobs(neomormod)


# Create table to compare 

# Variables to include in final table
keepvars <- c(
  "armed_conflict_lag1" = "Prior Year Armed conflict",
  "log_gdp" = "log(GDP)",
  "oecd" = "OECD",
  "pop_dens" = "Population density",
  "urban" = "Urban",
  "age_dep" = "Age dependency",
  "male_edu" = "Male education",
  "temp" = "Average temperature",
  "rainfall_1000" = "Average rainfall",
  "earthquake" = "Earthquake",
  "drought" = "Drought"
)

# Extract complete-case estimates and 95% CI
get_cc <- function(model, outcome) {

  estimates <- coef(model)
  ci <- confint(model)

  tibble(
    Outcome = outcome,
    Variable = names(estimates),
    Estimate = as.numeric(estimates),
    Lower = ci[, 1],
    Upper = ci[, 2],
    Method = "CC"
  )
}

# Extract pooled MI estimates and 95% CI
get_mi <- function(model, outcome) {

  results <- summary(
    model,
    conf.int = TRUE,
    conf.level = 0.95
  )

  tibble(
    Outcome = outcome,
    Variable = results$term,
    Estimate = results$estimate,
    Lower = results$`2.5 %`,
    Upper = results$`97.5 %`,
    Method = "MI"
  )
}

# Combine CC results
cc_results <- bind_rows(
  get_cc(matmormod, "Maternal mortality"),
  get_cc(un5mormod, "Under-5 mortality"),
  get_cc(infmormod, "Infant mortality"),
  get_cc(neomormod, "Neonatal mortality")
)

# Combine MI results
mi_results <- bind_rows(
  get_mi(out.mi.matmor, "Maternal mortality"),
  get_mi(out.mi.un5mor, "Under-5 mortality"),
  get_mi(out.mi.infmor, "Infant mortality"),
  get_mi(out.mi.neomor, "Neonatal mortality")
)

# Format final table
final_table <- bind_rows(cc_results, mi_results) |>
  filter(Variable %in% names(keepvars)) |>
  mutate(
    Variable = unname(keepvars[Variable]),
    Result = sprintf(
      "%.3f (%.3f, %.3f)",
      Estimate, Lower, Upper
    )
  ) |>
  select(Outcome, Variable, Method, Result) |>
  pivot_wider(
    names_from = Method,
    values_from = Result
  ) |>
  select(Outcome, Variable, CC, MI) |>
  arrange(
    factor(
      Outcome,
      levels = c(
        "Maternal mortality",
        "Under-5 mortality",
        "Infant mortality",
        "Neonatal mortality"
      )
    ),
    match(Variable, unname(keepvars))
  )

# View final table
print(final_table, n = Inf)


### save CC vs MI table
# note: codex gave notes to improve headings in following table

# Save table as HTML 
html_table_cc_vs_mi <- final_table |>
  select(Variable, CC, MI) |>
  knitr::kable(
    format = "html",
    escape = FALSE,
    col.names = c(
      "Variable",
      "Complete case",
      "Multiple imputation"
    ),
    align = c("l", "c", "c"),
    caption = paste(
      "Comparison of complete-case and",
      "multiple-imputation regression results"
    )
  ) |>
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    position = "left",
    font_size = 14
  ) |>
  kableExtra::add_header_above(
    c(" " = 1, "Regression coefficient β (95% CI)" = 2)
  ) |>
  kableExtra::pack_rows(
    index = setNames(
      as.integer(rle(final_table$Outcome)$lengths),
      rle(final_table$Outcome)$values
    ),
    bold = TRUE,
    background = "#E8EEF4"
  ) |>
  kableExtra::footnote(
    general = paste(
      "CI = confidence interval.",
      "Complete-case and pooled multiple-imputation estimates",
      "are presented with 95% confidence intervals.",
      "GDP was log transformed prior to imputation.",
      "Abbreviations: GDP, Gross Domestic Product; OECD, Organisation for Economic Co-operation and Development membership. "
    ),
    general_title = "Note: "
  )

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

kableExtra::save_kable(
  html_table_cc_vs_mi,
  file = "outputs/CC_vs_MI_table.html",
  self_contained = TRUE
)

