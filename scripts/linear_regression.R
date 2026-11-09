## Linear Regression in class assignment 
library(plm)
library(texreg)
library(tidyverse)

#load data
final_data <- read_csv(
  "data/processed/final_data.csv",
  show_col_types = FALSE
)


#Log transform GDP
final_data$log_gdp <- log(final_data$gdp_1000)


## four moratlity models 
# Define predictors
preds <- as.formula(
  "~ armed_conflict_lag1 + log_gdp + oecd + pop_dens +
  urban + age_dep + male_edu + temp +
  rainfall_1000 + earthquake + drought"
)

# Define outcomes
outcomes <- c(
  "matmor",
  "un5mor",
  "infmor",
  "neomor"
)

# Fit four models with country and year fixed effects
models <- lapply(outcomes, function(y) {

  formula <- update.formula(
    preds,
    as.formula(paste(y, "~ ."))
  )

  plm(
    formula,
    data = final_data,
    index = c("iso", "year"),
    effect = "twoways",
    model = "within",
  )
})
# Name models
names(models) <- outcomes

# View results
lapply(models, summary)


## create regression table with 95% CI using texreg


# Define variable names for table
keepvars <- list(
  "armed_conflict_lag1" = "Armed conflict",
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

# Display table in R console with 95% confidence intervals
screenreg(
  models,
  ci.force = TRUE,
  custom.coef.map = keepvars,
  custom.model.names = c(
    "Maternal mortality",
    "Under-5 mortality",
    "Infant mortality",
    "Neonatal mortality"
  )
)

# Export regression table to HTML
htmlreg(
  models,
  ci.force = TRUE,
  custom.coef.map = keepvars,
  custom.model.names = c(
    "Maternal mortality",
    "Under-5 mortality",
    "Infant mortality",
    "Neonatal mortality"
  ),
  file = "outputs/mortality_results.html"
)
