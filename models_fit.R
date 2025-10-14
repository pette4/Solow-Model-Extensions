# ------------------------------------------------------------------------
# Script: Regression Analysis with Diagnostics and Robustness Checks
#
# Purpose:
#   This script analyzes the relationship between log-transformed GDP per capita (lGDP)
#   and two predictors: log-transformed saving rate (lsaving) and log-transformed 
#   population growth (lpop). The analysis is run across various specifications:
#
#     1. Period Combinations:
#        - "2023_2023": Both dependent and independent variables use 2023 data.
#        - "2000-2023_2000-2023": Both are based on averages over 2000–2023.
#        - "2000-2023_2023": Independent variables are 2000–2023 averages, while the 
#          dependent variable is from 2023.
#
#     2. Model Constraints:
#        - No constraint: Standard OLS regression.
#        - With constraint: A reparameterized model that forces the coefficient of saving
#          rate to be equal in magnitude but opposite in sign to that of population growth.
#
# Once we determine the appropriate timeframe to run regressions we run across different country groups:
#        - All countries
#        - Low-resource rents countries (high_resources == 0)
#        - OECD countries (OECD == 1)
#
#   The script also conducts heteroskedasticity tests,
#   checks for multicollinearity, and tests the imposed constraint.
#
# Requirements:
#   - data.table, lmtest, car packages.
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Section 1: Load required packages and data
# ------------------------------------------------------------------------
library(data.table)
library(lmtest)   # For the Breusch-Pagan test
library(car)      # For VIF and linear hypothesis tests

dt <- fread("aggregated_data.csv")

# ------------------------------------------------------------------------
# Section 2: Define a function to prepare variables and log transformations by period
# ------------------------------------------------------------------------
assign_vars_period <- function(dt, x_period, y_period) {
  # Select the dependent variable (GDP per capita) based on y_period
  if (y_period == "2023") {
    GDP <- dt$last_gdp_per_capita
  } else if (y_period == "2000-2023") {
    GDP <- dt$avg_gdp_per_capita
  } else {
    stop("Invalid y_period; use '2023' or '2000-2023'")
  }
  
  # Select the independent variables (saving rate and population growth) based on x_period
  if (x_period == "2023") {
    saving <- dt$last_saving_rate
    pop <- dt$last_pop_growth
  } else if (x_period == "2000-2023") {
    saving <- dt$avg_saving_rate
    pop <- dt$avg_pop_growth

  } else {
    stop("Invalid x_period; use '2023' or '2000-2023'")
  }
  
  # Compute log-transformed variables, using sign() to handle potential negative values
  lGDP <- log(GDP)
  
  # IMPORTANT: use this to select saving rate
  lsaving = sign(saving) * log(abs(saving))
  lpop = log(pop + 5)

  return(data.frame(lGDP = lGDP, lsaving = lsaving, lpop = lpop))
}

# ------------------------------------------------------------------------
# Section 3: Define period combinations for regression specifications
# ------------------------------------------------------------------------
period_combinations <- list(
  "2023_2023" = list(x = "2023", y = "2023"),
  "2000-2023_2000-2023" = list(x = "2000-2023", y = "2000-2023"),
  "2000-2023_2023" = list(x = "2000-2023", y = "2023")
)

# ------------------------------------------------------------------------
# Section 4: Define a function to run regression and diagnostics for a given specification
# ------------------------------------------------------------------------
run_regression <- function(dt, x_period, y_period, constraint) {
  # Prepare the data by applying the log transformations for the chosen periods
  vars_df <- assign_vars_period(dt, x_period, y_period)
  
  # Select model formulation based on the constraint option
  if (constraint == "no_constraint") {
    model <- lm(lGDP ~ lsaving + lpop, data = vars_df)
  } else if (constraint == "with_constraint") {
    model <- lm(lGDP ~ I(lsaving - lpop), data = vars_df)
    #vif(model)
  } else {
    stop("Invalid constraint option; choose 'no_constraint' or 'with_constraint'")
  }
  
  # Create a unique model name for identification
  model_name <- paste("model", paste0(x_period, "_", y_period), constraint, sep = "_")
  cat("-----\nModel:", model_name, "\n")
  
  # Print the summary of the regression model
  print(summary(model))
  
  # Conduct a Breusch-Pagan test for heteroskedasticity
  bp_test <- bptest(model)
  cat("Breusch-Pagan Test:\n")
  print(bp_test)
  
  # For unconstrained models, perform additional diagnostics
  if (constraint == "no_constraint") {
    
    # Test the constraint formally: H0: lsaving + lpop = 0
    lh_test <- linearHypothesis(model, "lsaving + lpop = 0")
    cat("Test for constraint (lsaving + lpop = 0):\n")
    print(lh_test)
  }
  
  return(model)
}


# ------------------------------------------------------------------------
# Section 5: Loop over all combinations of period specifications, and constraints
# ------------------------------------------------------------------------
results <- list()

for (period in names(period_combinations)) {
  x_period <- period_combinations[[period]]$x
  y_period <- period_combinations[[period]]$y
  
  for (constraint in c("no_constraint", "with_constraint")) {
    # Run the regression with diagnostics for the current specification
    mod <- run_regression(dt, x_period, y_period, constraint)
    
    # Store the resulting model in a list for later reference
    model_key <- paste("model", period, constraint, sep = "_")
    results[[model_key]] <- mod
    
    # Optional: Pause between models for inspection (uncomment if desired)
    readline(prompt = "Press [enter] to continue to the next model")
  }
}


# ------------------------------------------------------------------------
# Section 6: Define country group subsets based on provided flags
# ------------------------------------------------------------------------
data_all            <- dt
data_high_resources <- dt[high_resources == 0]
data_OECD           <- dt[OECD == 1]

country_groups <- list(
  all = data_all,
  high_resources = data_high_resources,
  OECD = data_OECD
)


# ------------------------------------------------------------------------
# Section 7: Loop over all combinations of country groups and constraints (2000–2023 averages only)
# ------------------------------------------------------------------------
run_countries_regression <- function(dt, group_name, constraint) {
  # Extract log-transformed core variables
  GDP     <- dt$last_gdp_per_capita
  saving  <- dt$avg_saving_rate
  pop     <- dt$avg_pop_growth
  
  lGDP    <- log(GDP)
  lsaving = sign(saving) * log(abs(saving))
  lpop = log(pop + 5)
  vars_df <- data.frame(lGDP = lGDP, lsaving = lsaving, lpop = lpop)
  
  if (constraint == "no_constraint") {
    model <- lm(lGDP ~ lsaving + lpop, data = vars_df)
  } else if (constraint == "with_constraint") {
    model <- lm(lGDP ~ I(lsaving - lpop), data = vars_df)
  } else {
    stop("Invalid constraint option; choose 'no_constraint' or 'with_constraint'")
  }
  
  # Create a model name
  model_name <- paste("model", group_name, "2000-2023", constraint, sep = "_")
  cat("-----\nModel:", model_name, "\n")
  
  # Print results
  print(summary(model))
  
  cat("Breusch-Pagan Test:\n")
  print(bptest(model))
  
  if (constraint == "no_constraint") {
    cat("Variance Inflation Factors:\n")
    print(vif(model))
    
    cat("Test for constraint (lsaving + lpop = 0):\n")
    lh_test <- linearHypothesis(model, "lsaving + lpop = 0")
    print(lh_test)
  }
  
  return(model)
}

results <- list()

for (group in names(country_groups)) {
  dt_group <- country_groups[[group]]
    
  for (constraint in c("no_constraint", "with_constraint")) {
    # Run the regression with diagnostics for the current specification
    mod <- run_countries_regression(dt_group, group, constraint)
    
    # Store the resulting model in a list for later reference
    model_key <- paste("model", group, constraint, sep = "_")
    results[[model_key]] <- mod
    
    # Optional: Pause between models for inspection (uncomment if desired)
    readline(prompt = "Press [enter] to continue to the next model")
  }
}


# ------------------------------------------------------------------------
# Section 8: Regressions Including Trade (2000–2023 averages only)
# ------------------------------------------------------------------------
run_trade_regression <- function(dt, group_name, constraint) {
  # Extract log-transformed core variables
  GDP     <- dt$last_gdp_per_capita
  saving  <- dt$avg_saving_rate
  pop     <- dt$avg_pop_growth
  trade    <- dt$avg_trade
  
  lGDP    <- log(GDP)
  lsaving = sign(saving) * log(abs(saving))
  lpop = log(pop + 5)
  ltrade   <- log(trade) 
  vars_df <- data.frame(lGDP = lGDP, lsaving = lsaving, lpop = lpop, ltrade = ltrade)
  
  if (constraint == "no_constraint") {
    model <- lm(lGDP ~ lsaving + lpop + ltrade, data = vars_df)
  } else if (constraint == "with_constraint") {
    model <- lm(lGDP ~ I(lsaving - lpop) + ltrade, data = vars_df)
  } else {
    stop("Invalid constraint option; choose 'no_constraint' or 'with_constraint'")
  }
  
  # Create a model name
  model_name <- paste("model", group_name, "2000-2023_trade", constraint, sep = "_")
  cat("-----\nModel:", model_name, "\n")
  
  # Print results
  print(summary(model))
  
  cat("Breusch-Pagan Test:\n")
  print(bptest(model))
  
  if (constraint == "no_constraint") {
    cat("Variance Inflation Factors:\n")
    print(vif(model))
    
    cat("Test for constraint (lsaving + lpop = 0):\n")
    lh_test <- linearHypothesis(model, "lsaving + lpop = 0")
    print(lh_test)
  }
  else {
    lh_test = linearHypothesis(model, "I(lsaving - lpop) = 0.5")
    print(lh_test)
    
  }
  
  return(model)
}

# Run regressions with trade across country groups
for (group in names(country_groups)) {
  dt_group <- country_groups[[group]]
  
  for (constraint in c("no_constraint", "with_constraint")) {
    model <- run_trade_regression(dt_group, group, constraint)
    
    model_key <- paste("model", group, "2000-2023_trade", constraint, sep = "_")
    results[[model_key]] <- model
    
    readline(prompt = "Press [enter] to continue to the next education model")
  }
}

