### CHOW TEST FOR GROUP SPLIT ###

# this script tests whether the difference between running the regression on all countries
# or on the selected groups with "high resources rents" and "OECD" countries is significant, 
# using appropriate chow tests

# Load necessary packages
library(data.table)
library(car)

# Load aggregated data as a data.table object
dt <- fread("aggregated_data.csv")

# Convert to data.table
dt <- as.data.table(dt)

# Ensure dummy variables are numeric (0/1)
D1 <- as.numeric(dt$high_resources)
D2 <- as.numeric(dt$OECD)

# Derive log variables while handling potential zero values
lgdp_per_capita_obs <- log(dt$last_gdp_per_capita)
lsaving_rate = sign(dt$avg_saving_rate) * log(abs(dt$avg_saving_rate))
lpop_growth = log(dt$avg_pop_growth + 5)


# Full model with interaction terms for high resources rents
full_model <- lm(lgdp_per_capita_obs ~ lsaving_rate + lpop_growth + D1 + lsaving_rate * D1 + lpop_growth * D1)
coef_names <- names(coef(full_model))

# Perform Chow test 
chow_test <- linearHypothesis(full_model, coef_names[grepl("D1", coef_names)])
print(chow_test)



# Full model with interaction terms for oecd 
full_model2 <- lm(lgdp_per_capita_obs ~ lsaving_rate + lpop_growth + D2 + lsaving_rate * D2 + lpop_growth * D2)
coef_names2 <- names(coef(full_model2))

# Perform Chow test 
chow_test2 <- linearHypothesis(full_model2, coef_names2[grepl("D2", coef_names2)])
print(chow_test2)



# We conclude that that is a significant discrepancy between high resource rents countries and regular countries
# and between OECD and non OECD countries, therefore we proceed with separate regressions for these three groups:
# 1) All countries
# 2) All countries apart from big natural resources exporters
# 3) Only OECD countries


