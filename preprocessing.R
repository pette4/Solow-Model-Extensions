## DATA PREPROCESSING ##
 
## INSTRUCTIONS:

## this script removes irrelevant columns to the specified datasets, 
## renames columns, adjusts datasets from wide to long format
## merges all into a single csv file and aggregates the data into a dataset
## with a singlee row per country

## to add a new variable to the dataset, just add the data.table object to the
## "datasets" list and specify in the "variables" list a name for the variable 
## to be included in the merged dataset 
## (this script assumes that data about each variables comes from a World Bank dataset)


# import libraries and load datasets 
library(data.table)
library(dplyr)
datasets = list(fread("gdp/gdp.csv"), 
                fread("pop_growth/pop_growth.csv"), 
                fread("saving/saving.csv"),
                fread("resource/resource.csv"),
                fread("educ_attained/educ_attained.csv"),
                fread("trade/trade.csv"))
variables = c("gdp_per_capita", "pop_growth", "saving_rate", "resource_rents", "educ_attained", "trade")
merged_data = datasets[[1]]

for (i in seq_along(datasets)) {
  # remove irrelevant and empty columns, rename columns using the values of the first row
  datasets[[i]] <- datasets[[i]][, -ncol(datasets[[i]]), with = FALSE]
  new_names <- as.character(unlist(datasets[[i]][1, ]))
  setnames(datasets[[i]], old = names(datasets[[i]]), new = new_names)
  datasets[[i]][, c("Indicator Name", "Indicator Code") := NULL]
  datasets[[i]] <- datasets[[i]][-1, ]
  
  # convert from wide to long format, inserting the relevant variable in a new column
  datasets[[i]] = melt(datasets[[i]], id.vars = c("Country Name", "Country Code"),
                       variable.name = "Year",
                       value.name = variables[[i]])
  
}

# merge the datasets in a single csv file
merged_data = datasets[[1]]
for (i in 2:length(datasets)) {
  merged_data = merge(merged_data, datasets[[i]]) 
}

# drop rows referring to years before 2000
merged_data[, Year := as.integer(Year) + 1959] 
merged_data = merged_data[Year > 1999]

# drop rows referring to regions instead of countries
region_list <- c("Africa Eastern and Southern", 
                 "Africa Western and Central",
                 "Arab World",
                 "Central Europe and the Baltics",
                 "Caribbean small states",
                 "East Asia & Pacific (excluding high income)",
                 "Early-demographic dividend",
                 "East Asia & Pacific",
                 "Europe & Central Asia (excluding high income)",
                 "Europe & Central Asia",
                 "Euro area",
                 "European Union",
                 "Fragile and conflict affected situations",
                 "High income",
                 "Heavily indebted poor countries (HIPC)",
                 "IBRD only",
                 "IDA & IBRD total",
                 "IDA total",
                 "IDA blend",
                 "IDA only",
                 "Not classified",
                 "Latin America & Caribbean (excluding high income)",
                 "Latin America & Caribbean",
                 "Least developed countries: UN classification",
                 "Low income",
                 "Lower middle income",
                 "Low & middle income",
                 "Late-demographic dividend",
                 "Middle East & North Africa",
                 "Middle income",
                 "Middle East & North Africa (excluding high income)",
                 "North America",
                 "OECD members",
                 "Other small states",
                 "Pre-demographic dividend",
                 "West Bank and Gaza",
                 "Pacific island small states",
                 "Post-demographic dividend",
                 "Sub-Saharan Africa (excluding high income)",
                 "Sub-Saharan Africa",
                 "Small states",
                 "East Asia & Pacific (IDA & IBRD countries)",
                 "Europe & Central Asia (IDA & IBRD countries)",
                 "Latin America & the Caribbean (IDA & IBRD countries)",
                 "Middle East & North Africa (IDA & IBRD countries)",
                 "South Asia",
                 "South Asia (IDA & IBRD)",
                 "Sub-Saharan Africa (IDA & IBRD countries)",
                 "Upper middle income",
                 "World")
merged_data = merged_data %>% filter(!`Country Name` %in% region_list)

# create a dataset with the mean of each independent variable and the last value and the mean of the
# dependent variable (gdp_per_capita)
aggregate_data <- function(df, time_series_vars) {
  df %>%
    group_by(`Country Name`) %>%
    summarise(
      across(all_of(time_series_vars), ~ last(na.omit(.x)), .names = "last_{.col}"),
      across(all_of(time_series_vars), ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}")
    )
}
aggregated_data <- as.data.table(aggregate_data(merged_data, variables))

# check and filter out countries that have no data for our key variables 
aggregated_data$`Country Name`[rowSums(is.na(aggregated_data[, c("avg_gdp_per_capita", "avg_pop_growth", "avg_saving_rate")])) > 0]
aggregated_data <- aggregated_data %>% filter(!rowSums(is.na(aggregated_data[, c("avg_gdp_per_capita", "avg_pop_growth", "avg_saving_rate")])))

# add boolean variable for whether resourcerents >= 10
aggregated_data[, high_resources := ifelse(aggregated_data$avg_resource_rents>=10.0, 1, 0)]

# add boolean variable for whether countries are in the OECD
OECD_countries_list <- c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
                    "Costa Rica", "Czechia", "Denmark", "Estonia", "Germany", "Finland", 
                    "France", "Greece", "Hungary", "Iceland", "Ireland", "Israel", "Italy", 
                    "Japan", "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands",
                    "New Zealand", "Norway", "Poland", "Portugal", "Slovenia", "Slovakia", 
                    "Spain", "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States")
aggregated_data[, OECD := ifelse(tolower(`Country Name`) %in% tolower(OECD_countries_list), 1, 0)]

# drop rows with NA values. They are:	Eritrea, Faroe Islands, Gabon, Kosovo, Libya
# we can remove them since they are small economies (OLS not biased much) and 
# since their data is likely not high quality anyways (contains measurement error)
aggregated_data <- na.omit(aggregated_data)  # Drop rows with any missing values

# remove Ukraine from the analysis: over 5% drop in population in 2023 --> leads to NAs because ln(pop_growth + 5) is not defined (argument < 0)
aggregated_data <- aggregated_data[aggregated_data$`Country Name` != "Ukraine", ]



# function to download merged dataset
download_data = function(data) {
  dataset_name = "aggregated_data.csv"
  fwrite(data, dataset_name)
}
download_data(aggregated_data)






