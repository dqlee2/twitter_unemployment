################################################################################
## Main code file
################################################################################

rm(list=ls())

# Install packages not yet installed
packages = c(
  "tidyverse","zoo","data.table","readxl","plm","glmnet","speedglm","ggrepel","ggsignif","forecast"
)
installed_packages = packages %in% rownames(installed.packages())
if (any(installed_packages==FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
sessionInfo()

#####################################################

## Step 0 Preprocessing

setwd('./preprocessing/')

# Clean UI claims data
source("clean_ui_data.R")

setwd('../')

## Step 1 Detecting Unemployment

setwd('./step_1_detecting_unemployment/')

# Construct distribution of unemployed users relative to population
source("dist_unemp.R")

# Plot the precision-recall curve
source("figure1a_pr_eval.R")

# Plot distribution of unemployed users relative to population by state
source("figure1b_dist_state.R")

# Plot distribution of unemployed users relative to population by age
source("figure1c_dist_age.R")

# Plot distribution of unemployed users relative to population by gender
source("figure1d_dist_gender.R")

setwd('../')

## Step 2 Construct Unemployment Index

setwd('./step_2_construct_unemployment_index/')

# Construct unemployment indices
source("clean_main_sample.R")

# Plot time series of actual UI claims and unemployment indices
source("figure2a_time_series.R")

setwd('../')

## Step 3 Monitoring Unemployment 

setwd('./step_3_monitoring_unemployment/')

# Predict national, state, city level UI claims
source("predict.R")

# Calculate RMSE of predictions
source("rmse.R")

# Plot RMSE by horizon d national level
source("figure2c_rmse_by_d_national.R")

# Plot RMSE by state
source("figure3a_rmse_by_state.R")

# Plot RMSE by horizon d state level
source("figure3b_rmse_by_d_state.R")

# Plot RMSE by horizon d city level
source("figure3c_rmse_by_d_city.R")

# Plot RMSE by city
source("figure3d_rmse_by_city_change_ui.R") # by avg change in UI claims
source("figure3e_rmse_by_city_penetration.R") # by penetration rate

setwd('../')



