# Reliable Estimation of Unemployment Using Social Media Traces and Large Language Models

This repository contains the data and code necessary to replicate the results presented in the paper.

The folders are organized by tasks related to data preparation, prediction, and evaluation of model performance. Each folder contains the necessary code and publicly available data to replicate the results. No individual-level data is provided as part of this repository. Once access is granted, the restricted-access data is expected to be located under `./preprocessing/data/restricted/`.

The folders in this repository are as follows:

## Step 0: Preprocessing

### Code

`collect_ui_data.R` is the code used to collect and preprocess the necessary UI claims data for the analysis.

### Data

This subfolder contains the data used in the main analysis.

Publicly available data include the following:

1. UI claims data: `ICNSA.csv` (national level), `ar539.csv` (state level), and `ui_city.csv` (city level, output from `collect_ui_data.R`).

2. Population and unemployment official statistics by demographic groups: `NST-EST2021-POP.xlsx` (by state), `city_pop.xlsx` (by city), `sc-est2022-agesex-civ.csv` (by age and gender), `unemp_dist0.csv` (by state, age, gender).

3. Time series data on the rule-based and JoblessBERT unemployment index: `time_series.csv`. 

4. Precision-recall curve calculated from a tweet level evaluation sample: `figure1a_pr_eval.csv'.

5. Crosswalk tables across different geographic units: `locations_US.csv` (maps users' inferred profile locations to U.S. states and cities), `locations_US_num_state.csv`, and `srd_code_to_state.csv` (state name and 2-character state codes).

### Restricted Data

The "restricted" subfolder contains templates for the restricted-access data, with its contents left blank. Researchers interested in reproducing the results involving restricted data will need to obtain the data independently and fill in the missing observations in each file. The restricted data include the following:

1. Unemployment Tweets: `unemployed_tweets_regex.csv` (Tweets classified as unemployment disclosures using the rule-based model), `unemployed_tweets_bert.csv` (Tweets classified as unemployment disclosures using the JoblessBERT model). 

2. User profile information: `latest_profiles.csv` and `latest_profiles_user_location.csv` (profile location, indicator of whether a user has a profile picture, number of friends, number of statuses posted by each user), `demo_inf_final_20240106.csv` (users' inferred age brackets and gender (binary)). 

3. Bloomberg consensus forecasts: `ui_consensus_forecast.csv` (Downloaded from the Bloomberg Terminal).

## Step 1: Detecting Unemployment

In this section, we evaluate the ability of the language models to detect disclosures of a user's unemployment status.

JoblessBERT, a large language model fine-tuned for detecting unemployment disclosures on Twitter, is based on Conversational BERT, which was pre-trained on extensive social media text. Using active learning, we iteratively annotated tweets to maximize classification performance, ultimately creating a model with 8,838 labeled examples. Detailed methods and the open-sourced JoblessBERT model are available in the related [methods paper](https://doi.org/10.18653/v1/2022.acl-long.453) and on [Hugging Face](https://huggingface.co/worldbank/jobless-bert).

This subfolder contains the following code and data files:

1. `dist_unemp.R` - Code used to construct distributions of unemployed users relative to the population by state, age, and gender.

2. `figure1a_pr_eval.R` - Code used to plot the precision-recall curve to evaluate model performance.

3. `figure1b_dist_state.R` - Code used to plot the distribution of unemployed users relative to the population by state.

4. `figure1c_dist_age.R` - Code used to plot the distribution of unemployed users relative to the population by age.

5. `figure1d_dist_gender.R` - Code used to plot the distribution of unemployed users relative to the population by gender.

## Step 2: Constructing the Unemployment Index

In this section, we construct the unemployment index, which is defined as the post-stratified percentage of users who disclosed their employment status using either the rule-based or JoblessBERT model.

1. `clean_main_sample.R` - Code used to construct unemployment indices at national, state, and city levels. 

2. `figure2a_time_series.R` - Code used to plot the time series of actual UI claims and unemployment indices.

## Step 3: Monitoring Unemployment

In this section, we use the unemployment indices to investigate whether self-disclosures of unemployment on Twitter can help monitor UI claims.

1. `predict.R` - Code used to construct predictions of national, state, and city-level UI claims.

2. `rmse.R` - Code used to calculate the RMSE of national, state, and city-level predictions of UI claims.

3. Code to visualize predictive performance:

    - `figure2c_rmse_by_d_national.R` - Code used to plot RMSE by forecast horizon at the national level.
    - `figure3a_rmse_by_state.R` - Code used to plot RMSE across states.
    - `figure3b_rmse_by_d_state.R` - Code used to plot RMSE by forecast horizon at the state level.
    - `figure3c_rmse_by_d_city.R` - Code used to plot RMSE by forecast horizon at the city level.
    - `figure3d_rmse_by_city_change_ui.R` - Code used to plot RMSE for cities sorted by their average change in UI claims.
    - `figure3e_rmse_by_city_penetration.R` - Code used to plot RMSE for cities sorted by their Twitter penetration rate.

## Instructions for Replication

To reproduce the results in the paper:

1. Obtain the restricted data and place them in `./preprocessing/data/restricted/`.
2. Run `main.m`, which calls the other supporting code files in the subfolders of this repository.
3. The results are either figures, which are saved as .pdf files in the same folder as the underlying code, or numbers cited in the main text, which can be copied from the output printed on the R console. 

## Software Requirements

The code in this repository has been written and tested using R 4.3.2. The full session information has been listed below:
```
R version 4.3.2 (2023-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 11 x64 (build 22000)

Matrix products: default


locale:
[1] LC_COLLATE=English_United States.utf8  LC_CTYPE=English_United States.utf8    LC_MONETARY=English_United States.utf8 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.utf8    

time zone: America/New_York
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] forecast_8.22.0   ggsignif_0.6.4    ggrepel_0.9.4     speedglm_0.3-5    biglm_0.9-2.1     DBI_1.1.3         MASS_7.3-60       glmnet_4.1-8     
 [9] Matrix_1.6-1.1    plm_2.6-3         readxl_1.4.3      data.table_1.14.8 zoo_1.8-12        lubridate_1.9.3   forcats_1.0.0     stringr_1.5.0    
[17] dplyr_1.1.3       purrr_1.0.2       readr_2.1.4       tidyr_1.3.0       tibble_3.2.1      ggplot2_3.4.4     tidyverse_2.0.0  

loaded via a namespace (and not attached):
 [1] gtable_0.3.4      shape_1.4.6       collapse_2.0.3    lattice_0.21-9    tzdb_0.4.0        quadprog_1.5-8    vctrs_0.6.4       tools_4.3.2      
 [9] Rdpack_2.6        generics_0.1.3    curl_5.1.0        parallel_4.3.2    sandwich_3.0-2    fansi_1.0.5       xts_0.14.0        pkgconfig_2.0.3  
[17] lifecycle_1.0.4   compiler_4.3.2    maxLik_1.5-2      munsell_0.5.0     codetools_0.2-19  Formula_1.2-5     pillar_1.9.0      iterators_1.0.14 
[25] foreach_1.5.2     nlme_3.1-163      fracdiff_1.5-3    tidyselect_1.2.0  bdsmatrix_1.3-6   digest_0.6.33     stringi_1.7.12    tseries_0.10-56  
[33] splines_4.3.2     miscTools_0.6-28  grid_4.3.2        colorspace_2.1-0  cli_3.6.1         magrittr_2.0.3    survival_3.5-7    utf8_1.2.4       
[41] withr_2.5.2       scales_1.2.1      timechange_0.2.0  TTR_0.24.4        quantmod_0.4.26   nnet_7.3-19       timeDate_4032.109 cellranger_1.1.0 
[49] hms_1.1.3         urca_1.3-4        rbibutils_2.2.16  lmtest_0.9-40     rlang_1.1.2       Rcpp_1.0.11       glue_1.6.2        rstudioapi_0.15.0
[57] R6_2.5.1
```
