# Reliable Estimation of Unemployment Using Social Media Traces and Large Language Models

This repository contains the data and code necessary to replicate the results presented in the paper.

The folders are organized by tasks related to data preparation, prediction, and evaluation of model performance. Each folder contains the necessary code and publicly available data to replicate the results. No individual-level data is provided as part of this repository. Once access is granted, the restricted-access data is expected to be located under `./preprocessing/data/restricted/`.

The folders in this repository are as follows:

## Step 0: Preprocessing

### Code

`collect_ui_data.R` is the code used to collect and preprocess the necessary UI claims data for the analysis.
    - Input: Raw data downloads in `./data/ui/`. The files in this subfolder come from various sources, including the Opportunity Insights Economic Tracker and state department of labor websites.
    - Output: `./data/ui_city.csv`, a data table containing UI claims for all cities with available data.

### Data

This subfolder contains the data used in the main analysis.

Publicly available data include the following:

1. UI claims data: `ICNSA.csv` (national level), `ar539.csv` (state level), `ui_state_iso2_US.csv` (state level data derived from `ar539.csv`), and `ui_city.csv` (city level, output from running `collect_ui_data.R`).

2. Population and unemployment official statistics by demographic groups: `NST-EST2021-POP.xlsx` (population by state), `city_pop.xlsx` (population by city), `sc-est2022-agesex-civ.csv` (population by age and gender), `unemp_dist0.csv` (unemployment by state, age, gender).

3. Time series data on the rule-based and JoblessBERT unemployment index: `time_series.csv`. Total number of users by geographic unit and demographic groups: `n_users_national_week_demo_US.csv` (number of users for national level index), `n_users_state_week_demo_US.csv` (number of users for state level index), `n_users_city_week_demo_US.csv` (number of users for city level index).

4. Crosswalk tables across different geographic units: `locations_US.csv` (maps users' inferred profile locations to U.S. states and cities), `locations_US_num_state.csv`, and `srd_code_to_state.csv` (state name and 2-character state codes).

### Restricted Data

The "restricted" subfolder contains templates for the restricted-access data, with its contents left blank. When possible, we provide the unique identifier for each unit of observation. Researchers interested in reproducing the results involving restricted data will need to obtain the data independently and fill in the missing observations in each file. The restricted data include the following:

1. Unemployment Tweets: `unemployed_tweets_regex.csv` (Tweets classified as unemployment disclosures using the rule-based model), `unemployed_tweets_bert.csv` (Tweets classified as unemployment disclosures using the JoblessBERT model). We provide the user ID and tweet ID that a researcher can use to retrieve the underlying tweets from the Twitter API.

2. User profile information: `latest_profiles.csv` and `latest_profiles_user_location.csv` (profile location, indicator of whether a user has a profile picture, number of friends, number of statuses posted by each user), `demo_inf_final_20240106.csv` (users' inferred age brackets and gender (binary)). We provide the user ID that a researcher can use to retrieve the underlying profile information from the Twitter API.

3. Bloomberg consensus forecasts: `ui_consensus_forecast.csv` (Downloaded from the Bloomberg Terminal).

## Step 1: Detecting Unemployment

In this section, we evaluate the ability of the language models to detect disclosures of a user's unemployment status.

JoblessBERT, a large language model fine-tuned for detecting unemployment disclosures on Twitter, is based on Conversational BERT, which was pre-trained on extensive social media text. Using active learning, we iteratively annotated tweets to maximize classification performance, ultimately creating a model with 8,838 labeled examples. Detailed methods and the open-sourced JoblessBERT model are available in the related [methods paper](https://doi.org/10.18653/v1/2022.acl-long.453) and on [Hugging Face](https://huggingface.co/worldbank/jobless-bert).

This subfolder contains the following code and data files:

1. dist_unemp.R - Code used to construct distributions of unemployed users relative to the population by state, age, and gender.
    - Input: Unemployment tweets (`unemployed_tweets_regex.csv`, `unemployed_tweets_bert.csv`), user profile information (`latest_profiles.csv`, `latest_profiles_user_location.csv`), users' inferred age brackets and gender (`demo_inf_final_20240106.csv`), state-level UI claims data (`ui_state_iso2_US.csv`), population and unemployment official statistics by demographic groups (`NST-EST2021-POP.xlsx`, `sc-est2022-agesex-civ.csv`, `unemp_dist0.csv`).
    - Output: State, age, and gender level distributions in `figure1b_dist_state.csv`, `figure1c_dist_age.csv`, and `figure1d_dist_gender.csv`, respectively.

2. figure1a_pr_eval.R - Code used to plot the precision-recall curve to evaluate model performance.
    - Input: `figure1a_pr_eval.csv`, which contains the precision and recall for rule-based and JoblessBERT models for various levels of classification thresholds.

3. figure1b_dist_state.R - Code used to plot the distribution of unemployed users relative to the population by state.
    - Input: `figure1b_dist_state.csv`.

4. figure1c_dist_age.R - Code used to plot the distribution of unemployed users relative to the population by age.
    - Input: `figure1c_dist_age.csv`.

5. figure1d_dist_gender.R - Code used to plot the distribution of unemployed users relative to the population by gender.
    - Input: `figure1d_dist_gender.csv`.

## Step 2: Constructing the Unemployment Index

In this section, we construct the unemployment index, which is defined as the post-stratified percentage of users who disclosed their employment status using either the rule-based or JoblessBERT model.

1. clean_main_sample.R - Code used to construct unemployment indices. The code collects all predictor variables into data tables at the national, state, and city levels.
    - Input: Unemployment tweets (`unemployed_tweets_regex.csv`, `unemployed_tweets_bert.csv`), user profile information (`latest_profiles.csv`, `latest_profiles_user_location.csv`), users' inferred age brackets and gender (`demo_inf_final_20240106.csv`), UI claims data (`ICNSA.csv`, `ar539.csv`, `ui_state_iso2_US.csv`, `ui_city.csv`), population official statistics by demographic groups (`NST-EST2021-POP.xlsx`), Bloomberg consensus forecasts (`ui_consensus_forecast.csv`).
    - Output: `time_series.csv` (national level publicly available data), `data_national.csv` (national level including restricted data), `data_state.csv` (state level), and `data_city.csv` (city level).

2. figure2a_time_series.R - Code used to plot the time series of actual UI claims and unemployment indices.
    - Input: Time series data in `time_series.csv`.

## Step 3: Monitoring Unemployment

In this section, we use the unemployment indices to investigate whether self-disclosures of unemployment on Twitter can help monitor UI claims.

1. predict.R - Code used to construct predictions of national, state, and city-level UI claims.

    - Input: Main regression sample in `data_national.csv` (national level), `data_state.csv` (state level), and `data_city.csv` (city level).
    - Output: National, state, and city-level forecasts in `./predict/forecasts_national*.csv`, `./predict/forecasts_state*.csv`, and `./predict/forecasts_city*.csv`.

2. rmse.R - Code used to calculate the RMSE of national, state, and city-level predictions of UI claims.

    - Input: National, state, and city-level forecasts in `./predict/forecasts_national*.csv`, `./predict/forecasts_state*.csv`, and `./predict/forecasts_city*.csv`.
    - Output: RMSE at various levels of aggregation in `figure2c_rmse_by_d_national.csv`, `figure3a_rmse_by_state.csv`, `figure3b_rmse_by_d_state.csv`, `figure3c_rmse_by_d_city.csv`, `figure3d_rmse_by_city_change_ui.csv`, `figure3e_rmse_by_city_penetration.csv`.

3. Code to visualize predictive performance:

    - figure2c_rmse_by_d_national.R - Code used to plot RMSE by forecast horizon at the national level.
      - Input: `figure2c_rmse_by_d_national.csv`.

    - figure3a_rmse_by_state.R - Code used to plot RMSE across states.
      - Input: `figure3a_rmse_by_state.csv`.

    - figure3b_rmse_by_d_state.R - Code used to plot RMSE by forecast horizon at the state level.
      - Input: `figure3b_rmse_by_d_state.csv`.

    - figure3c_rmse_by_d_city.R - Code used to plot RMSE by forecast horizon at the city level.
      - Input: `figure3c_rmse_by_d_city.csv`.

    - figure3d_rmse_by_city_change_ui.R - Code used to plot RMSE for cities sorted by their average change in UI claims.
      - Input: `figure3d_rmse_by_city_change_ui.csv`.

    - figure3e_rmse_by_city_penetration.R - Code used to plot RMSE for cities sorted by their Twitter penetration rate.
      - Input: `figure3e_rmse_by_city_penetration.csv`.

## Instructions for Replication

To reproduce the results in the paper:

1. Obtain the restricted data and place it in `./preprocessing/data/restricted/`.
2. Run `main.m`, which calls the other supporting code files in the subfolders of this repository.

The code in this repository has been written and tested using R 4.3.2. The following R packages have been used in the analysis: `tidyverse`, `zoo`, `data.table`, `plm`, `readxl`, `forecast`, `glmnet`, `speedglm`, `ggrepel`, `ggsignif`.
