# Training controls for gps study

# NOTES------------------------------

# v2: update context feature `type` with more information for values labeled other, expand mtry hyperparameter range downwards to include 10 and 15
# v3: remove pratesum features due to high missingness
# v4: add in preliminary context features (location variance, time in transit, time spent out of the house in the evening)
# v5: recalculate preliminary context features (location variance, time in transit, time spent out of the house in the evening)

# Batches done:
# xgboost all context features (v1)
# glmnet all context features (v1)
# xgboost all context features (v2)
# xgboost raw and diff context features (v3)
# xgboost movement + context features, raw and diff (v4)
# xgboost recalculate movement and context features, raw and diff (v5)
# xgboost same v5 features but with hour roll (v6)

# Batches to do:
# xgboost with yn strat (kendra)
# xgboost with lh strat (john)
# xgboost with nlh strat (claire)
# xgboost with no strat (claire)

# source format_path
source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# SET GLOBAL PARAMETERS--------------------
study <- "gps"
window <- "day"
lead <- 0
version <- "v8" 
algorithm <- "xgboost"
model <- "no_strat" # strat_yn strat_lh strat_nlh

feature_set <- c("context_movement") # GPS feature set name
data_trn <- str_c("features_gps_day_1h.csv")

seed_splits <- 102030

ml_mode <- "classification"   # regression or classification
configs_per_job <- 25 # number of model configurations that will be fit/evaluated within each CHTC

# RESAMPLING FOR OUTCOME-----------------------------------
# note that ratio is under_ratio, which is used by downsampling as is
# It is converted to  overratio (1/ratio) for up and smote
# memory constraints with SMOTE
# daily lapse base rate ~ 7% (~ 13:1 majority to minority cases)
resample <- c("none", "up_1", "up_2", "up_3", "up_4", "up_5",
              "down_1", "down_2", "down_3", "down_4", "down_5")

# CHTC SPECIFIC CONTROLS------ ---------------------
username <- "punturieri" # for setting staging directory (until we have group staging folder)
stage_data <- TRUE # If FALSE .sif will still be staged, just not data_trn
max_idle <- 1000
request_cpus <- 1 
request_memory <- "20000MB"
request_disk <- "2000MB"
want_campus_pools <- TRUE
want_ospool <- TRUE


# OUTCOME-------------------------------------
y_col_name <- "lapse" 
y_level_pos <- "lapse" 
y_level_neg <- "no lapse"


# CV SETTINGS---------------------------------
cv_resample_type <- "kfold" # can be boot, kfold, or nested
cv_resample = "3_x_10" # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- NULL # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- NULL # outer resample will always be kfold
cv_group <- "subid" # set to NULL if not grouping
cv_strat <- NULL # using variable names saved as model (can also pass in string or set to NULL)
cv_strat_file_name <- "lapse_strat.csv" # This file is in the shared path_data and contains all EMA subids
# we left join strat variables so all studies with smaller samples can still use it


cv_name <- if_else(cv_resample_type == "nested",
                   str_c(cv_resample_type, "_", cv_inner_resample, "_",
                         cv_outer_resample),
                   str_c(cv_resample_type, "_", cv_resample))

# STUDY PATHS----------------------------
# the name of the batch of jobs to set folder name
name_batch <- str_c("train_", algorithm, "_", cv_name, "_", version, "_", model) 
# the path to the batch of jobs to put the folder name
path_batch <- format_path(str_c("risk/chtc/", study, "/", name_batch)) 
# location of data set
path_data <- format_path(str_c("risk/data_processed/shared")) 

# ALGORITHM-SPECIFIC HYPERPARAMETERS-----------
#hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 10)) # alpha (mixture)
#hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
#hp2_glmnet_max <- 2 # max for penalty grid
#hp2_glmnet_out <- 200 # length of penalty grid

#hp1_knn <- seq(5, 255, length.out = 26) # neighbors (must be integer)

#hp1_rf <- c(2, 10, 20, 30, 40) # mtry (p/3 for reg or square root of p for class)
#hp2_rf <- c(2, 15, 30) # min_n
#hp3_rf <- 1500 # trees (10 x's number of predictors)

hp1_xgboost <- c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, .4)  # learn_rate, how fast model fits residual error; high: faster, but may overshoot, low: slower, may get stuck on less optimal solutions
hp2_xgboost <- c(1, 2, 3, 4) # tree_depth, complexity of tree structure (larger no. = more likely to overfit)
hp3_xgboost <- c(10, 15, 20, 30, 40, 50)  # mtry, no. feats. to split on at each split
# trees = 500
# early stopping = 20

#hp1_rda <- seq(0.1, 1, length.out = 10)  # frac_common_cov: Fraction of the Common Covariance Matrix (0-1; 1 = LDA, 0 = QDA)
#hp2_rda <- seq(0.1, 1, length.out = 10) # frac_identity: Fraction of the Identity Matrix (0-1)
 
#hp1_nnet <- seq(10, 50, length.out = 5)  # epochs
#hp2_nnet <- seq(0, 0.1, length.out = 15) # penalty
#hp3_nnet <- seq(5, 30, length.out = 5) # hidden units

# FORMAT DATA-----------------------------------------
# load in data set which constraint strat conditions
  
format_data <- function (df, lapse_strat = NULL){
  
  if(!is.null(lapse_strat)) {
    df <- df |> 
      rename(y = !!y_col_name) |> 
      # set pos class first
      mutate(y = factor(y, levels = c(!!y_level_pos, !!y_level_neg)), 
             across(where(is.character), factor)) |>
      select(-c(dttm_label)) |> 
      left_join(lapse_strat |> 
                  select(subid, all_of(cv_strat)), by = "subid")
  }
  
  if(is.null(lapse_strat)) {
    df <- df |> 
      rename(y = !!y_col_name) |> 
      # set pos class first
      mutate(y = factor(y, levels = c(!!y_level_pos, !!y_level_neg)), 
             across(where(is.character), factor)) |>
      select(-c(dttm_label)) 
  }
  
  return(df)
}


# BUILD RECIPE---------------------------------------
# Script should have a single build_recipe function to be compatible with fit script. 
build_recipe <- function(d, config) {
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- config$algorithm
  
  if (config$resample == "none") {
    resample <- config$resample
  } else {
    resample <- str_split(config$resample, "_")[[1]][1]
    ratio <- as.numeric(str_split(config$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  if(!is.null(lapse_strat)) {
    rec <- recipe(y ~ ., data = d) |>
      step_rm(subid, label_num, matches(cv_strat)) # needed to retain until now for grouped CV in splits
    
  }
  
  if(is.null(lapse_strat)) {
    rec <- recipe(y ~ ., data = d) |>
      step_rm(subid, label_num)
  }
  
  rec <- rec |> 
    step_impute_median(all_numeric_predictors()) |> 
    step_impute_mode(all_nominal_predictors()) |> 
    step_dummy(all_factor_predictors()) |> 
    step_select(where(~ !any(is.na(.)))) |>
    step_nzv(all_predictors())
  
  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    rec <- rec |> 
      # ratio is equivalent to tidymodels under_ratio
      themis::step_downsample(y, under_ratio = ratio, seed = 10) 
  }
  
  
  if (resample == "smote") {
    ratio <- 1 / ratio # correct ratio to over_ratio
    rec <- rec |> 
      themis::step_smote(y, over_ratio = ratio, seed = 10) 
  }
  
  if (resample == "up") {
    ratio <- 1 / ratio # correct ratio to over_ratio
    rec <- rec |> 
      themis::step_upsample(y, over_ratio = ratio, seed = 10)
  }
  
  return(rec)
}
