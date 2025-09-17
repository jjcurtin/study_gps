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
# xgboost same v5 features but with hour roll (v7) -- thought I made filtering changes here but accidentally did not
# v8 testing different stratifications with kfold, same v6 features
# xgboost movement, context, weather raw and diff, hour roll, with filtering updates and corrected some type features (v9)
# glmnet with lh strat, movement and context and weather features, hour roll (v10)


# Currently running:
# xgboost with lh strat, movement and context and weather and circadian features, hour roll (v11)

# source format_path
source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# SET GLOBAL PARAMETERS--------------------
study <- "gps"
window <- "day"
lead <- 0
version <- "v11" 
algorithm <- "xgboost"
model <- "mod_comparison"

feature_set <- c("baseline", "all") # GPS feature set name
data_trn <- str_c("features_combined.csv")

seed_splits <- 102030

ml_mode <- "classification"   # regression or classification
configs_per_job <- 10 # number of model configurations that will be fit/evaluated within each CHTC

# RESAMPLING FOR OUTCOME-----------------------------------
# note that ratio is under_ratio, which is used by downsampling as is
# It is converted to  overratio (1/ratio) for up and smote
# memory constraints with SMOTE
# daily lapse base rate ~ 7% (~ 13:1 majority to minority cases)
resample <- c("none", "up_1", "up_2", "up_3", "up_4", "up_5",
              "down_1", "down_2", "down_3", "down_4", "down_5")

# CHTC SPECIFIC CONTROLS------ ---------------------
username <- "punturieri" # for setting staging directory (until we have group staging folder)
stage_data <- FALSE # If FALSE .sif will still be staged, just not data_trn
max_idle <- 1000
request_cpus <- 1 
request_memory <- "20000MB"
request_disk <- "3000MB"
want_campus_pools <- TRUE
want_ospool <- TRUE


# OUTCOME-------------------------------------
y_col_name <- "lapse" 
y_level_pos <- "lapse" 
y_level_neg <- "no lapse"


# CV SETTINGS---------------------------------
cv_resample_type <- "kfold" # can be boot, kfold, or nested
cv_resample = "5_x_5" # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- NULL # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- NULL # outer resample will always be kfold
cv_group <- "subid" # set to NULL if not grouping
cv_strat <- TRUE # whether or not you have a stratifying variable


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
path_data <- format_path(str_c("risk/data_processed/gps")) 

# ALGORITHM-SPECIFIC HYPERPARAMETERS-----------
hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 10)) # alpha (mixture)
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 200 # length of penalty grid

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
  
format_data <- function(df, lapse_strat = NULL){
  
  df |> 
      rename(y = !!y_col_name) |> 
      # set pos class first
      mutate(y = factor(y, levels = c(!!y_level_pos, !!y_level_neg)), 
             across(where(is.character), factor)) |>
      select(-c(dttm_label))
  
}


# BUILD RECIPE------
# Script should have a single build_recipe function to be compatible with fit script. 
build_recipe <- function(d, config) {
  # d: (training) dataset from which to build recipe
  # config: single-row config-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- config$algorithm
  feature_set <- config$feature_set
  
  if (config$resample == "none") {
    resample <- config$resample
  } else {
    resample <- str_split(config$resample, "_")[[1]][1]
    ratio <- as.numeric(str_split(config$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) |> 
    step_rm(subid)
  
  if(cv_strat) {
    rec <- rec |> 
      step_rm(strat) # remove strat variable
  }

  # no need for selection for "all" feature set 
  if(feature_set == "baseline") {
    rec <- rec |> 
      step_rm(contains("recent_cm")) 
  }
  
  rec <- rec |>
    step_zv(all_predictors()) |> 
    step_impute_median(all_numeric_predictors()) |> 
    step_impute_mode(all_nominal_predictors()) 
  
 
   
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    # under_ratio = ratio.  No conversion needed
    rec <- rec |> 
      themis::step_downsample(y, under_ratio = ratio, seed = 10) 
  }
  
  if (resample == "smote") {
    # correct ratio to over_ratio
    rec <- rec |> 
      themis::step_smote(y, over_ratio = 1 / ratio, seed = 10) 
  }
  
  if (resample == "up") {
    # correct ratio to over_ratio
    rec <- rec |> 
      themis::step_upsample(y, over_ratio = 1 / ratio, seed = 10)
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet") {
    rec <- rec  |>
      step_dummy(all_nominal_predictors()) |>
      step_normalize(all_predictors())
  } 
  
  if (algorithm == "random_forest") {
    # no algorithm specific steps
  } 
  
  if (algorithm == "xgboost") {
    rec <- rec  |> 
      step_dummy(all_nominal_predictors())
  } 
  
  # final steps for all algorithms
  rec <- rec |>
    # drop columns with NA values after imputation (100% NA)
    step_rm(where(~any(is.na(.)))) |>
    step_nzv()
  
  return(rec)
}
