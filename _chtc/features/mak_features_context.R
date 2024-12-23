# Script to engineer features on CHTC

# Constants
dist_max <- 0.031   # only use context if places are within 50 meters (0.031 miles)
window <- "1day"  #window for calculating labels
roll_dur <- "day"

devtools::source_url("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true", 
                     sha1 = "a58e57da996d1b70bb9a5b58241325d6fd78890f")

path_gps <- format_path("studydata/risk/data_processed/gps")

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(foreach)
  source("../analysis_risk/shared/fun_features.R")
})

# get chtc process num
#args <- commandArgs(trailingOnly = TRUE) 
#job_start <- 2001
#job_stop <- 2002
#job_start <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in row numbers
#job_stop <- as.numeric(args[2])

# read in data
# CP: instead of reading in this file, I will change this so that the file is created here
# just leaving at as this for now while I am testing out code
data <- read_csv(here::here(path_gps, "gps_enriched.csv.xz"), show_col_types = FALSE) |>
  # variable conversions
  mutate(time = with_tz(time, tz = "America/Chicago"),
         dist = dist / 1609.344,
         duration = duration / 60,
         speed = dist / duration,
         dist_context = dist_context / 1609.344) |>
  rename(dttm_obs = time) |> 
  # filtering, see: eda_places
  mutate(duration = if_else(dist > 0.01 & duration == 0, NA_real_, duration),
         duration = if_else(speed > 100, NA_real_, duration),
         duration = if_else(duration > 2 & dist > 0.31, NA_real_, duration),
         duration = if_else(duration > 24, 24, duration),
         known_loc = if_else(dist_context <= dist_max & speed <= 4, TRUE, FALSE),
         known_loc = if_else(is.na(known_loc), FALSE, known_loc)) |> 
  filter(known_loc == TRUE) # currently only care about observations with context
                                   # this may change when need to figure out total duration
                                   # of observations in window in future

labels <- read_csv(here::here(path_gps, str_c("labels_", roll_dur, ".csv")), show_col_types = FALSE) |>
  # dttm_label represents label ONSET, required for score_ratesum function
  mutate(dttm_label = with_tz(day_start, tz = "America/Chicago"),
         day_end = with_tz(day_end, tz = "America/Chicago")) #|>  
  #slice(job_start:job_stop) |> 
  #mutate(label_num = seq(job_start, job_stop, by = 1))

dates <- read_csv(here::here(path_gps, "study_dates.csv"), show_col_types = FALSE)  |>  
  select(subid, data_start = study_start) |>  
  mutate(data_start = with_tz(data_start, tz = "America/Chicago"))


# initialize period durations and lead hours ------------------
period_durations <- c(6, 12, 24, 48, 72, 168) 
lead <-  0 

# make features ------------------
# CP: iterates through each subject by label. each feature_row represents
# all calculations for every period duration for that place feature and
# each possible context_value for that feature.
# row: day of observation
# column: every possible rate calculated for each context_value and duration combination
# three types of rates are returned from score_ratesum:
# a raw rate and two relative rates (these are our change rate features):
# dratecount: difference between rate in period and rate across all data
# pratecount: percent change between rate in period and rate across all data

#labels <- labels |> 
  #slice_head(n = 100)

#i_label <- 1   # for testing

cl <- parallel::makePSOCKcluster(parallel::detectCores(logical = FALSE))
doParallel::registerDoParallel(cl)

features <- foreach (i_label = 1:nrow(labels),
                     .combine = "rbind",
                     .packages = c("dplyr", "foreach", "lubridate", "stringr")) %dopar% {
  
  # for (the_label_num in job_start:job_stop) {
  label <- labels |> slice(i_label)
  subid <- label$subid 
  dttm_label <-  label$dttm_label
  #the_label_num <- label$label_num
  
  # CONTEXT ---
  
  # type
  feature_row <- score_ratesum(subid, 
                               dttm_label,
                               x_all  = data,
                               period_durations = period_durations,
                               lead = lead, 
                               data_start = dates, 
                               col_name = "duration", 
                               context_col_name = "type",
                               context_values = c("aa", "airport", "bar",
                                                  "cafe", "church",
                                                  "community space / recreation",
                                                  "errands", "family", "fitness",
                                                  "friend", "healthcare", "home",
                                                  "library", "liquorstore", "other",
                                                  "park", "public drinking space",
                                                  "restaurant", "school",
                                                  "temporary residence",
                                                  "travel stop",
                                                  "volunteer", "work"))
  
  # drank
  feature_row <- feature_row |>  
    full_join(score_ratesum(subid, 
                            dttm_label,
                            x_all  = data,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = dates, 
                            col_name = "duration",
                            context_col_name = "drank",
                            context_values = c("yes", "no")), 
              by = c("subid", "dttm_label"))
  
  # alcohol
  feature_row <- feature_row |>  
    full_join(score_ratesum(subid, 
                            dttm_label,
                            x_all  = data,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = dates, 
                            col_name = "duration",
                            context_col_name = "alcohol",
                            context_values = c("yes", "no")), 
              by = c("subid", "dttm_label"))
  
  # emotion
  feature_row <- feature_row |>  
    full_join(score_ratesum(subid, 
                            dttm_label,
                            x_all  = data,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = dates, 
                            col_name = "duration",
                            context_col_name = "emotion",
                            context_values = c("pleasant", "unpleasant", "mixed", "neutral")), 
              by = c("subid", "dttm_label"))
  
  # risk
  feature_row <- feature_row |>  
    full_join(score_ratesum(subid, 
                            dttm_label,
                            x_all  = data,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = dates, 
                            col_name = "duration",
                            context_col_name = "risk",
                            context_values = c("high", "medium", "low", "no")), 
              by = c("subid", "dttm_label"))
  
  # avoid
  feature_row <- feature_row |> 
    full_join(score_ratesum(subid, 
                            dttm_label,
                            x_all  = data,
                            period_durations = period_durations,
                            lead = lead, 
                            data_start = dates, 
                            col_name = "duration",
                            context_col_name = "avoid",
                            context_values = c("yes", "no")), 
              by = c("subid", "dttm_label"))
  
  #feature_row <- feature_row |> 
    #mutate(label_num = the_label_num)
  
  feature_row
}

# Quick EDA -----------

## type
features |> select(contains("rratesum") & contains("type")) |> skimr::skim()

features |> select(contains("dratesum") & contains("type")) |>  skimr::skim()

features |> select(contains("pratesum") & contains("type")) |> skimr::skim()

## drank
features |> select(contains("rratesum") & contains("drank")) |> skimr::skim()

features |> select(contains("dratesum") & contains("drank")) |> skimr::skim()

features |> select(contains("pratesum") & contains("drank")) |> skimr::skim()

## alcohol
features |> select(contains("rratesum") & contains("alcohol")) |> skimr::skim()

features |> select(contains("dratesum") & contains("alcohol")) |> skimr::skim()

features |> select(contains("pratesum") & contains("alcohol")) |> skimr::skim()

## emotion
features |> select(contains("rratesum") & contains("emotion")) |> skimr::skim()

features |> select(contains("dratesum") & contains("emotion")) |> skimr::skim()

features |> select(contains("pratesum") & contains("emotion")) |> skimr::skim()

## risk
features |> select(contains("rratesum") & contains("risk")) |> skimr::skim()

features |> select(contains("dratesum") & contains("risk")) |> skimr::skim()

features |> select(contains("pratesum") & contains("risk")) |> skimr::skim()

## avoid
features |> select(contains("rratesum") & contains("avoid")) |> skimr::skim()

features |> select(contains("dratesum") & contains("avoid")) |> skimr::skim()

features |> select(contains("pratesum") & contains("avoid")) |> skimr::skim()

# Add outcome label and other info to features ------------------
features |> 
  mutate(lapse = labels$lapse,
         label_num = 1:nrow(features)) |>  
  relocate(label_num, subid, dttm_label, lapse) |>
  write_csv(here::here(path_gps, str_c("features_", roll_dur, ".csv")))
