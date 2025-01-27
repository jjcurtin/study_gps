# Script to engineer features on CHTC

# Constants
dist_max <- 0.031 # tolerance threshold for how far someone can be for know location (~=50m)
window <- "day"  #window for calculating labels
roll_dur <- "hour" # "day"
labels_version <- "v2" # corresponds to lapse labels version
version <- "v3" # corresponds to version of mak_jobs script
sample <- "gps"

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(foreach)
  source("fun_features.R")
})

# location variance scoring function
score_location_variance <- function(the_subid, the_dttm_label, x_all, 
                                    period_durations, data_start, 
                                    lat, lon) {
  
  # Gets value for col_name and returns a raw variance and a variance change
  
  # the_subid: single integer subid
  # the_dttm_label: single dttm for label onset
  # x_all:  raw data for all subids and communications
  # period_durations: vector of 1+ integer period_durations in hours
  # data_start: a df with data_start = min(study_start, comm_start) for all subids
  # col_name: column name for raw data for feature as string - should be continuous var
  
  # filter down to when stationary
  data <- data |> filter(transit == "no")
  
  # define period_var function
  period_var <- function (.x) {
    if (!is.numeric(.x)) stop("Input must be a numeric vector.")
    
    #print(paste("Input:", paste(.x, collapse = ", ")))
    #print(paste("Length:", length(.x)))
    
    if (length(.x) == 1) { # if single value, AKA no change in location, variance is 0
      #print("Single value, setting variance to 0")
      the_var <- 0
    } else if (length(.x) > 0) { # calculate variance if length greater than 0
      if (!all(is.na(.x))) { 
        #print("Not all values are NA, calculating variance")
        the_var <- var(.x, na.rm = TRUE)
      } else {
        #print("All values are NA, setting variance to NA")
        the_var <- NA
      }
    } else {
      #print("Empty vector, setting variance to NA")
      the_var <- NA
    }
    
    return(the_var)
  }
  
  
  
  # get baseline variance using all data before label dttm
  baseline <- data %>% 
    get_x_period(the_subid, the_dttm_label, x_all = ., lead, period_duration = Inf) %>% # Inf gives all data back to first obs
    #summarise("base" := period_var(.data[[col_name]])) %>% # base_lat, base lon, then add together and take log
    #summarise("base" := log10(period_var(.data[[lat]]) + period_var(.data[[lon]]) + 1)) %>%
    summarise("base" := log10(period_var(lat) + period_var(lon) + 1)) %>%
    pull(base)
  
  features <- foreach (period_duration = period_durations, .combine = "cbind") %do% {
    
    raw <- data %>%
      get_x_period(the_subid, the_dttm_label, ., lead, period_duration) %>% 
      #summarise("raw" := log10(period_var(.data[[lat]]) + period_var(.data[[lon]]) + 1)) %>% # raw_lat, raw_lon, add together
      summarise("raw" := log10(period_var(lat) + period_var(lon) + 1)) %>%
      pull(raw)
    
    tibble(
      "p{period_duration}.rvar_location" := raw,
      "p{period_duration}.dvar_location" := raw - baseline) |> 
      #"{data_type_value}.p{period_duration}.pvar_{col_name}" := (raw - baseline) / baseline) %>% 
      rename_with(~str_remove_all(.x, ".NA")) %>% 
      rename_with(~str_remove(.x, "^NA."))
  }
  
  features <- features %>%
    mutate(subid = the_subid,
           dttm_label = the_dttm_label) %>%
    relocate(subid, dttm_label)
  
  return(features)
}

# get chtc process num
args <- commandArgs(trailingOnly = TRUE)
#job_start <- 2000
#job_stop <- 2100
job_start <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in row numbers
job_stop <- as.numeric(args[2])

# Read in data

data <- read_csv("gps.csv.xz", show_col_types = FALSE) |>
  # variable conversions
  mutate(time = with_tz(time, tz = "America/Chicago"),
         dist = dist / 1609.344,
         duration = duration / 60,
         speed = dist / duration,
         dist_context = dist_context / 1609.344) |>
  rename(dttm_obs = time) |>
  mutate(duration = if_else(dist > 0.01 & duration == 0, NA_real_, duration),
         duration = if_else(speed > 100, NA_real_, duration),
         duration = if_else(duration > 2 & dist > 0.31, NA_real_, duration),
         duration = if_else(duration > 24, 24, duration),
         known_loc = if_else(dist_context <= dist_max & speed <= 4, "yes", "no"),
         known_loc = if_else(is.na(known_loc), "no", known_loc),
         transit = if_else(speed <= 4, "no", "yes"),
         home = if_else(type == "home", "yes", "no"),
         evening_out = if_else((as.numeric(hour(dttm_obs)) >= 19 | as.numeric(hour(dttm_obs)) < 4)
                               & home == "no",
                               "yes", "no"))

labels <- read_csv("labels.csv",
                   show_col_types = FALSE)

if (roll_dur == "day") {
  labels <- labels |> mutate(dttm_label = with_tz(day_start, tz = "America/Chicago"),
         day_end = with_tz(day_end, tz = "America/Chicago")) |>
  slice(job_start:job_stop) |>
  mutate(label_num = seq(job_start, job_stop, by = 1))
}
if (roll_dur == "hour") {
  labels <- labels |> mutate(dttm_label = with_tz(dttm_label, tz = "America/Chicago")) |>
  slice(job_start:job_stop) |>
  mutate(label_num = seq(job_start, job_stop, by = 1))
}

dates <- read_csv("study_dates.csv", show_col_types = FALSE)  |>
  select(subid, data_start = study_start) |>
  mutate(data_start = with_tz(data_start, tz = "America/Chicago"))

# Initialize period durations and lead hours ------------------
period_durations <- c(6, 12, 24, 48, 72, 168)
lead <- 0

# Make features ------------------
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

features <- foreach (i_label = 1:nrow(labels),
                     .combine = "rbind") %do% {

                       # for (the_label_num in job_start:job_stop) {
                       label <- labels |> slice(i_label)
                       subid <- label$subid
                       dttm_label <-  label$dttm_label
                       the_label_num <- label$label_num

                       # CONTEXT ---

                       # type
                       feature_row <- score_ratesum(subid,
                                                    dttm_label,
                                                    x_all  = (data |> filter(known_loc == "yes")),
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
                                                 x_all  = (data |> filter(known_loc == "yes")),
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
                                                 x_all  = (data |> filter(known_loc == "yes")),
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
                                                 x_all  = (data |> filter(known_loc == "yes")),
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
                                                 x_all  = (data |> filter(known_loc == "yes")),
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
                                                 x_all  = (data |> filter(known_loc == "yes")),
                                                 period_durations = period_durations,
                                                 lead = lead,
                                                 data_start = dates,
                                                 col_name = "duration",
                                                 context_col_name = "avoid",
                                                 context_values = c("yes", "no")),
                                   by = c("subid", "dttm_label"))


                       # MOVEMENT ---

                       # transition time
                       # amount of time in non-stationary state
                       feature_row <- feature_row |>
                         full_join(score_ratesum(subid,
                                       dttm_label,
                                       x_all  = data,
                                       period_durations = period_durations,
                                       lead = lead,
                                       data_start = dates,
                                       col_name = "duration",
                                       context_col_name = "transit",
                                       context_values = c("yes", "no")),
                                       by = c("subid", "dttm_label"))

                       # evening
                       feature_row <- feature_row |>
                         full_join(score_ratesum(subid,
                                                 dttm_label,
                                                 x_all  = data,
                                                 period_durations = period_durations,
                                                 lead = lead,
                                                 data_start = dates,
                                                 col_name = "duration",
                                                 context_col_name = "evening_out",
                                                 context_values = c("yes", "no")),
                                   by = c("subid", "dttm_label"))

                       # location variance
                       feature_row <- feature_row |>
                         full_join(score_location_variance(subid,
                                                           dttm_label,
                                                           x_all  = data,
                                                           period_durations = period_durations,
                                                           data_start = dates,
                                                           lat = lat, lon = lon),
                                   by = c("subid", "dttm_label"))

                       feature_row <- feature_row |>
                          mutate(label_num = the_label_num)

                       feature_row
                     }

# Add outcome label and other info to features ------------------
features |>
  mutate(lapse = labels$lapse,
         label_num = 1:nrow(features)) |>
  relocate(label_num, subid, dttm_label, lapse) |>
  write_csv((str_c("features_", window,
                                       "_",
                                       roll_dur, "_hour_",
                                       version, "_",
                                       job_start, "_",
                                       job_stop, ".csv")))
