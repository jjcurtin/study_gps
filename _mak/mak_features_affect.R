# Script to engineer features on CHTC

# Constants
dist_max <- 0.031
window <- "1day"  #window for calculating labels

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
# CP: instead of reaading in this file, I will change this so that the file is created here
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
         known_loc = if_else(is.na(known_loc), FALSE, known_loc),  # don't filter based on known loc
         transit = if_else(speed <= 4, FALSE, TRUE),
         home = if_else(type == "home", TRUE, FALSE),
         evening_out = if_else((as.numeric(hour(dttm_obs)) >= 19 | as.numeric(hour(dttm_obs)) < 4)
                           & home == FALSE,
                           TRUE, FALSE))

labels <- read_csv(here::here(path_gps, "labels.csv"), show_col_types = FALSE) |>
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


# feature function test
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
  data <- data |> filter(transit == FALSE)
  
  # define period_var function
  period_var <- function (.x) {
    if (length(.x) > 0) { 
      if (!all(is.na(.x))) {
        the_var <- var(.x, na.rm = TRUE)
      } else the_var <- NA
    } else the_var <- NA
    
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




#labels <- labels |> 
#  slice_head(n = 100)

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
                       
                       # home stay built into context features
                       
                       # transition time
                       # amount of time in non-stationary state
                       # change to 4mph speed filter
                       feature_row <- score_ratesum(subid, 
                                                    dttm_label,
                                                    x_all  = data,
                                                    period_durations = period_durations,
                                                    lead = lead, 
                                                    data_start = dates, 
                                                    col_name = "duration", 
                                                    context_col_name = "transit",
                                                    context_values = c(TRUE, FALSE))
                       
                       # evening
                       feature_row <- feature_row |>
                         full_join(score_ratesum(subid, 
                                                 dttm_label,
                                                 x_all  = data,
                                                 period_durations = period_durations,
                                                 lead = lead, 
                                                 data_start = dates, 
                                                 col_name = "duration", 
                                                 context_col_name = "evening",
                                                 context_values = c(TRUE, FALSE)),
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
           
                       
                       
                       # n clusters -- could use number of contexts collected
                       
                       # total distance
                       
                       # normalized entropy
                       
                       # circadian rhythm
                       
                       feature_row
                     }

# Quick EDA -----------

## transit
features |> select(contains("rratesum") & contains("transit")) |> skimr::skim()

features |> select(contains("dratesum") & contains("transit")) |>  skimr::skim()

features |> select(contains("pratesum") & contains("transit")) |> skimr::skim()

## evening
features |> select(contains("rratesum") & contains("evening")) |> skimr::skim()

features |> select(contains("dratesum") & contains("evening")) |>  skimr::skim()

features |> select(contains("pratesum") & contains("evening")) |> skimr::skim()

## location variance
features |> select(contains("rvar") & contains("location")) |> skimr::skim()

features |> select(contains("dvar") & contains("location")) |>  skimr::skim()

# Add outcome label and other info to features ------------------
features |>
  mutate(lapse = labels$lapse,
         label_num = 1:nrow(features)) |>  
  relocate(label_num, subid, dttm_label, lapse) |>
  write_csv(here::here(path_gps, "features_affect.csv"))

# load in to create new version of features_affect

d1 <- read_csv(here::here(path_gps, "features_affect.csv"))
d2 <- read_csv(here::here(path_gps, "features.csv"))
d3 <- left_join(d1, d2, by = c("subid", "dttm_label", "lapse", "label_num"))

d3 |>
  write_csv(here::here(path_gps, "features_affect_context.csv"))