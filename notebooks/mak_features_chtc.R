# Script to engineer features on CHTC

# Constants
dist_max <- 50   # only use context if places are within 50 meters
window <- "1day"  #window for calculating labels

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(vroom)
  library(foreach)
  source("fun_features.R")
})

# get chtc process num
args <- commandArgs(trailingOnly = TRUE) 
#job_start <- 2001
#job_stop <- 2002
job_start <- as.numeric(args[1]) # CHTC arg starts at 1 because using passed in row numbers
job_stop <- as.numeric(args[2])

# read in data
data <- vroom("data.csv.xz", show_col_types = FALSE)%>% 
  mutate(time = with_tz(time, tz = "America/Chicago"),
         duration = duration / 60) %>% #convert minutes to hours because period duration units are hours
  rename(dttm_obs = time) %>% 
  filter(dist_context <= dist_max) # currently only care about observations with context
                                   # this may change when need to figure out total duration
                                   # of observations in window in future

labels <- vroom("labels.csv", show_col_types = FALSE) %>% 
  mutate(dttm_label = with_tz(dttm_label, tz = "America/Chicago")) %>% 
  slice(job_start:job_stop) %>% 
  mutate(label_num = seq(job_start, job_stop, by = 1))

dates <- vroom("study_dates.csv", show_col_types = FALSE) %>% 
  select(subid, data_start = study_start) %>% 
  mutate(data_start = with_tz(data_start, tz = "America/Chicago"))


# initialize period durations and lead hours ------------------
period_durations <- c(6, 12, 24, 48, 72, 168) 
lead <-  0 

# make features ------------------
# i_label <- 1   # for testing
features <- foreach (i_label = 1:nrow(labels), .combine = "rbind") %do% {
  
  # for (the_label_num in job_start:job_stop) {
  label <- labels %>% slice(i_label)
  subid <- label$subid 
  dttm_label <-  label$dttm_label
  the_label_num <- label$label_num
  
  # type
  feature_row <- score_ratesum(subid, 
                               dttm_label,
                               x_all  = data,
                               period_durations = period_durations,
                               lead = lead, 
                               data_start = dates, 
                               col_name = "duration", 
                               context_col_name = "type",
                               context_values = c("aa", "bar", "cafe", "church", "family",
                                                  "fitness", "healthcare", "home",
                                                  "liquorstore", "park", "restaurant",
                                                  "school", "volunteer", "work"))
  
  # drank
  feature_row <- feature_row %>% 
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
  feature_row <- feature_row %>% 
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
  feature_row <- feature_row %>% 
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
  feature_row <- feature_row %>% 
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
  feature_row <- feature_row %>% 
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
  
  feature_row <- feature_row %>% 
    mutate(label_num = the_label_num)
  
  feature_row
}

# Add outcome label and other info to features ------------------
features %>%
  mutate(lapse = labels$lapse) %>% 
  relocate(label_num, subid, dttm_label, lapse) %>% 
  vroom_write(str_c("features_", window, "_", job_start, "_", job_stop, ".csv"), delim = ",")